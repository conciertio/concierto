(ns concierto.core
  (:require [clojure.string :as str]
            [babashka.cli :as cli]
            [babashka.process :refer [shell]]
            [babashka.classpath :refer [add-classpath]]
            [babashka.fs :as fs]
            [clojure.pprint :as pprint]
            [selmer.parser :as sel]
            [selmer.filters :as filt]
            [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [medley.core :as m]
            [cheshire.core :as json]))

(def OR-SEP :also)
(def CONF-FILE "concierto.conf")
(def TARGET-DIR "targets")
(def SCENARIO-DIR "scenarios")
(def SERVICE-DIR "services")
(def EXTENSION-DIR "extensions")
(def WORKING_DIR ".concierto")

(defn hurl
  ([msg val]
   (throw (ex-info msg val)))
  ([msg]
   (throw (ex-info msg {}))))

(defn warn [& msg]
  (apply println "WARN" msg))

(defn some-option? [args opt]
  (some? (get-in args [:opts opt])))

(defn get-option
  ([args opt]
   (get-in args [:opts opt]))
  ([args opt not-found]
   (get-in args [:opts opt] not-found)))

(defn get-opt-select [args]
  (get-in args [:opts :select]))

(defn pretty [val]
  (binding [pprint/*print-right-margin* 140]
    (clojure.pprint/pprint
     val
     (io/writer *out*))))

(defn namespace-exists?
  "check that a namespace has been loaded"
  [sym]
  (-> sym
      find-ns
      nil?
      not))

(defn get-envvar
  "Get an environment variable or return nil"
  [var]
  (let [v (System/getenv var)]
    (if (= "" v)
      nil
      v)))

(defn get-uuid []
  (.toString (java.util.UUID/randomUUID)))

(defn string-seq
  "Break a string on \\n and return as sequence"
  [str]
  (line-seq (io/reader (char-array str))))

(defn path
  "Create a path from a series of items"
  [& args]
  (str (apply fs/path args)))

(defn init-app-dir []
  (let [app-dir (or (get-envvar "CONCIERTO_APP")
                    (.toString (fs/cwd)))]
    (add-classpath app-dir)
    app-dir))

(defn get-target-from-env []
  (get-envvar "CONCIERTO_TARGET"))

(defn get-scenario-from-env []
  (get-envvar "CONCIERTO_SCENARIO"))

(def ENV (atom {:target nil
                :scenario nil
                :app-dir (init-app-dir)
                :dry-run false
                :verbose false
                :engine false}))

(defn app-dir []
  (get @ENV :app-dir))

(defn set-app-dir [dir]
  (swap! ENV assoc :app-dir dir))

(defn working-dir [] (path (app-dir) WORKING_DIR))

(defn set-verbose [verbose]
  (swap! ENV assoc :verbose verbose))

(defn verbose? []
  (:verbose @ENV))

(defn verbose-str
  ([verbose-opt]
   (if (verbose?) (str " " verbose-opt " ") ""))
  ([]
   (verbose-str "-v")))

(defn set-dry-run [dry-run]
  (when (some? dry-run)
    (set-verbose true)
    (swap! ENV assoc :dry-run true)))

(defn dry-run? []
  (:dry-run @ENV))

(def RUN-ID (get-uuid))

(def RUN-DIR
  (path (working-dir) "tmp" RUN-ID))

(defn create-run-dir
  "Create the local working directory for this run."
  []
  (when-not (fs/exists? RUN-DIR)
    (fs/create-dirs RUN-DIR)))

(defn remove-run-dir
  "Remove the temporary working directory"
  []
  (fs/delete-tree RUN-DIR))

(defn get-format [args]
  (cond
    (some? (get-option args :json)) :json
    (some? (get-option args :list)) :list
    :else :edn))

(defn format-out [args data]
  (case (get-format args)
    :list (doseq [t data]
            (cond
              (map? t) (println
                        (str/join ","
                                  (map #(name %1) (vals t))))
              :else (println t)))
    :json (println (json/encode data))
    (pretty data)))

(defn- get-configuration
  "Get global configuration on no args."
  []
  (let [conf-file (path (app-dir) CONF-FILE)]
    (if (fs/exists? conf-file)
      (read-string (slurp conf-file))
      {})))

(def get-conf (memoize get-configuration))

(defn- get-target-config
  [target]
  (let [conf-file (path (app-dir) TARGET-DIR target CONF-FILE)]
    (when (fs/exists? conf-file)
      (read-string (slurp conf-file)))))

(defn vprint [& args]
  (when (verbose?)
    (apply println args)))

(defn service-dir [service]
  (path (app-dir) SERVICE-DIR (name service)))

(defn create-temp-dir []
  (path
   (fs/create-temp-dir {:dir
                        RUN-DIR})))

(defn create-temp-file []
  (path (fs/absolutize (fs/create-temp-file
                        {:dir RUN-DIR}))))

(defn set-globals [glbs]
  (when glbs
    (swap! ENV assoc :glbs glbs)))

(defn list-directory [dir]
  (map #(fs/file-name %1)
       (fs/list-dir (path (app-dir) dir) fs/directory?)))

(defn- get-partials
  "Given a path create it's partials, 
   i.e. my/test/topic -> my, my/test , my/test/topic."
  [path]
  (->> (str/split path #"/")
       (map-indexed (fn [i v] [i v]))
       (reduce (fn [acc [i v]]
                 (conj acc
                       (conj (into [] (flatten (drop (dec i) acc))) v)))
               [])
       reverse))

(defn get-code [start-path file-name funs]
  (->>
   (get-partials start-path)
   (reduce (fn [acc next-ns]
             (let [d (apply path (app-dir) next-ns)
                   appf (path d  (str file-name ".clj"))]
               (if (fs/exists? appf)
                 (let [nns (symbol
                            (str
                             (str/join "." next-ns)
                             "." file-name))]
                   (load-file appf)
                   (if (namespace-exists? nns)
                     (merge acc
                            (-> nns
                                (ns-publics)
                                (select-keys
                                 funs)))
                     acc))
                 acc)))
           {})))

(defn load-events [start-path]
  (get-code start-path "hooks" ['attr-cluster 'attr-machine 'on-deploy]))

(defn have-event? [sym]
  (contains? (:events @ENV) sym))

(defn run-event
  "If we have an event to run, run it, and return the map it produces, 
   otherwise return an empty map"
  [event & args]
  (when (have-event? event)
    (apply (get (:events @ENV) event) args)))

(def event (memoize run-event))


(defn extensions-dir []
  (path (app-dir) EXTENSION-DIR))

; ******* Targets

(defn get-target []
  (let [target (:target @ENV)]
    (when-not (some? target)
      (hurl (str "Specify target as --target, in " CONF-FILE " file or CONCIERTO_TARGET env var")))
    target))

(defn- targets-dir []
  (path (app-dir) TARGET-DIR))

(defn- target-dir []
  (path (app-dir)
        TARGET-DIR
        (get-target)))

(defn target-exists? [target]
  (fs/directory? (path (app-dir) TARGET-DIR target)))

(defn list-targets []
  (list-directory TARGET-DIR))

(defn set-target [target]
  (when-let [target (or target
                        (get-target-from-env)
                        (:target (get-conf)))]
    (if (target-exists? target)
      (do
        (swap! ENV assoc
               :target target
               :events (load-events (path
                                     TARGET-DIR target)))
        (when-some [conf (get-target-config target)]
          (swap! ENV m/assoc-some
                 :engine (:engine conf)
                 :remote-exec (:remote-exec conf))))
      (hurl (format "Target %s does not exist in %s" target (targets-dir))))))

; ******* Scenarios

(defn scenarios-dir []
  (path (app-dir) SCENARIO-DIR))

(defn scenario-exists? [scenario]
  (fs/directory? (path (scenarios-dir) scenario)))

(defn get-scenario []
  (when-not (fs/exists? (scenarios-dir))
    (hurl "A Concierto app must have a scenarios directory!"))

  (let [scenario (:scenario @ENV)]
    (when-not scenario
      (hurl (str "Specify scenario as --scenario, in " CONF-FILE " file or CONCIERTO_SCENARIO env var")))
    scenario))

(defn list-scenarios []
  (list-directory SCENARIO-DIR))

(defn scenario-dir []
  (path (app-dir)
        SCENARIO-DIR
        (get-scenario)))

(defn set-scenario [scenario]
  (when-let [scenario (or scenario
                          (get-scenario-from-env)
                          (:scenario (get-conf)))]
    (if (scenario-exists? scenario)
      (swap! ENV assoc
             :scenario scenario)
      (hurl (str "Scenario " scenario " does not exist in " (scenarios-dir))))))

(defn services-dir []
  (path (app-dir) SERVICE-DIR))

(defn list-services []
  (list-directory SERVICE-DIR))

(defn file-exists? [file]
  (fs/exists? (path (app-dir) file)))

(defn get-file
  "Get a file from the app-dir"
  [file]
  (slurp (path (app-dir) file)))

(defn set-container-engine [engine]
  (swap! ENV assoc :engine (or
                            engine
                            (:engine (get-conf))
                            "docker"))
  (when-not (some #{(:engine @ENV)} ["docker" "podman"])
    (hurl (format "ERR: Don't know container engine %s!" (:engine @ENV)))))

(defn engine []
  (:engine @ENV))

(defn engine-compose []
  (case (engine)
    "docker" "docker-compose"
    "podman" "podman-compose"))

(defn write-data [file data]
  (clojure.pprint/pprint
   data
   (io/writer file)))

; Variables

(defn- load-clj-resource [path]
  (let [target-file (str path ".clj")]
    (when (fs/exists? target-file)
      (let [ns (str "targets." (get-target) ".machines")]
        (load-file target-file)
        ((resolve (symbol ns "source")))))))

(defn- load-file-resource [mypath]
  (let [f (str mypath ".edn")]
    (if (fs/exists? f)
      (read-string
       (slurp f))
      (if (fs/exists? mypath)
        (read-string (:out (shell {:out :string} mypath)))
        (vprint "WARN:" (fs/file-name mypath) "edn/clj/naked does not exist")))))

(defn- load-res [name]
  (let [vals (load-clj-resource name)]
    (if vals
      vals
      (load-file-resource name))))

(def load-resource (memoize load-res))

(defn load-machines []
  (load-resource (path (target-dir) "machines")))

(defn load-access []
  (load-resource (path (target-dir) "access")))

(defn load-services []
  (load-resource (path (target-dir) "services")))

(defn load-extra [name]
  (load-resource (path (target-dir) name)))


(def machines load-machines)

; MACHINE SELECTION

(defn- process-split
  "Look for OR-SEP in the seq of tokens partition into pairs and drop
   the OR-SEP"
  [incoming]
  (let [keyworded (map #(keyword %1) incoming)]
    (if (some #{OR-SEP} (map #(keyword %1) keyworded))
      (->> keyworded
           (partition-by #{OR-SEP})
           (filter #(not= (first %1) OR-SEP)))
      (list incoming))))

(defn- select-by-attr
  "Create filter transducers, one for each pair of selection criteria
   e.g. :role :api.
   This must work hand in hand with select-machines, as it's select-machines 
   which provides the multiple pairs."
  [selector val]
  (let [val (name val)]
    (filter (fn [all-attrs]
              (let [v (get all-attrs (keyword selector))]
                (cond
                  (vector? v) (some #{(keyword val)} v)
                  :else
                  (= (name v)  val)))))))

(defn select-machines
  "Split the selection criteria by OR-SEP, and for all pairs within an OR-SEP
   create filter a transducer for each pair of key/val, when all the required 
   tranducers have beencreated per OR-SEP transduce machine and accumulate it.

   If :role is a vector replace the vector with the current selected-role in 
   the clause
   
   Can call machines frequently as it's memoized."
  [& list-attrs]
  (->> (process-split list-attrs)
       (reduce
        (fn [acc selection]
          (let [pair (partition 2 selection)
                filters (apply comp
                               (map (fn [[selector val]]
                                      (select-by-attr selector val)) pair))
                selected-role (->> (filter #(= :role (first %1)) pair)
                                   first
                                   second)
                filtered (if (some? selected-role)
                           (map #(assoc %1 :role selected-role)
                                (into [] filters (machines)))
                           (into [] filters (machines)))]

            (conj acc filtered))) [])))

(defn load-globals
  "Load global vars from file, if there is an override from stdin
   then merge those vars"
  []
  (let [glbs (load-resource (path (target-dir) "globals"))]
    (if (some? (get @ENV :glbs))
      (merge glbs (get @ENV :glbs))
      glbs)))

(defn filter-machines [args]
  (if-let [machines (get-opt-select args)]
    (apply select-machines machines)
    (select-machines)))

(defn- cluster-hook
  "Run the attr-cluster hook if it exists and gather the data per
   cluster"
  [machines]
  (reduce (fn [acc cluster]
            (assoc acc (keyword cluster)
                   (event 'attr-cluster cluster)))
          {}
          (keys (group-by :cluster
                          (flatten machines)))))

(defn- machines-hook
  "Run the attr-machine per machine and merge it with the 
   existing machine data."
  [machines cluster-data]
  (->> machines
       (map (fn [s]
              (map (fn [machine]
                     (let [cluster (:cluster machine)]
                       (merge machine
                              (event 'attr-machine machine
                                     (get cluster-data cluster))))) s)))))

(defn- attr
  "Gather all the data."
  [args]
  (let [machines (filter-machines args)
        cluster-data (cluster-hook machines)
        base  {:services (load-services)
               :globals (load-globals)
               :machines (machines-hook machines cluster-data)
               :clusters cluster-data
               :access (load-access)
               :target (get-target)}]
    (if-let [resources (get-option args :resource)]
      (reduce (fn [acc resource]
                (vprint "Loading resource " resource)
                (assoc acc resource (load-extra resource)))
              base resources)
      base)))

(def gather (memoize attr))

(defn get-role-from-args [args]
  (or
   (get-option args :role)
   (let [r (seq (->>
                 (partition 2 (get-in args [:opts :select]))
                 (filter (fn [[k _v]]
                           (= (name k) "role")))))]
     (when (some? r)
       (second (first r))))))

(defn- wait
  "Add a delay"
  [args]
  (when (some-option? args :wait)
    (let [secs (get-option args :wait 1)]
      (Thread/sleep (* secs 1000)))))

(defn with-machines
  "Iterate through the machine selection.
   Process by OR-SEP grouping, so that each has it's own pmap; then
   one can wait for the tier to come up (allowing servers to start) 
   before proceeding with next tier."
  ([args fun]
   (with-machines args fun (fn [_a _m] {})))
  ([args fun init-fun]
   (let [attr (gather args)
         mach (:machines attr)
         init (init-fun args mach)]
     (->> mach
          (map (fn [group]
                 (let [res
                       (doall (pmap
                               (fn [m]
                                 (fun args m init)) group))]
                   (wait args)
                   res)))
          flatten
          (map print)
          doall))))


(defn get-registry []
  (get-in (load-access) [:docker :registry :url]))

(defn filter-add-repo [image]
  (let [url (get-registry)]
    (if (some? url)
      (str url "/" image)
      image)))

(defn template-file [args file]
  (when-not (fs/exists? file)
    (hurl (str file " does not exist")))
  (filt/add-filter! :registry filter-add-repo)
  (let [env (gather args)]
    (sel/render (slurp file) env)))

(defn get-ssh-from-ip [ip]
  (->>
   (select-machines :ip ip)
   (map :ssh)
   (filter #(some? %1))
   (first)))

(defn ssh-info [ip]
  (let [host-ssh (get-ssh-from-ip ip)
        global-ssh (:ssh (load-access))]
    (merge global-ssh host-ssh)))

(defn ssh-str [ip cmd]
  (let [merged (ssh-info ip)]
    (str "ssh "
         (:user merged) "@"
         ip " -p "
         (:port merged) " "
         cmd)))

(defn ssh-raw [ip cmd]
  (if (dry-run?)
    (do
      (vprint (ssh-str ip cmd))
      "")
    (:out (shell {:out :string
                  :extra-env (System/getenv)} (ssh-str ip cmd)))))

(defn cshell
  ([cmd]
   (cshell {} cmd))
  ([opts cmd]
   (if (dry-run?)
     (do
       (vprint cmd)
       "")
     (:out (shell (merge opts {:out :string}) cmd)))))

(defn get-raw-role [role]
  (slurp (path (scenario-dir) (str (name role) ".yml"))))

(defn role-deps
  "For a given role find all the dependent services as a sequence."
  [role]
  (->> (get-raw-role role)
       (yaml/parse-string)
       :services
       vals
       (map :depends_on)
       flatten
       (filter some?)
       sort
       distinct))

(defn role-services [role]
  (->> (get-raw-role role)
       (yaml/parse-string)
       :services
       keys))

(defn- print-config []
  (println
   (format "Scenario: %s, Target: %s, Engine: %s"
           (get-scenario)
           (get-target)
           (engine))))

(defn template
  "Return a temporary file which has been templated from the incoming."
  [file-to-template env]
  ; remember the env already contains :dir and :extra-env 
  (let [tmp (create-temp-file)]
    (when (fs/exists? file-to-template)
      (spit tmp (sel/render (slurp file-to-template) env))
      (fs/set-posix-file-permissions tmp "rwx------")
      (vprint "Templating " file-to-template "...")
      tmp)))

(defn exec-templated-file
  "Execute the provided file but first substitute all templating variables.
   Works with all types of file."
  [args]
  (let [app-dir (app-dir)
        tmpl-file (path app-dir (get-option args :script))]
    (if (fs/exists? tmpl-file)
      (let [tmp (create-temp-file)
            env {:dir app-dir
                 :extra-env {:CONCIERTO_APP app-dir
                             :CONCIERTO_TARGET (get-target)
                             :CONCIERTO_SCENARIO (get-scenario)}}]
        (spit tmp (sel/render (slurp tmpl-file) (gather args)))
        (fs/set-posix-file-permissions tmp "rwx------")
        (vprint "Executing" tmpl-file "...")
        (shell env tmp))
      (println "File " tmpl-file "does not exist"))))


(defn lint [args]
  (when-not (fs/exists? (scenarios-dir))
    (warn "Scenarios dir does not exist"))

  (when-not (fs/exists? (services-dir))
    (warn "Services dir does not exist"))

  (when-not (fs/exists? (extensions-dir))
    (warn "Extensions dir does not exist"))

  (when-not (fs/exists? (targets-dir))
    (warn "Targets dir does not exist"))

  (let [attr (gather args)]
    (when-not (get-in attr [:access :docker])
      (warn "access has no :docker key"))

    (when-not (get-in attr [:access :docker :registry])
      (warn "Cannot deploy without an :access :docker :registry"))

    (when-not (get-in attr [:access :ssh])
      (warn "Cannot deploy without an :access :ssh"))

    (when (= (count (get attr :machines)) 0)
      (warn "Have no machines"))))

(defn init [dtable]

  (-> (Runtime/getRuntime)
      (.addShutdownHook
       (Thread. (fn []
                  (remove-run-dir)))))

  (let [opts (cli/parse-opts *command-line-args*
                             {:aliases {:v :verbose}
                              :coerce
                              {:verbose :boolean}})]

    (try
      (create-run-dir)
      (set-verbose (:verbose opts))
      (set-target (:target opts))
      (set-scenario (:scenario opts))
      (set-dry-run (:dry-run opts))
      (set-container-engine (:engine opts))

      (cli/dispatch dtable *command-line-args*
                    {:coerce {:select []
                              :resource []}})

      (System/exit 0)

      (catch Exception e
        (if (some? (:stack-trace opts))
          (println e)
          (do
            (println (ex-message e))
            (let [data (ex-data e)]
              (when (and (some? data) (not= {} data))
                (println "-> " data)))))
        (System/exit 1)))))
