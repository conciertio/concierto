(ns concierto.cli
  (:require
   [concierto.deploy :as d]
   [concierto.core :as core]
   [concierto.service :as service]
   [clojure.string :as str]
   [concierto.remote :as remote]
   [babashka.signal :as signal]
   [babashka.process :refer [exec]]
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [cheshire.core :as json]))

(defn- list-machines [args  machine _init-val]
  (let [fields (or (core/get-option args :list)
                   [:host :ip :cluster :role])]
    (println (str (->> fields
                       (map #(keyword %1))
                       (select-keys machine)
                       vals
                       (map #(name %1))
                       (str/join " "))))))

(defn- machines [args]
  (if (core/get-option args :list)
    (core/with-machines args list-machines)
    (if (core/get-option args :json)
      (println (json/encode (flatten (:machines (core/gather args)))))
      (core/pretty (flatten (:machines (core/gather args)))))))

(defn- gather [args]
  (let [vars (core/gather args)]
    (core/format-out args vars)))

(defn- get-deps [args]
  (when-let [role (core/get-option args :role)]
    (println (core/role-deps role))))

(defn- ssh [args {:keys [ip host]} _init-val]
  (if (core/verbose?)
    (println (str/trim (core/ssh-raw ip (str/join " " (:args args)))))
    (println (str/trim (str host ":" (core/ssh-raw ip (str/join " " (:args args))))))))

(defn- deploy [args]
  (core/with-machines args d/deploy d/deploy-init))

(defn- build [args]
  (service/build args (core/get-scenario)))

(defn- push [args]
  (service/push args (first (get args :args))))

(defn- build-services [args services]
  (doseq [service services]
    (when (not (signal/pipe-signal-received?))
      (try
        (core/vprint "BUILDING SERVICE " service)
        (service/process-build args service)
        (catch Exception e
          (core/warn "Can't build " service ":" (ex-message e)))))))

(defn- build-all [args]
  (let [build-meta (read-string
                    (slurp (core/path
                            (core/services-dir)
                            "build.edn")))
        build-order (:build-order build-meta)]
    (build-services args build-order)))

(defn- build-role [args]
  (let [role (core/get-option args :role)
        services (core/role-services role)]
    (build-services args services)))

(defn- conf-set [args]
  (let [conf (core/get-conf)]
    (core/write-data
     core/CONF-FILE
     (assoc conf (keyword (first (:args args)))
            (second (:args args))))))

(defn- conf-unset [args]
  (let [conf (core/get-conf)]
    (core/write-data
     core/CONF-FILE
     (dissoc conf (keyword (first (:args args)))))))

(defn- visit [args]
  (let [host (first (:args args))
        machine (seq (filter #(= (:host %1) host) (core/machines)))]
    (if (some? machine)
      (let [ip (:ip (first machine))
            ssh (core/ssh-str ip (str/join " " (drop 1 (:args args))))]
        (exec ssh))
      (core/vprint "server not registered"))))

(defn template [args]
  (let [templated (core/template-file args (core/get-option args :in))
        out-file (core/get-option args :out)]
    (if (some? out-file)
      (do
        (spit out-file templated)
        (core/vprint "Wrote" out-file))
      (println templated))))

(defn list-seq [args fun]
  (let [data (fun)]
    (core/format-out args data)))

(defn- version [args]
  (let [v (read-string (slurp (io/resource "version.edn")))]
    (if (core/get-option args :tag)
      (println (:tag v))
      (core/format-out args v))))

(defn- lint [args]
  (core/lint args))

(defn help [_args table]
  (println "\nCOMMANDS")
  (println "---------\n")
  (let [sep (atom "15")]
    (doseq [cmd-spec table]
      (let [c (:cmds cmd-spec)]
        (when (and (seq c) (:help cmd-spec))
          (if (= c ["divider"])
            (do
              (reset! sep "30")
              (println "\nEXTENSIONS")
              (println "----------\n"))

            (println (format (str "%-" @sep "s %s") (str/join " " c) (:help cmd-spec))))))))
  (println "
            
MACHINE SELECTION
-----------------
            
Select machines with --select <key val>* also <key val>*

e.g. ./concierto machines --select cluster us role api        

ENVIRONMENT VARIABLES
---------------------
            
CONCIERTO_APP    set the app directory            
CONCIERTO_SCENARIO set the scenario directory
CONCIERTO_TARGET set the target directory
            
SWITCHES
--------
            
--verbose (-v) switch on messages
--dry-run      print commands to execute
--role <role>  specify role when not included in machine selection
--json         output as json
--list         output as list
--debug        dump templated deploy into 'debug' dir

DEFAULTS
--------

The following variables may be set in config using the set command,
and are stored in " core/CONF-FILE "

scenario    - provide a default scenario
target      - provide a default target
engine      - provide a default engine; docker or podman (default docker)
extensions  - merge extensions (default true)
remote-exec - provide a path to a file that is executed remotely on deploy                                   

e.g. ./concierto set scenario myscenario
                                    
For more info mail contact@conciert.io
"))

(defn- want-extensions? []
  (let [given-ext (:extensions (core/get-conf))]
    (if (some? given-ext)
      given-ext
      true)))

(defn- merge-extensions [main-table]
  (let [ext-dir (core/extensions-dir)
        final-table
        (if (and (want-extensions?) true (fs/exists? ext-dir))
          (->> (fs/match ext-dir "glob:**/extend.clj" {:recursive true})
               (map #(core/path %1))
               (reduce (fn [acc ext]
                         (concat acc ((load-file ext))))

                       main-table))
          main-table)]
    ; always add the default help table at the end
    (concat final-table {:cmds [] :fn #(help %1 final-table)
                         :help "Show help"})))


(def table
  [{:cmds ["scenarios"] :fn #(list-seq %1 core/list-scenarios)
    :help "List scenarios"}

   {:cmds ["targets"] :fn #(list-seq %1 core/list-targets)
    :help "List targets"}

   {:cmds ["machines"]
    :fn machines
    :coerce {:json :boolean :list []}
    :help "List machines"}

   {:cmds ["services"] :fn #(list-seq %1 core/list-services)
    :help "List services"}

   {:cmds ["gather"] :fn gather
    :help "Show all defined attributes of target"}

   {:cmds ["ssh"] :fn #(core/with-machines %1 ssh)
    :help "Execute a command over set of machines"}

   {:cmds ["set"] :fn conf-set
    :help (str "Set a variable in " core/CONF-FILE)}

   {:cmds ["unset"] :fn conf-unset
    :help (str "Unset a variable in " core/CONF-FILE)}

   {:cmds ["visit"] :fn visit
    :help "Visit a machine by IP, specified by host, e.g. visit stage-api1"}

   {:cmds ["exec"] :fn core/exec-templated-file
    :args->opts [:script]
    :require [:script]
    :help "Run a templated script locally"}

   {:cmds ["rexec"] :fn #(core/with-machines %1 remote/ext-remote remote/ext-remote-init)
    :args->opts [:script]
    :require [:script]
    :help "Run a templated script remotely over set of machines"}

   {:cmds ["template"] :fn template
    :help "Template a file, --in <file> --out <file>, otherwise prints to stdout"}

  ;;  {:cmds ["deps"] :fn get-deps
  ;;   :args->opts [:role]
  ;;   :require [:role]
  ;;   :help "List service dependencies of role"}

   {:cmds ["build" "all"] :fn build-all
    :help "Build all services listed in services/build.edn in order, --push to push"}

   {:cmds ["build" "role"] :fn build-role
    :help "Build all services referenced by role, --push to push"}

   {:cmds ["build"] :fn build :opts
    {:coerce {:nocache :boolean :push :boolean}}
    :help "Build the service(s), --push to push"}

   {:cmds ["push"] :fn push
    :help "Push the built service to registry"}

   {:cmds ["deploy"] :fn #(core/with-machines %1 d/deploy d/deploy-init)
    :help "Deploy over set of machines"}

   {:cmds ["lint"] :fn lint
    :help "Check for inconsistencies (TBD)"}

   #_{:cmds ["undeploy"] :fn #(core/with-machines %1 d/undeploy)
      :help "Undeploy locally the currently running containers"}

   {:cmds ["version"] :fn version
    :help "Current version"}

   {:cmds ["divider"] :help "just print out a divider"}])


(defn -main [& _args]
  (let [dispatch-with-ext (merge-extensions table)]
    (if (or (nil? (seq *command-line-args*))
            (some #{"help"} *command-line-args*))
      (help *command-line-args* dispatch-with-ext)
      (core/init dispatch-with-ext))))
