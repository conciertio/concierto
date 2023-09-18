(ns concierto.deploy
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [selmer.parser :as sel]
            [selmer.filters :as filt]
            [concierto.core :as core]
            [clojure.java.io :as io]
            [clj-yaml.core :as yaml]))

(defn- get-executor
  "Find remote execution script defined in conf as :remote-exec"
  [dynamic-vars]
  (or
   (when-let [f (:remote-exec (core/get-conf))]
     (sel/render (slurp f) dynamic-vars))
   (sel/render (slurp (io/resource "remote-exec")) dynamic-vars)))

(defn- role-text [role]
  (slurp (core/path (core/scenario-dir) (str (name role) ".yml"))))

(defn- get-env-files [out-str]
  (->>
   (:services (yaml/parse-string out-str))
   (map (fn [[k, v]] [k (first (:env_file v))]))
   (filter (fn [[_k v]] (some? v)))))

(defn- get-deploy-env [service env-file]
  (let [f (core/path "services" (name service) env-file)]
    (when (core/file-exists? f)
      (core/get-file f))))

(defn- service-to-deploy [args]
  (if-let [service (core/get-option args :service)]
    service
    ""))

(defn- deploy-setup
  "Called once per machine to initalize the machine dir to upload"
  [_args machine-data init]
  (filt/add-filter! :registry  core/filter-add-repo)
  (let [role (name (:role machine-data))
        {:keys [vars tempd]} init
        {:keys [ip host cluster]} machine-data
        dynamic-var (assoc vars
                           :machine machine-data
                           :cluster (get-in vars [:clusters (keyword cluster)]))
        raw-yaml (role-text role)
        raw-env (get-env-files raw-yaml)
        yml (str role ".yml")]

    (core/vprint "Processing" host "with role" role)

    (let [machine-dir (core/path tempd ip role)
          executor-location (core/path machine-dir "executor")
          executor (get-executor dynamic-var)]
        ; write the final yaml file once per machine

      (fs/create-dirs machine-dir)

      (spit (core/path machine-dir yml)
            (sel/render raw-yaml dynamic-var))

      (spit executor-location executor)
      (fs/set-posix-file-permissions executor-location "rwx------")

        ; potentially write environment files
      (doseq [[service env-file] raw-env]
        (let [env (get-deploy-env service env-file)]
          (when (some? env)
            (spit (core/path machine-dir
                             (str/replace env-file "./" ""))
                  (sel/render env dynamic-var))))))))

(defn- deploy-remote
  "Called once per machine after the initial setup by deploy-setup.
   Get the machine dir based on it's IP address, then tar that directory
   up and scp it to the remote machine.
   Untar and call executor file on remote machine."
  [args machine-data init]
  (let [{:keys [tempd]} init
        role (name (:role machine-data))
        ip (:ip machine-data)
        machine-dir (core/path tempd ip)
        tar-id (core/get-uuid)
        tar-file (str tar-id ".tar")
        tar-local (str tempd "-" tar-file)
        ssh-info (core/ssh-info ip)
        copy-to-remote (str "scp -4 -p "
                            "-P " (:port ssh-info)
                            " " tar-local " "
                            (:user ssh-info)
                            "@"
                            ip
                            ":" tar-file)]

    (core/cshell {:err :string}
                 (str "tar cz -C " machine-dir " -f " tar-local " ."))

    (core/cshell copy-to-remote)
    (core/ssh-raw ip (str "mkdir " tar-id " && "
                          "tar xzf " tar-id ".tar -C " tar-id " && "
                          "cd " tar-id "/" role " && "
                          "./executor"))

    (core/ssh-raw ip (str "rm -rf " tar-id " && rm " tar-id ".tar"))

    (core/run-event "on-deploy" args)))

(defn- deploy-local [args machine-data init]
  (let [{:keys [tempd]} init
        role (name (:role machine-data))
        d (core/path tempd (:ip machine-data))
        compose (str (core/engine-compose) " -f "
                     (core/path d role
                                (str role ".yml"))
                     " up --force-recreate --detach "
                     (service-to-deploy args))]

    (core/cshell compose)))

; -----------------------------------------------------
; deploy-init and deploy called by core/with-machines.

(defn deploy-init
  "Initialise the temporary directory for this deploy.
   Get all the attributes."
  [args _machines]
  (let [debug-dir (core/path (core/app-dir) "debug")]
    (when (fs/exists? debug-dir)
      (fs/delete-tree debug-dir))
    (let [tempd (core/create-temp-dir)]
      {:vars (core/gather args)
       :tempd tempd})))

(defn deploy [args machine-data init]
  (when-not (some? (core/get-role-from-args args))
    (core/hurl "A deploy must have a role specified"))

  (deploy-setup args machine-data init)

  (if-not (core/get-option args :debug)
    (if (= (:host machine-data) "localhost")
      (deploy-local args machine-data init)
      (deploy-remote args machine-data init))
    (let [debug-dir (core/path (core/app-dir) "debug")
          dest-dir (core/path debug-dir (name (:host machine-data)))
          source-dir (str (:tempd init) "/" (:ip machine-data))]
      (fs/copy-tree source-dir dest-dir)
      :ok)))


(defn undeploy [args machine-data _init]
  (let [{:keys [ip host]} machine-data
        role (core/get-role-from-args args)]
    (if (= host "localhost")
      (core/cshell (str (core/engine-compose) " -f "
                        (core/path (core/working-dir)
                                   role (str role ".yml"))
                        " down"))
      (core/ssh-raw
       ip
       (str (core/engine-compose) " -f "
            (core/path "catalina" role (str role ".yml"))
            " down")))))