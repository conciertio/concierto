(ns concierto.service
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [concierto.core :as core]
            [selmer.parser :as sel]
            [selmer.filters :as filt]))

(defn exec-templated-file
  "Execute the provided file but first substitute all templating variables.
   Works with all types of file."
  [service name env]
  ; remember the env already contains :dir and :extra-env 
  (let [tmp (core/create-temp-file)
        serviced (core/service-dir service)
        tmpl-file (core/path serviced name)]
    (when (fs/exists? tmpl-file)
      (spit tmp (sel/render (slurp tmpl-file) (:extra-env env)))
      (fs/set-posix-file-permissions tmp "rwx------")
      (core/vprint "Executing" service name "...")
      (shell env tmp))))

(defn- template-build-file [serviced env build-file]
  (let [template-file (core/path serviced build-file)
        final (sel/render (slurp template-file) env)
        tmp-docker (core/create-temp-file)]
    (spit tmp-docker final)
    (fs/absolutize tmp-docker)))

(defn filter-add-repo
  "when building need to build locally (as opposed to pushing which needs
   remote repo)"
  [image]
  image)

(defn no-cache [args]
  (if-not (core/get-option args :cache)
    " --no-cache "
    ""))

(defn push [args service]
  (let [vars (core/gather args)
        key-service (keyword service)
        image-name (get-in vars [:services key-service :image])
        registry (core/get-registry)
        remote-name (str registry
                         "/"
                         image-name)
        verify (get-in vars [:access :docker :registry :tls-verify] true)]

    (core/vprint "Pushing service"
                 (get-in vars [:services key-service :image])
                 "to"
                 registry)

    (core/cshell (str (core/engine) " tag " image-name " " remote-name))
    (if (and (= (core/engine) "podman") (false? verify))
      (core/cshell (str (core/engine) " push " " --tls-verify=false " remote-name))
      (core/cshell (str (core/engine) " push " remote-name)))))

(defn process-build [args service]

  (filt/add-filter! :registry filter-add-repo)

  (let [push? (get-in args [:opts :push])
        serviced (core/service-dir service)
        build-file (core/get-option args :build-file "Dockerfile")
        key-service (keyword service)]

    (core/vprint "Building with" build-file)

    (when-not (fs/exists? (core/path serviced build-file))
      (core/hurl (str "Must have " build-file " when building")))

    (let [vars (core/gather args)
          env {:dir serviced
               :extra-env vars}]

      (exec-templated-file service "pre_build" env)

      (let [templated-docker (template-build-file serviced vars build-file)
            build-str (str
                       (core/engine)
                       " build"
                       (no-cache args)
                       " -t "
                       (get-in vars [:services key-service :image])
                       " -f "
                       templated-docker
                       " .")]

        (core/vprint build-str)

        (core/cshell env build-str)

        (exec-templated-file service "post_build" env)

        (when push?
          (push args service))))))


(defn build [args scenario]
  (when (core/scenario-exists? scenario)
    (core/set-scenario scenario))
  (let [services (:args args)]
    (doseq [service services]
      (try
        (core/vprint "Processing " (core/service-dir service))
        (if (fs/directory? (core/service-dir service))
          (process-build args service)
          (core/vprint "service " service " doesn't exist in services"))
        (catch Exception e
          (core/vprint "Can't build " service ":" (ex-message e))
          (core/vprint e))))))

