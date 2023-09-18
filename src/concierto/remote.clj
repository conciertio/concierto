(ns concierto.remote
  (:require [babashka.fs :as fs]
            [concierto.core :as core]))

(defn ext-remote-init [args _machines]
  (let [file-to-exec (core/get-option args :script)]
    (if (fs/exists? file-to-exec)
      (let [tmpFile (core/template file-to-exec (core/gather args))]
        (core/cshell (str "chmod u+x" tmpFile))
        {:tmp-file tmpFile})
      (core/hurl (str "File " file-to-exec " does not exist")))))

(defn ext-remote [_args {:keys [ip]} init-val]
  (let [tmp-file (:tmp-file init-val)
        remote-tmp (core/get-uuid)
        ssh-info (core/ssh-info ip)
        quiet (if (core/verbose?) " -q " "")
        copy-to-remote (str "scp -4 -p " quiet
                            "-P " (:port ssh-info)
                            " " tmp-file " "
                            (:user ssh-info)
                            "@"
                            ip
                            ":" remote-tmp)]

    (core/cshell copy-to-remote)
    (core/ssh-raw ip (str "./" remote-tmp ";rm " remote-tmp))))

