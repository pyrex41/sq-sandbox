# Modules — list available squashfs layers

(import config :prefix "config/")

(defn list-modules [config]
  "Return array of module names (without .squashfs)."
  (def mod-dir (config/modules-dir config))
  (def result @[])
  (try
    (each entry (os/dir mod-dir)
      (when (string/has-suffix? entry ".squashfs")
        (array/push result (string/slice entry 0 (- (length entry) 8)))))
    ([e] nil))
  (sort result))

(defn module-exists? [config name]
  (def path (string (config/modules-dir config) "/" name ".squashfs"))
  (os/stat path))

(defn base-module-exists? [config]
  (module-exists? config "000-base-alpine"))
