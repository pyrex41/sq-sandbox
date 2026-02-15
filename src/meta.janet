# Sandbox metadata — read/write .meta/sandbox.json

(import json)

(defn write-sandbox-meta [sandbox-dir meta]
  (def path (string sandbox-dir "/.meta/sandbox.json"))
  (os/shell (string "mkdir -p " sandbox-dir "/.meta"))
  (spit path (json/encode meta)))

(defn read-sandbox-meta [sandbox-dir]
  (def path (string sandbox-dir "/.meta/sandbox.json"))
  (try
    (when (os/stat path)
      (def content (slurp path))
      (when (and content (> (length content) 0))
        (json/decode content)))
    ([e] nil)))
