# Sandbox metadata — read/write .meta/sandbox.json

(import json)

(defn write-sandbox-meta [sandbox-dir meta]
  (def path (string sandbox-dir "/.meta/sandbox.json"))
  (os/execute ["mkdir" "-p" (string sandbox-dir "/.meta")] :p)
  (spit path (json/encode meta)))

(defn write-policy [sandbox-dir policy]
  "Write policy to .meta/policy as JSON. No-op if policy is nil."
  (when policy
    (def path (string sandbox-dir "/.meta/policy"))
    (os/execute ["mkdir" "-p" (string sandbox-dir "/.meta")] :p)
    (spit path (json/encode policy))))

(defn read-policy [sandbox-dir]
  "Read policy from .meta/policy. Returns nil if absent."
  (def path (string sandbox-dir "/.meta/policy"))
  (try
    (when (os/stat path)
      (def content (slurp path))
      (when (and content (> (length content) 0))
        (json/decode content)))
    ([e] nil)))

(defn read-sandbox-meta [sandbox-dir]
  (def path (string sandbox-dir "/.meta/sandbox.json"))
  (try
    (when (os/stat path)
      (def content (slurp path))
      (when (and content (> (length content) 0))
        (def m (json/decode content))
        (def policy (read-policy sandbox-dir))
        (when policy (put m "policy" policy))
        m))
    ([e] nil)))
