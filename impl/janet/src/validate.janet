# Validation — sandbox IDs, labels, module names
# Prevents path traversal and keeps filesystem operations safe.

(def id-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")
(def label-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.")
(def allow-net-host-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-.:/*")

(defn valid-id? [id]
  "Valid IDs: 1-64 chars, alphanumeric plus hyphens and underscores.
   Must not start or end with hyphen."
  (and (string? id)
       (> (length id) 0)
       (<= (length id) 64)
       (not (string/has-prefix? "-" id))
       (not (string/has-suffix? "-" id))
       (string/check-set id-chars id)))

(defn valid-label? [label]
  "Valid labels: 1-128 chars, alphanumeric plus hyphens, underscores, dots."
  (and (string? label)
       (> (length label) 0)
       (<= (length label) 128)
       (string/check-set label-chars label)))

(defn valid-module? [name]
  "Same rules as valid-label."
  (valid-label? name))

(defn valid-workdir? [workdir]
  "Workdir must be absolute path, no .., reasonable length."
  (and (string? workdir)
       (> (length workdir) 0)
       (<= (length workdir) 512)
       (string/has-prefix? "/" workdir)
       (not (string/find ".." workdir))))

(defn valid-cmd? [cmd]
  "Cmd must be non-empty string, reasonable length."
  (and (string? cmd)
       (> (length (string/trim cmd)) 0)
       (<= (length cmd) 65536)))

(defn clamp-timeout [val min-s max-s]
  (def n (if (number? val) (math/floor val) min-s))
  (def clamped (if (< n min-s) min-s (if (> n max-s) max-s n)))
  clamped)

(defn valid-allow-net-host? [host]
  "Validate allow-net host/CIDR/wildcard token conservatively."
  (when (not (string? host))
    (return false))
  (def h (string/trim host))
  (when (or (= (length h) 0) (> (length h) 253))
    (return false))
  (when (or (string/find "\0" h)
            (string/find " " h)
            (string/find "\t" h)
            (string/find "\n" h)
            (string/find "\r" h)
            (string/find "\"" h)
            (string/find "'" h))
    (return false))
  (when (not (string/check-set allow-net-host-chars h))
    (return false))
  (var star-count 0)
  (each ch h
    (when (= ch 42) (++ star-count))) # '*' byte value
  (when (> star-count 1)
    (return false))
  (when (= star-count 1)
    (when (or (not (string/has-prefix? "*." h))
              (= (length h) 2))
      (return false)))
  true)
