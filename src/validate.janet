# Validation — sandbox IDs, labels, module names
# Prevents path traversal and keeps filesystem operations safe.

(def id-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")
(def label-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.")

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
