# Manager — sandbox registry, per-sandbox lock
# Uses a table for sandboxes, ev/mutex for concurrency.

(import sandbox :prefix "sandbox/")

(defn make-manager [config]
  @{
    :config config
    :sandboxes @{}
    :lock (ev/lock)
  })

(defn manager-create [manager id opts]
  "Create sandbox. Returns sandbox or throws."
  (ev/with-lock (get manager :lock)
    (when (get (get manager :sandboxes) id)
      (error (string/format "sandbox already exists: %s" id)))
    (def count (length (keys (get manager :sandboxes))))
    (when (>= count (get (get manager :config) :max-sandboxes))
      (error (string/format "sandbox limit reached: %d" (get (get manager :config) :max-sandboxes))))
    (def sandbox (sandbox/create-sandbox (get manager :config) id opts))
    (put (get manager :sandboxes) id sandbox)
    sandbox))

(defn manager-destroy [manager id]
  "Destroy sandbox by id."
  (ev/with-lock (get manager :lock)
    (def sandbox (get (get manager :sandboxes) id))
    (when sandbox
      (sandbox/destroy-sandbox sandbox)
      (put (get manager :sandboxes) id nil)
      (put (get manager :sandboxes) id nil))
    sandbox))

(defn manager-get [manager id]
  (get (get manager :sandboxes) id))

(defn manager-list [manager]
  (def result @[])
  (each [k v] (pairs (get manager :sandboxes))
    (when v (array/push result v)))
  result)
