# Reaper — background fiber to destroy expired sandboxes.
#
# Runs as an ev fiber alongside net/server handlers.
# ev/sleep yields to other fibers (doesn't block the thread).

(import manager :prefix "manager/")

(def interval-s 10)

(defn reap-expired
  "Destroy sandboxes that have exceeded max_lifetime_s since last_active."
  [mgr]
  (def now (os/time))
  (def to-destroy @[])
  (each [id s] (pairs (get mgr :sandboxes))
    (when (and s (not= s :creating))
      (def max-s (get s :max-lifetime-s 0))
      (when (> max-s 0)
        (def last-active (get s :last-active 0))
        (when (>= (- now last-active) max-s)
          (array/push to-destroy id)))))
  (each id to-destroy
    (try
      (do
        (eprintf "reaper: destroying expired sandbox %s\n" id)
        (manager/manager-destroy mgr id))
      ([e] (eprintf "reaper: failed to destroy %s: %s\n" id (string e))))))

(defn start-reaper
  "Start reaper as a background ev fiber. Returns the fiber."
  [mgr]
  (ev/spawn
    (forever
      (ev/sleep interval-s)
      (try (reap-expired mgr)
        ([e] (eprintf "reaper: error: %s\n" (string e)))))))
