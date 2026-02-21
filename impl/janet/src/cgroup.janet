# Cgroup v2 — REMOVED in unprivileged mode.
# Sandboxes run under bubblewrap without cgroup isolation.

(defn create-cgroup [id cpu-cores memory-mb] nil)
(defn destroy-cgroup [handle] nil)
