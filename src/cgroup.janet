# Cgroup v2 resource limits.
# Writes cpu.max and memory.max under /sys/fs/cgroup/squashd-<id>.
# Uses spit for file writes — no shell needed.

(def cgroup-root "/sys/fs/cgroup")
(def cgroup-period-us 100000)

(defn create-cgroup
  "Create a cgroupv2 directory and write cpu.max + memory.max.
   cpu-cores is a float (2.0 = 200%% of one core).
   memory-mb is an integer (1024 = 1 GiB).
   Returns handle table."
  [id cpu-cores memory-mb]
  (def cgroup-path (string cgroup-root "/squashd-" id))

  # Create cgroup directory
  (os/shell (string "mkdir -p " cgroup-path))

  # cpu.max: "<quota> <period>"
  (def quota (math/round (* cpu-cores cgroup-period-us)))
  (spit (string cgroup-path "/cpu.max")
        (string/format "%d %d" quota cgroup-period-us))

  # memory.max: bytes
  (spit (string cgroup-path "/memory.max")
        (string/format "%d" (* memory-mb 1048576)))

  @{:path cgroup-path :id id})

(defn destroy-cgroup
  "Remove cgroup directory. Moves processes to parent first."
  [handle]
  (when handle
    (try
      (do
        # Move remaining processes to parent cgroup
        (def procs-path (string (get handle :path) "/cgroup.procs"))
        (try
          (do
            (def content (slurp procs-path))
            (when content
              (each line (string/split "\n" (string/trim content))
                (when (> (length line) 0)
                  (try (spit (string cgroup-root "/cgroup.procs") line)
                    ([e] nil))))))
          ([e] nil))
        # rmdir the cgroup
        (os/execute ["rmdir" (get handle :path)] :p))
      ([e] nil))))
