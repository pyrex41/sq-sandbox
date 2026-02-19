# Exec — run command in sandbox via sq-exec helper with pipe capture.
#
# Key improvements over temp-file approach:
#   - sq-exec handles isolation (bubblewrap/unshare+chroot) and timeout
#   - Pipe-based stdout/stderr capture — no temp files on disk
#   - ev/deadline timeout — SIGTERM then SIGKILL, proper cleanup
#   - Capped reads with drain — won't fill disk or block child

(import json)

(def max-output 65536)
(def timeout-exit-code 124)

(defn- read-capped
  "Read from stream up to max-bytes. Drains excess to prevent child blocking."
  [stream max-bytes]
  (def buf @"")
  (while (< (length buf) max-bytes)
    (def chunk
      (try (ev/read stream (min 4096 (- max-bytes (length buf))))
        ([e] nil)))
    (when (nil? chunk) (break))
    (when (= (length chunk) 0) (break))
    (buffer/push buf chunk))
  # Drain remaining so child's writes don't block on a full pipe
  (when (>= (length buf) max-bytes)
    (forever
      (def chunk (try (ev/read stream 4096) ([e] nil)))
      (when (or (nil? chunk) (= (length chunk) 0)) (break))))
  (string buf))

(defn exec-in-sandbox
  "Run cmd in sandbox. Returns result table.
   Uses sq-exec helper for isolation — no direct unshare/chroot calls."
  [sandbox cmd opts]
  (def workdir (get opts :workdir "/"))
  (def timeout-s (get opts :timeout 300))
  (def sdir (get opts :sandbox-dir))
  (def merged (string (get sandbox :dir) "/merged"))

  (def started (os/time))
  (def seq (+ 1 (get sandbox :exec-count 0)))
  (put sandbox :exec-count seq)
  (put sandbox :last-active started)

  # Build command array for sq-exec
  (def args ["sq-exec" merged cmd workdir (string timeout-s)])

  (var proc nil)
  (var timed-out false)
  (var stdout-str "")
  (var stderr-str "")
  (var exit-code 1)

  (try
    (do
      (set proc (os/spawn args :p {:out :pipe :err :pipe}))

      # Read stdout/stderr concurrently in fibers, results via channels
      (def out-ch (ev/chan 1))
      (def err-ch (ev/chan 1))
      (ev/spawn (ev/give out-ch (read-capped (in proc :out) max-output)))
      (ev/spawn (ev/give err-ch (read-capped (in proc :err) max-output)))

      # Safety-net timeout killer fiber (sq-exec has its own timeout, but this catches hangs)
      (def safety-timeout (+ timeout-s 5))
      (def killer
        (ev/go (fn []
          (ev/sleep safety-timeout)
          (set timed-out true)
          (try (os/proc-kill proc false :term) ([e] nil))
          (ev/sleep 1)
          (try (os/proc-kill proc) ([e] nil)))))

      # Wait for readers (they complete when child dies or pipes close)
      (set stdout-str (ev/take out-ch))
      (set stderr-str (ev/take err-ch))

      # Reap the child
      (set exit-code (os/proc-wait proc))
      (os/proc-close proc)

      # Cancel killer if process finished before timeout
      (try (ev/cancel killer "done") ([e] nil))

      (when timed-out (set exit-code timeout-exit-code)))
    ([e]
      (set stderr-str (string "exec spawn error: " e))
      (set exit-code 126)))

  (def finished (os/time))

  # Write exec log (best-effort)
  (when sdir
    (def log-dir (string sdir "/.meta/log"))
    (try
      (do
        (os/execute ["mkdir" "-p" log-dir] :p)
        (spit (string log-dir "/" (string/format "%04d" seq) ".json")
          (json/encode @{
            :seq seq :cmd cmd :workdir workdir
            :exit_code exit-code :started started :finished finished
            :stdout stdout-str :stderr stderr-str
            :timed_out timed-out})))
      ([e] nil)))

  @{:exit_code exit-code :stdout stdout-str :stderr stderr-str
    :started started :finished finished :seq seq})
