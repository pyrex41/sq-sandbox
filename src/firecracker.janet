# Firecracker backend — VM lifecycle via sq-firecracker CLI + vsock exec.
#
# Delegates heavy lifting to the sq-firecracker helper binary.
# Host-side: tap networking, CID allocation (native flock), drive hot-add.
# Guest-side: vsock JSON protocol for exec, remount, snapshot.

(import json)
(import config :prefix "config/")
(import mounts :prefix "mounts/")
(import syscalls)

(def max-output 65536)
(def timeout-exit-code 124)
(def min-cid 100)

(defn- allow-net-hosts
  "Normalize allow-net input into an array of host strings."
  [allow-net]
  (def hosts @[])
  (when allow-net
    (if (string? allow-net)
      (array/push hosts allow-net)
      (each host allow-net
        (when (string? host)
          (array/push hosts host)))))
  hosts)

# ---------------------------------------------------------------------------
# CID allocation — native flock(2) via FFI, no shell
# ---------------------------------------------------------------------------

(defn allocate-cid
  "Allocate a unique CID for a Firecracker VM. Uses flock(2) for atomicity."
  [config]
  (def data-dir (get config :data-dir))
  (def lock-path (string data-dir "/.fc-cid-counter.lock"))
  (def counter-path (string data-dir "/.fc-cid-counter"))
  (mounts/ensure-dir data-dir)
  (try
    (syscalls/flock-allocate-counter lock-path counter-path min-cid)
    ([e]
      # Fallback to shell if FFI unavailable (e.g. non-Linux)
      (def script
        (string
          "exec 9>" lock-path "; "
          "flock 9; "
          "if [ -f " counter-path " ]; then "
          "  CID=$(cat " counter-path "); "
          "else "
          "  CID=" (string min-cid) "; "
          "fi; "
          "echo $CID; "
          "echo $((CID + 1)) > " counter-path "; "
          "exec 9>&-"))
      (def proc (os/spawn ["/bin/sh" "-c" script] :p {:out :pipe}))
      (def out @"")
      (while true
        (def chunk (try (ev/read (in proc :out) 4096) ([e] nil)))
        (when (or (nil? chunk) (= (length chunk) 0)) (break))
        (buffer/push out chunk))
      (os/proc-wait proc)
      (os/proc-close proc)
      (scan-number (string/trim (string out))))))

# ---------------------------------------------------------------------------
# Network — tap device + NAT (not veth like chroot backend)
# ---------------------------------------------------------------------------

(defn fc-setup-network
  "Create tap device and NAT rules for a Firecracker VM."
  [id index allow-net]
  (def tap-name (string "sq-" id "-tap"))
  (def host-ip (string/format "10.0.%d.1/30" index))
  (def subnet (string/format "10.0.%d.0/30" index))
  (def hosts (allow-net-hosts allow-net))
  (def chain (string "SQ-" id))

  # Create tap device
  (os/execute ["ip" "tuntap" "add" "dev" tap-name "mode" "tap"] :p)
  (os/execute ["ip" "addr" "add" host-ip "dev" tap-name] :p)
  (os/execute ["ip" "link" "set" tap-name "up"] :p)

  # NAT for outbound traffic (always needed).
  (os/execute ["iptables" "-t" "nat" "-A" "POSTROUTING"
               "-s" subnet "-j" "MASQUERADE"] :p)

  # Optional egress allow-list chain.
  (when (> (length hosts) 0)
    (os/execute ["iptables" "-N" chain] :p)
    (os/execute ["iptables" "-I" "FORWARD" "-i" tap-name "-j" chain] :p)
    (os/execute ["iptables" "-A" chain "-m" "conntrack" "--ctstate" "ESTABLISHED,RELATED" "-j" "ACCEPT"] :p)
    (os/execute ["iptables" "-A" chain "-p" "udp" "--dport" "53" "-j" "ACCEPT"] :p)
    (os/execute ["iptables" "-A" chain "-p" "tcp" "--dport" "53" "-j" "ACCEPT"] :p)
    (each host hosts
      (if (string/find "*" host)
        (eprintf "fc-net: wildcard %s requires proxy mode, skipping\n" host)
        (os/execute ["iptables" "-A" chain "-d" host "-j" "ACCEPT"] :p)))
    (os/execute ["iptables" "-A" chain "-j" "DROP"] :p))

  @{:tap-name tap-name
    :index index
    :host-ip host-ip
    :subnet subnet
    :allow-net allow-net
    :chain (if (> (length hosts) 0) chain nil)})

(defn fc-teardown-network
  "Remove tap device and NAT rules."
  [id index]
  (def tap-name (string "sq-" id "-tap"))
  (def subnet (string/format "10.0.%d.0/30" index))
  (def chain (string "SQ-" id))

  # Remove optional egress chain.
  (try (os/execute ["iptables" "-D" "FORWARD" "-i" tap-name "-j" chain] :p)
    ([e] nil))
  (try (os/execute ["iptables" "-F" chain] :p)
    ([e] nil))
  (try (os/execute ["iptables" "-X" chain] :p)
    ([e] nil))

  # Remove NAT rule (best-effort)
  (try (os/execute ["iptables" "-t" "nat" "-D" "POSTROUTING"
                     "-s" subnet "-j" "MASQUERADE"] :p)
    ([e] nil))

  # Remove tap device
  (try (os/execute ["ip" "link" "delete" tap-name] :p)
    ([e] nil)))

# ---------------------------------------------------------------------------
# VM lifecycle — shell out to sq-firecracker
# ---------------------------------------------------------------------------

(defn fc-start-vm
  "Start a Firecracker VM via sq-firecracker CLI."
  [id cpu memory-mb squashfs-paths cid meta-dir]
  (def args @["sq-firecracker" "start" id
              (string cpu) (string memory-mb)])
  (each p squashfs-paths
    (array/push args p))
  (def rc (os/execute args :p {:env @{"FC_CID" (string cid)}}))
  (when (not= rc 0)
    (error (string/format "sq-firecracker start failed for %s (exit %d)" id rc)))

  # Write CID to meta directory for later use
  (mounts/ensure-dir meta-dir)
  (spit (string meta-dir "/cid") (string cid))
  (spit (string meta-dir "/vm-pid") (string id)))

(defn fc-stop-vm
  "Stop a Firecracker VM via sq-firecracker CLI."
  [id meta-dir]
  (def rc (try (os/execute ["sq-firecracker" "stop" id] :p) ([e] 1)))
  (when (not= rc 0)
    (eprintf "warning: sq-firecracker stop failed for %s (exit %d)\n" id rc)))

# ---------------------------------------------------------------------------
# Vsock exec — send JSON via socat, parse response
# ---------------------------------------------------------------------------

(defn- read-capped
  "Read from stream up to max-bytes. Drains excess."
  [stream max-bytes]
  (def buf @"")
  (while (< (length buf) max-bytes)
    (def chunk
      (try (ev/read stream (min 4096 (- max-bytes (length buf))))
        ([e] nil)))
    (when (nil? chunk) (break))
    (when (= (length chunk) 0) (break))
    (buffer/push buf chunk))
  (when (>= (length buf) max-bytes)
    (forever
      (def chunk (try (ev/read stream 4096) ([e] nil)))
      (when (or (nil? chunk) (= (length chunk) 0)) (break))))
  (string buf))

(defn fc-exec
  "Execute a command in a Firecracker VM via vsock."
  [cid cmd workdir timeout-s]
  (def payload (json/encode @{:cmd cmd :workdir workdir :timeout timeout-s}))
  (def socat-timeout (string (+ timeout-s 5)))

  (var proc nil)
  (var timed-out false)
  (var stdout-str "")
  (var exit-code 1)

  (try
    (do
      (set proc (os/spawn
                  ["socat" (string "-T" socat-timeout)
                   "-" (string "VSOCK-CONNECT:" cid ":5000")]
                  :p {:in :pipe :out :pipe :err :pipe}))

      # Send the JSON payload, then close stdin
      (ev/write (in proc :in) payload)
      (ev/close (in proc :in))

      # Read stdout concurrently with a timeout killer
      (def out-ch (ev/chan 1))
      (def err-ch (ev/chan 1))
      (ev/spawn (ev/give out-ch (read-capped (in proc :out) max-output)))
      (ev/spawn (ev/give err-ch (read-capped (in proc :err) max-output)))

      (def killer
        (ev/go (fn []
          (ev/sleep (+ timeout-s 10))
          (set timed-out true)
          (try (os/proc-kill proc false :term) ([e] nil))
          (ev/sleep 1)
          (try (os/proc-kill proc) ([e] nil)))))

      (set stdout-str (ev/take out-ch))
      (def stderr-str (ev/take err-ch))
      (def socat-exit (os/proc-wait proc))
      (os/proc-close proc)
      (try (ev/cancel killer "done") ([e] nil))

      (when (not= socat-exit 0)
        (when (not timed-out)
          (error (string/format "socat failed (exit %d): %s" socat-exit stderr-str))))

      # Parse JSON response from guest agent
      (if timed-out
        @{:exit_code timeout-exit-code
          :stdout "" :stderr "vsock exec timed out"
          :timed_out true}
        (do
          (def resp (try (json/decode stdout-str)
                     ([e] @{"exit_code" 1
                            "stdout" ""
                            "stderr" (string "failed to parse guest response: " stdout-str)})))
          @{:exit_code (get resp "exit_code" 1)
            :stdout (get resp "stdout" "")
            :stderr (get resp "stderr" "")
            :timed_out false})))
    ([e]
      @{:exit_code 126
        :stdout ""
        :stderr (string "vsock exec error: " e)
        :timed_out false})))

# ---------------------------------------------------------------------------
# Drive hot-add + remount
# ---------------------------------------------------------------------------

(defn fc-add-drive
  "Hot-add a squashfs drive to a running VM and trigger remount."
  [id drive-id squashfs-path cid meta-dir]
  # Add drive via sq-firecracker CLI
  (def rc (os/execute ["sq-firecracker" "add-drive" id drive-id squashfs-path] :p))
  (when (not= rc 0)
    (error (string/format "sq-firecracker add-drive failed for %s/%s" id drive-id)))

  # Tell guest agent to remount
  (def resp (fc-exec cid "__squash_remount" "/" 30))
  (when (not= (get resp :exit_code) 0)
    (error (string/format "guest remount failed: %s" (get resp :stderr ""))))
  resp)
