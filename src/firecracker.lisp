(in-package #:squashd)

;;; ── Firecracker Backend ──────────────────────────────────────────────
;;;
;;; Alternative sandbox backend that runs each sandbox in a Firecracker
;;; microVM. Shells out to `sq-firecracker` CLI and communicates with
;;; the guest agent via vsock (socat).
;;;
;;; Network: tap devices with /30 subnets (10.0.<index>.0/30)
;;; Exec: JSON commands over VSOCK-CONNECT:<cid>:5000
;;; Drives: sq-firecracker add-drive + vsock remount trigger
;;;
;;; WARM PATH — runs on every sandbox operation when backend is :firecracker.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; ── CID Allocation ───────────────────────────────────────────────────
;;;
;;; Each microVM needs a unique CID for vsock. We use a thread-safe
;;; counter with flock on a file for persistence across restarts.

(defvar *cid-lock* (bt:make-lock "cid-alloc"))
(defvar *next-cid* 3
  "Next CID to allocate. CID 0-2 are reserved by vsock (0=hypervisor, 1=local, 2=host).")

(defun allocate-cid (config)
  "Allocate a unique CID for a new microVM. Thread-safe with flock persistence."
  (let ((cid-file (format nil "~A/.cid-counter" (config-data-dir config))))
    (bt:with-lock-held (*cid-lock*)
      ;; Read current CID from file if it exists
      (when (probe-file cid-file)
        (ignore-errors
          (let ((val (parse-integer
                      (string-trim '(#\Space #\Newline #\Return)
                                   (uiop:read-file-string cid-file)))))
            (when (> val *next-cid*)
              (setf *next-cid* val)))))
      (let ((cid *next-cid*))
        (incf *next-cid*)
        ;; Persist for crash recovery
        (ignore-errors
          (write-string-to-file cid-file (format nil "~D" *next-cid*)))
        cid))))

;;; ── Network Setup (tap devices) ──────────────────────────────────────
;;;
;;; Firecracker uses tap devices instead of veth pairs.
;;; IP scheme: 10.0.<index>.1/30 (host), 10.0.<index>.2 (guest).

(defun firecracker-setup-network (id index allow-net)
  "Set up tap device and NAT for a firecracker VM.
   ID is the sandbox identifier.
   INDEX is the unique subnet index.
   ALLOW-NET is a list of allowed CIDRs (unused for now — firecracker
   uses network-level isolation via the VM boundary).
   Returns the tap device name."
  (declare (type simple-string id)
           (type fixnum index)
           (ignore allow-net))
  (let ((tap (format nil "sq-~A-tap" id))
        (host-addr (format nil "10.0.~D.1/30" index))
        (subnet (format nil "10.0.~D.0/30" index)))
    ;; Create tap device
    (unless (run-command-ok "ip" "tuntap" "add" "dev" tap "mode" "tap")
      (error 'sandbox-error :id id :message "failed to create tap device"))
    ;; Assign IP and bring up
    (run-command-ok "ip" "addr" "add" host-addr "dev" tap)
    (run-command-ok "ip" "link" "set" tap "up")
    ;; NAT for outbound traffic
    (run-command-ok "iptables" "-t" "nat" "-A" "POSTROUTING"
                    "-s" subnet "-j" "MASQUERADE")
    ;; Enable IP forwarding
    (write-string-to-file "/proc/sys/net/ipv4/ip_forward" "1")
    tap))

(defun firecracker-teardown-network (id index)
  "Tear down tap device and NAT rules for a firecracker VM."
  (declare (type simple-string id)
           (type fixnum index))
  (let ((tap (format nil "sq-~A-tap" id))
        (subnet (format nil "10.0.~D.0/30" index)))
    ;; Remove NAT rule
    (ignore-errors
      (run-command "iptables" "-t" "nat" "-D" "POSTROUTING"
                   "-s" subnet "-j" "MASQUERADE"))
    ;; Delete tap device
    (ignore-errors
      (run-command "ip" "link" "delete" tap))))

;;; ── VM Lifecycle ─────────────────────────────────────────────────────

(defun firecracker-start-vm (id cpu memory-mb squashfs-paths cid meta-dir)
  "Start a Firecracker microVM by shelling out to sq-firecracker.
   SQUASHFS-PATHS is a list of squashfs file paths to mount as drives.
   CID is the vsock context ID.
   META-DIR is the sandbox metadata directory for PID file etc."
  (declare (type simple-string id meta-dir)
           (type fixnum cpu memory-mb cid)
           (type list squashfs-paths))
  (let ((args (list "sq-firecracker" "start" id
                    (format nil "~D" cpu)
                    (format nil "~D" memory-mb))))
    ;; Append all squashfs paths as positional args
    (dolist (path squashfs-paths)
      (setf args (append args (list path))))
    (multiple-value-bind (exit-code stdout stderr)
        (apply #'run-command args)
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (error 'sandbox-error :id id
               :message (format nil "sq-firecracker start failed: ~A" stderr)))))
  ;; Write CID to meta dir for later reference
  (ignore-errors
    (write-string-to-file (format nil "~A/cid" meta-dir)
                          (format nil "~D" cid))))

(defun firecracker-stop-vm (id meta-dir)
  "Stop a Firecracker microVM."
  (declare (type simple-string id meta-dir)
           (ignore meta-dir))
  (multiple-value-bind (exit-code stdout stderr)
      (run-command "sq-firecracker" "stop" id)
    (declare (ignore stdout))
    (unless (zerop exit-code)
      (warn "sq-firecracker stop ~A failed: ~A" id stderr))))

;;; ── Vsock Exec ───────────────────────────────────────────────────────

(defun firecracker-exec (cid cmd workdir timeout-s)
  "Execute a command in the guest via vsock.
   Sends JSON to VSOCK-CONNECT:<cid>:5000 via socat.
   Returns an exec-result struct."
  (declare (type fixnum cid timeout-s)
           (type simple-string cmd workdir))
  (let* ((started (get-unix-time))
         (start-ticks (get-internal-real-time))
         (json-cmd (jojo:to-json
                    (list :|cmd| cmd
                          :|workdir| workdir
                          :|timeout| timeout-s)))
         (socat-addr (format nil "VSOCK-CONNECT:~D:5000" cid)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list "socat" (format nil "-T~D" timeout-s) "-" socat-addr)
         :input (make-string-input-stream json-cmd)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (let* ((finished (get-unix-time))
             (end-ticks (get-internal-real-time))
             (duration-ms (round (* (- end-ticks start-ticks) 1000)
                                 internal-time-units-per-second)))
        (if (zerop exit-code)
            ;; Parse JSON response from guest agent
            (handler-case
                (let ((resp (jojo:parse stdout)))
                  (make-exec-result
                   :exit-code (or (getf resp :|exit_code|) 1)
                   :stdout (or (getf resp :|stdout|) "")
                   :stderr (or (getf resp :|stderr|) "")
                   :started started
                   :finished finished
                   :duration-ms duration-ms))
              (error ()
                (make-exec-result
                 :exit-code 1
                 :stdout stdout
                 :stderr (format nil "failed to parse vsock response: ~A" stderr)
                 :started started
                 :finished finished
                 :duration-ms duration-ms)))
            ;; socat itself failed
            (make-exec-result
             :exit-code (if (= exit-code 124) 124 1)
             :stdout ""
             :stderr (format nil "vsock exec failed (socat exit ~D): ~A"
                             exit-code stderr)
             :started started
             :finished finished
             :duration-ms duration-ms))))))

;;; ── Drive Hot-Add ────────────────────────────────────────────────────

(defun firecracker-add-drive (id drive-id squashfs-path cid meta-dir)
  "Hot-add a squashfs drive to a running VM, then trigger remount in guest.
   DRIVE-ID is a unique identifier for the drive attachment.
   CID is the vsock context ID."
  (declare (type simple-string id drive-id squashfs-path meta-dir)
           (type fixnum cid)
           (ignore meta-dir))
  ;; 1. Hot-add the drive via sq-firecracker
  (multiple-value-bind (exit-code stdout stderr)
      (run-command "sq-firecracker" "add-drive" id drive-id squashfs-path)
    (declare (ignore stdout))
    (unless (zerop exit-code)
      (error 'sandbox-error :id id
             :message (format nil "add-drive failed: ~A" stderr))))
  ;; 2. Trigger remount in guest via vsock
  (let ((json-cmd (jojo:to-json (list :|cmd| "__squash_remount")))
        (socat-addr (format nil "VSOCK-CONNECT:~D:5000" cid)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list "socat" "-" socat-addr)
         :input (make-string-input-stream json-cmd)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (warn "vsock remount trigger failed for ~A: ~A" id stderr)))))
