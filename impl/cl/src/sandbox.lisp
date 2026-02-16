(in-package #:squashd)

;;; ── Sandbox Data Structure ──────────────────────────────────────────
;;;
;;; Core sandbox struct representing a running sandbox instance.
;;; Uses defstruct for performance — direct slot access, no CLOS dispatch.
;;;
;;; The sandbox struct is created during sandbox creation and stored in
;;; the manager's hash-table. Its slots are accessed by exec.lisp (for
;;; fork/execve), reaper.lisp (for lifetime checks), and api.lisp (for
;;; JSON serialization).
;;;
;;; WARM PATH — accessed on every API call and exec.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; ── State type ──────────────────────────────────────────────────────

(deftype sandbox-state ()
  "Valid states for a sandbox lifecycle."
  '(member :creating :ready :destroying :destroyed))

;;; ── Sandbox struct ──────────────────────────────────────────────────

(defstruct (sandbox (:constructor %make-sandbox))
  (id             ""        :type simple-string)
  (state          :creating :type sandbox-state)
  (mounts         nil       :type (or null sandbox-mounts))
  (netns          nil       :type (or null netns-handle))
  (cgroup         nil       :type (or null cgroup-handle))
  (created        0         :type (unsigned-byte 64))
  (last-active    0         :type (unsigned-byte 64))
  (exec-count     0         :type fixnum)
  (max-lifetime-s 0         :type fixnum)
  (lock           (bt:make-lock "sandbox") :type t))

;;; ── Resolv.conf seeding ────────────────────────────────────────────
;;;
;;; After netns creation, seed /etc/resolv.conf in the sandbox's overlay
;;; upper layer so DNS queries route through the netns gateway (which
;;; DNATs to the host's real nameserver).

(defun seed-resolv-conf (sandbox-dir netns)
  "Write /etc/resolv.conf into the sandbox upper layer.
   Points DNS at the netns gateway IP (10.200.<index>.1) which is
   DNATed to the host nameserver by setup-netns.
   SANDBOX-DIR is the sandbox's data directory.
   NETNS is a netns-handle (or NIL to skip)."
  (when netns
    (let* ((etc-dir (format nil "~A/upper/data/etc" sandbox-dir))
           (resolv-path (format nil "~A/resolv.conf" etc-dir))
           (gateway (format nil "10.200.~D.1" (netns-handle-index netns))))
      (ensure-directories-exist (format nil "~A/" etc-dir))
      (write-string-to-file resolv-path
                            (format nil "nameserver ~A~%" gateway)))))

;;; ── Sandbox destruction ─────────────────────────────────────────────
;;;
;;; Tears down all sandbox resources in reverse creation order.
;;; Each step is wrapped in ignore-errors — cleanup must be best-effort.

(defun destroy-sandbox (sandbox)
  "Destroy all resources associated with SANDBOX.
   Unmounts filesystems, tears down netns, removes cgroup.
   Each step is individually error-isolated.
   Sets state to :destroyed when complete."
  (setf (sandbox-state sandbox) :destroying)

  ;; 1. Tear down mounts (overlay → snapshot → tmpfs → squashfs in reverse)
  (when (sandbox-mounts sandbox)
    (ignore-errors (destroy-sandbox-mounts (sandbox-mounts sandbox))))

  ;; 2. Tear down network namespace
  (when (sandbox-netns sandbox)
    (ignore-errors (teardown-netns (sandbox-netns sandbox))))

  ;; 3. Remove cgroup
  (when (sandbox-cgroup sandbox)
    (ignore-errors (destroy-cgroup (sandbox-cgroup sandbox))))

  (setf (sandbox-state sandbox) :destroyed)
  sandbox)
