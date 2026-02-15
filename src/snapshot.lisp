(in-package #:squashd)

;;; ── Snapshot/Restore ─────────────────────────────────────────────────
;;;
;;; Snapshot and restore operations are implemented as part of the
;;; manager (manager.lisp): manager-snapshot and manager-restore.
;;; They shell out to mksquashfs for creating snapshots and use
;;; the mounts.lisp mount operations for restore (remounting overlay
;;; with snapshot as an additional read-only layer).
;;;
;;; This file is reserved for future snapshot-specific helpers if needed.
