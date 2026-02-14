(in-package #:squashd)

;;; ── Background reaper ─────────────────────────────────────────────────
;;;
;;; Runs in a dedicated thread, checking every 10 seconds for sandboxes
;;; that have exceeded their max-lifetime-s. Each sandbox destroy is
;;; wrapped in handler-case for per-sandbox error isolation — a single
;;; failed destroy must never crash the reaper loop.
;;;
;;; COLD PATH — runs every 10 seconds. Maximize safety for correctness.

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defun find-expired-sandboxes (manager)
  "Return a list of sandbox IDs whose max-lifetime-s has been exceeded.
   Holds the manager lock only for the scan, not for destruction."
  (let ((now (get-unix-time))
        (expired nil))
    (bt:with-lock-held ((manager-lock manager))
      (maphash (lambda (id sandbox)
                 (let ((max-life (sandbox-max-lifetime-s sandbox)))
                   (when (and (plusp max-life)
                              (> (- now (sandbox-created sandbox)) max-life))
                     (push id expired))))
               (manager-sandboxes manager)))
    expired))

(defun reaper-loop (manager)
  "Main reaper loop. Runs forever, sleeping 10 seconds between cycles.
   Each sandbox destruction is individually error-isolated."
  (log:info "reaper: started")
  (loop
    (sleep 10)
    (handler-case
        (let ((expired (find-expired-sandboxes manager)))
          (dolist (id expired)
            (handler-case
                (progn
                  (log:info "reaper: destroying expired sandbox ~A" id)
                  (manager-destroy-sandbox manager id))
              (error (e)
                (log:warn "reaper: failed to destroy ~A: ~A" id e)))))
      ;; Catch any error in the outer scan itself so the loop never dies
      (error (e)
        (log:error "reaper: unexpected error in scan cycle: ~A" e)))))
