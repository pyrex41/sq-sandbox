(in-package #:squashd)

;;; ── Module Management ───────────────────────────────────────────────
;;;
;;; Modules are squashfs images stored in the modules directory.
;;; Convention: NNN-name.squashfs where NNN is sort order
;;;   000 = base, 1xx = runtime, 2xx = service, 9xx = checkpoint
;;;
;;; WARM PATH — module listing called from API.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

(defun count-modules (config)
  "Count the number of .squashfs module files in the modules directory."
  (let ((dir (modules-dir config)))
    (if (probe-file dir)
        (length (directory (make-pathname :directory (pathname-directory
                                                      (pathname dir))
                                          :name :wild
                                          :type "squashfs")))
        0)))

(defun base-module-exists-p (config)
  "Return T if the base module (000-base-alpine.squashfs) exists."
  (and (probe-file (format nil "~A/000-base-alpine.squashfs"
                           (modules-dir config)))
       t))

(defun list-available-modules (config)
  "Return a list of module info plists suitable for JSON serialization.
   Each entry has :name, :size, :location keys."
  (let ((dir (modules-dir config))
        (result nil))
    (when (probe-file dir)
      (dolist (path (directory (make-pathname
                                :directory (pathname-directory
                                            (pathname dir))
                                :name :wild
                                :type "squashfs")))
        (let* ((namestring (namestring path))
               (basename (pathname-name path))
               (size (handler-case
                         (with-open-file (s namestring :element-type '(unsigned-byte 8))
                           (file-length s))
                       (error () 0))))
          (push (list :|name| basename
                      :|size| size
                      :|location| "local")
                result))))
    ;; Return sorted by name
    (sort result #'string< :key (lambda (m) (getf m :|name|)))))

(defun module-exists-p (config module-name)
  "Return T if a module with MODULE-NAME exists locally."
  (and (probe-file (format nil "~A/~A.squashfs"
                           (modules-dir config) module-name))
       t))
