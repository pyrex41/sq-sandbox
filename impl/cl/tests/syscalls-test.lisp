(in-package #:squashd-tests)

;;; ── Helper Functions ─────────────────────────────────────────────────────

(defun unique-tmpdir ()
  "Generate a unique temporary directory path for testing"
  (format nil "/tmp/squashd-test-~A" (get-universal-time)))

(defun cleanup-mount (path)
  "Attempt to unmount and remove a test mount point, ignoring errors"
  (ignore-errors
    (squashd-syscalls::%umount2 path squashd-syscalls::+mnt-detach+))
  (ignore-errors
    (delete-file path))
  (ignore-errors
    (uiop:delete-empty-directory path)))

;;; ── Mount/Umount2 Tests ──────────────────────────────────────────────────

(test mount-tmpfs-success
  "Test mounting tmpfs filesystem succeeds"
  (let ((mount-point (unique-tmpdir)))
    (unwind-protect
         (progn
           ;; Create mount point directory
           (ensure-directories-exist mount-point)

           ;; Mount tmpfs
           (let ((result (squashd-syscalls::%mount "tmpfs" mount-point "tmpfs" 0 (cffi:null-pointer))))
             (is (zerop result)
                 "mount() should return 0 on success")

             ;; Verify mount is present by checking /proc/mounts
             (let ((mounts (uiop:read-file-string "/proc/mounts")))
               (is (search mount-point mounts)
                   "Mount point should appear in /proc/mounts"))))
      ;; Cleanup
      (cleanup-mount mount-point))))

(test mount-with-flags
  "Test mounting tmpfs with specific flags (nodev, nosuid, noexec)"
  (let ((mount-point (unique-tmpdir)))
    (unwind-protect
         (progn
           (ensure-directories-exist mount-point)

           ;; Mount with combined flags
           (let* ((flags (logior squashd-syscalls::+ms-nodev+
                                 squashd-syscalls::+ms-nosuid+
                                 squashd-syscalls::+ms-noexec+))
                  (result (squashd-syscalls::%mount "tmpfs" mount-point "tmpfs" flags (cffi:null-pointer))))
             (is (zerop result)
                 "mount() with flags should return 0 on success")))
      (cleanup-mount mount-point))))

(test umount2-success
  "Test unmounting filesystem succeeds"
  (let ((mount-point (unique-tmpdir)))
    (unwind-protect
         (progn
           (ensure-directories-exist mount-point)

           ;; Mount first
           (squashd-syscalls::%mount "tmpfs" mount-point "tmpfs" 0 (cffi:null-pointer))

           ;; Unmount
           (let ((result (squashd-syscalls::%umount2 mount-point 0)))
             (is (zerop result)
                 "umount2() should return 0 on success")

             ;; Verify mount is gone
             (let ((mounts (uiop:read-file-string "/proc/mounts")))
               (is (not (search mount-point mounts))
                   "Mount point should not appear in /proc/mounts after umount"))))
      (cleanup-mount mount-point))))

(test umount2-detach
  "Test lazy unmount with MNT_DETACH flag"
  (let ((mount-point (unique-tmpdir)))
    (unwind-protect
         (progn
           (ensure-directories-exist mount-point)
           (squashd-syscalls::%mount "tmpfs" mount-point "tmpfs" 0 (cffi:null-pointer))

           ;; Unmount with detach flag
           (let ((result (squashd-syscalls::%umount2 mount-point squashd-syscalls::+mnt-detach+)))
             (is (zerop result)
                 "umount2() with MNT_DETACH should return 0 on success")))
      (cleanup-mount mount-point))))

(test mount-invalid-path-errno
  "Test mount() to invalid path sets errno"
  (let ((invalid-path "/nonexistent/mount/point/that/does/not/exist"))
    (let ((result (squashd-syscalls::%mount "tmpfs" invalid-path "tmpfs" 0 (cffi:null-pointer))))
      (is (= result -1)
          "mount() to invalid path should return -1")
      (is (plusp (squashd-syscalls::get-errno))
          "errno should be set (ENOENT=2 or similar)"))))

(test umount2-nonexistent-errno
  "Test umount2() on nonexistent path sets errno"
  (let ((result (squashd-syscalls::%umount2 "/nonexistent/mount" 0)))
    (is (= result -1)
        "umount2() on nonexistent mount should return -1")
    (is (plusp (squashd-syscalls::get-errno))
        "errno should be set (EINVAL=22 or ENOENT=2)")))

;;; ── Fork Tests ───────────────────────────────────────────────────────────

(test fork-returns-valid-pid
  "Test fork() returns valid PID in parent process"
  (let ((pid (squashd-syscalls::%fork)))
    (cond
      ((> pid 0)
       ;; Parent process
       (is (plusp pid)
           "fork() should return positive PID in parent")

       ;; Wait for child to exit
       (cffi:with-foreign-object (status :int)
         (squashd-syscalls::%waitpid pid status 0)))

      ((zerop pid)
       ;; Child process - exit immediately
       (squashd-syscalls::%exit 0))

      (t
       ;; Fork failed
       (fail "fork() should not return negative value on success")))))

(test fork-child-has-different-pid
  "Test child process has different PID from parent"
  (let ((parent-pid (squashd-syscalls::%getpid))
        (fork-result (squashd-syscalls::%fork)))
    (cond
      ((> fork-result 0)
       ;; Parent: wait for child
       (cffi:with-foreign-object (status :int)
         (squashd-syscalls::%waitpid fork-result status 0)))

      ((zerop fork-result)
       ;; Child: verify PID is different
       (let ((child-pid (squashd-syscalls::%getpid)))
         (when (/= child-pid parent-pid)
           (squashd-syscalls::%exit 0))
         ;; If PIDs are same (impossible), exit with error
         (squashd-syscalls::%exit 1)))

      (t
       (fail "fork() failed")))))

(test fork-child-exit-status
  "Test waitpid() correctly captures child exit status"
  (let ((pid (squashd-syscalls::%fork)))
    (cond
      ((> pid 0)
       ;; Parent: wait and check exit status
       (cffi:with-foreign-object (status :int)
         (let ((wait-result (squashd-syscalls::%waitpid pid status 0)))
           (is (= wait-result pid)
               "waitpid() should return child PID")
           ;; Check if exited normally (WIFEXITED macro: status & 0x7F == 0)
           ;; and exit code is 42 (WEXITSTATUS macro: (status >> 8) & 0xFF)
           (let ((raw-status (cffi:mem-ref status :int)))
             (is (zerop (logand raw-status #x7F))
                 "Child should exit normally")
             (is (= 42 (logand (ash raw-status -8) #xFF))
                 "Child exit code should be 42")))))

      ((zerop pid)
       ;; Child: exit with code 42
       (squashd-syscalls::%exit 42))

      (t
       (fail "fork() failed")))))

;;; ── Pipe/Read/Write Tests ────────────────────────────────────────────────

(test pipe-creation-success
  "Test pipe() creates valid file descriptors"
  (cffi:with-foreign-object (fds :int 2)
    (let ((result (squashd-syscalls::%pipe fds)))
      (is (zerop result)
          "pipe() should return 0 on success")

      (let ((read-fd (cffi:mem-aref fds :int 0))
            (write-fd (cffi:mem-aref fds :int 1)))
        (is (>= read-fd 0)
            "Read fd should be non-negative")
        (is (>= write-fd 0)
            "Write fd should be non-negative")
        (is (/= read-fd write-fd)
            "Read and write fds should be different")

        ;; Cleanup
        (squashd-syscalls::%close read-fd)
        (squashd-syscalls::%close write-fd)))))

(test pipe-write-read-data-transfer
  "Test write() and read() successfully transfer data through pipe"
  (cffi:with-foreign-objects ((fds :int 2)
                               (write-buf :char 13)
                               (read-buf :char 13))
    ;; Create pipe
    (squashd-syscalls::%pipe fds)

    (let ((read-fd (cffi:mem-aref fds :int 0))
          (write-fd (cffi:mem-aref fds :int 1))
          (test-string "Hello, pipe!"))

      (unwind-protect
           (progn
             ;; Write test string to write end
             (loop for i from 0 below (length test-string)
                   do (setf (cffi:mem-aref write-buf :char i)
                            (char-code (char test-string i))))
             (setf (cffi:mem-aref write-buf :char (length test-string)) 0)

             (let ((bytes-written (squashd-syscalls::%sys-write write-fd write-buf (length test-string))))
               (is (= bytes-written (length test-string))
                   "write() should return number of bytes written")

               ;; Read from read end
               (let ((bytes-read (squashd-syscalls::%read read-fd read-buf 13)))
                 (is (= bytes-read (length test-string))
                     "read() should return number of bytes read")

                 ;; Verify data matches
                 (let ((read-string (cffi:foreign-string-to-lisp read-buf :count bytes-read)))
                   (is (string= read-string test-string)
                       "Data read should match data written")))))

        ;; Cleanup
        (squashd-syscalls::%close read-fd)
        (squashd-syscalls::%close write-fd)))))

(test pipe-read-blocks-until-data
  "Test read() on empty pipe returns 0 when write end closed"
  (cffi:with-foreign-objects ((fds :int 2)
                               (read-buf :char 10))
    (squashd-syscalls::%pipe fds)

    (let ((read-fd (cffi:mem-aref fds :int 0))
          (write-fd (cffi:mem-aref fds :int 1)))

      (unwind-protect
           (progn
             ;; Close write end immediately
             (squashd-syscalls::%close write-fd)

             ;; Reading should return 0 (EOF)
             (let ((bytes-read (squashd-syscalls::%read read-fd read-buf 10)))
               (is (zerop bytes-read)
                   "read() should return 0 when write end is closed and pipe is empty")))

        ;; Cleanup
        (ignore-errors (squashd-syscalls::%close read-fd))))))

(test pipe-multiple-writes-reads
  "Test multiple write/read operations on same pipe"
  (cffi:with-foreign-objects ((fds :int 2)
                               (buf :char 5))
    (squashd-syscalls::%pipe fds)

    (let ((read-fd (cffi:mem-aref fds :int 0))
          (write-fd (cffi:mem-aref fds :int 1)))

      (unwind-protect
           (progn
             ;; Write "ABC"
             (loop for c in '(#\A #\B #\C)
                   for i from 0
                   do (setf (cffi:mem-aref buf :char i) (char-code c)))
             (squashd-syscalls::%sys-write write-fd buf 3)

             ;; Read "ABC"
             (squashd-syscalls::%read read-fd buf 3)
             (is (char= (code-char (cffi:mem-aref buf :char 0)) #\A))
             (is (char= (code-char (cffi:mem-aref buf :char 1)) #\B))
             (is (char= (code-char (cffi:mem-aref buf :char 2)) #\C))

             ;; Write "XYZ"
             (loop for c in '(#\X #\Y #\Z)
                   for i from 0
                   do (setf (cffi:mem-aref buf :char i) (char-code c)))
             (squashd-syscalls::%sys-write write-fd buf 3)

             ;; Read "XYZ"
             (squashd-syscalls::%read read-fd buf 3)
             (is (char= (code-char (cffi:mem-aref buf :char 0)) #\X))
             (is (char= (code-char (cffi:mem-aref buf :char 1)) #\Y))
             (is (char= (code-char (cffi:mem-aref buf :char 2)) #\Z)))

        ;; Cleanup
        (squashd-syscalls::%close read-fd)
        (squashd-syscalls::%close write-fd)))))

;;; ── Errno Tests ──────────────────────────────────────────────────────────

(test errno-read-invalid-fd
  "Test read() with invalid fd sets errno"
  (cffi:with-foreign-object (buf :char 10)
    (let ((result (squashd-syscalls::%read -1 buf 10)))
      (is (= result -1)
          "read() with invalid fd should return -1")
      (is (plusp (squashd-syscalls::get-errno))
          "errno should be set (EBADF=9)"))))

(test errno-write-invalid-fd
  "Test write() with invalid fd sets errno"
  (cffi:with-foreign-object (buf :char 10)
    (let ((result (squashd-syscalls::%sys-write -1 buf 10)))
      (is (= result -1)
          "write() with invalid fd should return -1")
      (is (plusp (squashd-syscalls::get-errno))
          "errno should be set (EBADF=9)"))))

(test errno-close-invalid-fd
  "Test close() with invalid fd sets errno"
  (let ((result (squashd-syscalls::%close -1)))
    (is (= result -1)
        "close() with invalid fd should return -1")
    (is (plusp (squashd-syscalls::get-errno))
        "errno should be set (EBADF=9)")))

(test errno-pipe-invalid-pointer
  "Test pipe() with NULL pointer sets errno"
  (let ((result (squashd-syscalls::%pipe (cffi:null-pointer))))
    (is (= result -1)
        "pipe() with NULL pointer should return -1")
    (is (plusp (squashd-syscalls::get-errno))
        "errno should be set (EFAULT=14)")))

;;; ── Integration Tests ────────────────────────────────────────────────────

(test fork-pipe-ipc
  "Integration test: fork with pipe for inter-process communication"
  (cffi:with-foreign-objects ((fds :int 2)
                               (write-buf :char 6)
                               (read-buf :char 6))
    ;; Create pipe before fork
    (squashd-syscalls::%pipe fds)

    (let ((read-fd (cffi:mem-aref fds :int 0))
          (write-fd (cffi:mem-aref fds :int 1))
          (pid (squashd-syscalls::%fork)))

      (cond
        ((> pid 0)
         ;; Parent process
         (unwind-protect
              (progn
                ;; Close write end in parent
                (squashd-syscalls::%close write-fd)

                ;; Read message from child
                (let ((bytes-read (squashd-syscalls::%read read-fd read-buf 6)))
                  (is (= bytes-read 5)
                      "Should read 5 bytes from child")

                  (let ((msg (cffi:foreign-string-to-lisp read-buf :count 5)))
                    (is (string= msg "child")
                        "Should receive 'child' message from child process")))

                ;; Wait for child
                (cffi:with-foreign-object (status :int)
                  (squashd-syscalls::%waitpid pid status 0)))

           ;; Cleanup
           (squashd-syscalls::%close read-fd)))

        ((zerop pid)
         ;; Child process
         ;; Close read end in child
         (squashd-syscalls::%close read-fd)

         ;; Write message to parent
         (loop for c in '(#\c #\h #\i #\l #\d)
               for i from 0
               do (setf (cffi:mem-aref write-buf :char i) (char-code c)))
         (squashd-syscalls::%sys-write write-fd write-buf 5)

         ;; Close write end and exit
         (squashd-syscalls::%close write-fd)
         (squashd-syscalls::%exit 0))

        (t
         (fail "fork() failed"))))))

;;; ── Test Runner ──────────────────────────────────────────────────────────

(defun run-tests ()
  "Run all squashd syscall tests"
  (run! 'squashd-tests))
