(in-package #:squashd-test)

(def-suite exec-tests
  :description "Tests for sandbox command execution and output capture"
  :in :squashd-test)

(in-suite exec-tests)

;;; ── Test Fixtures ─────────────────────────────────────────────────────
;;;
;;; Note: The real sandbox struct is defined in sandbox.lisp (task 5).
;;; For now, we test the standalone functions and document integration
;;; tests that should be run once task 5 is complete.

(defun make-test-merged-dir ()
  "Create a test overlay merged directory for log testing."
  (let* ((test-dir (format nil "/tmp/squashd-test-~A" (random 999999)))
         (merged-path (format nil "~A/merged" test-dir))
         (log-dir (format nil "~A/.meta/log" merged-path)))

    ;; Create directory structure
    (ensure-directories-exist (format nil "~A/" log-dir))

    (list :test-dir test-dir :merged-path merged-path)))

(defun cleanup-test-dir (test-info)
  "Clean up test directory."
  (let ((test-dir (getf test-info :test-dir)))
    (when (and test-dir (probe-file test-dir))
      (ignore-errors
        (uiop:run-program (list "rm" "-rf" test-dir))))))

(defmacro with-test-merged-dir ((var) &body body)
  "Execute body with a test merged directory, ensuring cleanup."
  `(let ((,var (make-test-merged-dir)))
     (unwind-protect
          (progn ,@body)
       (cleanup-test-dir ,var))))

;;; ── Helper Function Tests ────────────────────────────────────────────

(test get-unix-time-test
  "Test that get-unix-time returns a reasonable timestamp"
  (let ((t1 (squashd::get-unix-time))
        (t2 (squashd::get-unix-time)))
    (is (typep t1 '(unsigned-byte 64)))
    (is (>= t2 t1))
    ;; Should be sometime after 2020 (unix timestamp > 1577836800)
    (is (> t1 1577836800))))

(test get-monotonic-ms-test
  "Test monotonic clock returns increasing values"
  (let ((t1 (squashd::get-monotonic-ms))
        (t2 (squashd::get-monotonic-ms)))
    (is (typep t1 'fixnum))
    (is (>= t2 t1))))

(test exec-result-struct-test
  "Test exec-result struct creation and accessors"
  (let ((result (squashd::make-exec-result
                 :exit-code 0
                 :stdout "hello"
                 :stderr "world"
                 :started 100
                 :finished 200
                 :seq 1)))
    (is (= 0 (squashd::exec-result-exit-code result)))
    (is (string= "hello" (squashd::exec-result-stdout result)))
    (is (string= "world" (squashd::exec-result-stderr result)))
    (is (= 100 (squashd::exec-result-started result)))
    (is (= 200 (squashd::exec-result-finished result)))
    (is (= 1 (squashd::exec-result-seq result)))))

;;; ── Pipe Tests ───────────────────────────────────────────────────────

(test make-pipe-test
  "Test pipe creation returns two valid file descriptors"
  (multiple-value-bind (read-fd write-fd)
      (squashd::make-pipe)
    (is (typep read-fd 'fixnum))
    (is (typep write-fd 'fixnum))
    (is (>= read-fd 0))
    (is (>= write-fd 0))
    (is (/= read-fd write-fd))
    ;; Clean up
    (squashd::close-fd-safe read-fd)
    (squashd::close-fd-safe write-fd)))

(test close-fd-safe-test
  "Test close-fd-safe handles valid and invalid fds"
  ;; Should not error on -1
  (finishes (squashd::close-fd-safe -1))

  ;; Should close a valid fd
  (multiple-value-bind (read-fd write-fd)
      (squashd::make-pipe)
    (finishes (squashd::close-fd-safe read-fd))
    (finishes (squashd::close-fd-safe write-fd))))

;;; ── Exec Log Tests ───────────────────────────────────────────────────

(test write-exec-log-spec
  "SPEC: Test exec log writing

   This test documents the expected behavior. Full testing requires
   a real sandbox struct from task 5.

   The test should:
   1. Create a sandbox with .meta/log directory
   2. Call write-exec-log with test data
   3. Verify JSON file is created with correct structure
   4. Verify all fields match input data"
  (skip "Requires real sandbox from task 5"))

;;; ── Mock Sandbox for Exec Tests ──────────────────────────────────────
;;;
;;; These tests require a more complete mock that works with exec-in-sandbox.
;;; Since exec-in-sandbox uses sandbox accessors (sandbox-netns, sandbox-cgroup,
;;; sandbox-mounts, sandbox-exec-count), we need to create a struct that
;;; provides these.
;;;
;;; For now, we document the test cases that SHOULD be written once the
;;; sandbox struct is available from task 5.

;;; ── Integration Test Documentation ───────────────────────────────────
;;;
;;; The following tests should be run in a privileged container with
;;; actual sandbox creation from task 5. They are documented here as
;;; specifications.

(test exec-simple-command-spec
  "SPEC: exec-in-sandbox should run simple commands and return results

   Test plan:
   1. Create a real sandbox with base layer mounted
   2. Call (exec-in-sandbox sb \"echo hello\")
   3. Verify:
      - exit-code is 0
      - stdout contains \"hello\"
      - stderr is empty
      - started/finished timestamps are set
      - seq is incremented
      - log file exists at .meta/log/exec-1.json"
  (skip "Requires real sandbox from task 5"))

(test exec-exit-codes-spec
  "SPEC: exec-in-sandbox should propagate exit codes correctly

   Test plan:
   1. Run command that exits 0: \"true\"
   2. Run command that exits 1: \"false\"
   3. Run command that exits 42: \"sh -c 'exit 42'\"
   4. Verify each exit code is captured correctly"
  (skip "Requires real sandbox from task 5"))

(test exec-stdout-stderr-separation-spec
  "SPEC: exec-in-sandbox should capture stdout and stderr separately

   Test plan:
   1. Run: \"sh -c 'echo out1; echo err1 >&2; echo out2; echo err2 >&2'\"
   2. Verify:
      - stdout contains \"out1\" and \"out2\" in order
      - stderr contains \"err1\" and \"err2\" in order
      - stdout does not contain err1/err2
      - stderr does not contain out1/out2"
  (skip "Requires real sandbox from task 5"))

(test exec-timeout-kills-process-spec
  "SPEC: exec-in-sandbox should kill process on timeout with exit code 124

   Test plan:
   1. Run command that sleeps forever: \"sleep 999999\"
   2. Set timeout to 1 second
   3. Verify:
      - Function returns in ~1 second (not 999999)
      - exit-code is 124
      - Process is killed (check via ps or /proc)"
  (skip "Requires real sandbox from task 5"))

(test exec-timeout-boundary-spec
  "SPEC: exec-in-sandbox should handle commands that complete just before timeout

   Test plan:
   1. Run \"sleep 0.5\" with timeout 1
   2. Verify:
      - exit-code is 0 (not 124)
      - No SIGKILL sent
   3. Run \"sleep 1.5\" with timeout 1
   4. Verify:
      - exit-code is 124
      - Process killed"
  (skip "Requires real sandbox from task 5"))

(test exec-signal-exit-codes-spec
  "SPEC: exec-in-sandbox should return 128+signal for killed processes

   Test plan:
   1. Run command that triggers SIGSEGV (if possible in sandbox)
   2. Verify exit-code is 128 + signal number
   3. Run command killed by SIGTERM
   4. Verify exit-code is 128 + 15 = 143"
  (skip "Requires real sandbox from task 5"))

(test exec-large-output-spec
  "SPEC: exec-in-sandbox should handle output up to +max-output+ (65536 bytes)

   Test plan:
   1. Run command that generates 1KB output
   2. Verify all output captured
   3. Run command that generates 100KB output
   4. Verify output truncated at 65536 bytes
   5. Verify no crash or hang"
  (skip "Requires real sandbox from task 5"))

(test exec-namespace-isolation-spec
  "SPEC: exec-in-sandbox should enforce namespace isolation

   Test plan:
   1. Create sandbox with network namespace
   2. Run \"ip addr\" inside sandbox
   3. Verify output shows sandbox network, not host network
   4. Create sandbox without network namespace
   5. Run \"ip addr\" inside sandbox
   6. Verify isolation is correct for that config"
  (skip "Requires real sandbox from task 5 and network namespace"))

(test exec-cgroup-enforcement-spec
  "SPEC: exec-in-sandbox should place child in cgroup

   Test plan:
   1. Create sandbox with cgroup limits
   2. Run command inside sandbox
   3. Check /sys/fs/cgroup/squashd-<id>/cgroup.procs during execution
   4. Verify child PID is in the cgroup
   5. Verify resource limits are enforced"
  (skip "Requires real sandbox from task 5 and cgroup"))

(test exec-chroot-isolation-spec
  "SPEC: exec-in-sandbox should chroot into overlay merged directory

   Test plan:
   1. Create sandbox with specific layers
   2. Create a unique file in overlay upper dir
   3. Run \"ls /\" inside sandbox
   4. Verify sandbox sees overlay filesystem, not host filesystem
   5. Verify unique file is visible"
  (skip "Requires real sandbox from task 5"))

(test exec-workdir-spec
  "SPEC: exec-in-sandbox should chdir to workdir before executing

   Test plan:
   1. Create sandbox with /app directory
   2. Run \"pwd\" with workdir=\"/app\"
   3. Verify output is \"/app\"
   4. Run \"pwd\" with workdir=\"/tmp\"
   5. Verify output is \"/tmp\"
   6. Run with invalid workdir
   7. Verify appropriate error (exit code 125 or similar)"
  (skip "Requires real sandbox from task 5"))

(test exec-concurrent-spec
  "SPEC: exec-in-sandbox should handle concurrent executions in same sandbox

   Test plan:
   1. Create one sandbox
   2. Launch 5 concurrent exec-in-sandbox calls with different commands
   3. Verify:
      - All complete successfully
      - exec-count increments correctly (should be 5)
      - Each gets unique seq number (1-5)
      - All log files written correctly"
  (skip "Requires real sandbox from task 5"))

(test exec-log-sequence-spec
  "SPEC: exec-in-sandbox should increment sequence for each exec

   Test plan:
   1. Create sandbox
   2. Run 3 commands in sequence
   3. Verify:
      - First result has seq=1, second seq=2, third seq=3
      - Log files exec-1.json, exec-2.json, exec-3.json exist
      - Each log has correct seq field"
  (skip "Requires real sandbox from task 5"))

(test exec-after-sandbox-destroy-spec
  "SPEC: exec-in-sandbox should fail gracefully on destroyed sandbox

   Test plan:
   1. Create sandbox
   2. Destroy sandbox (unmount everything)
   3. Try to exec in destroyed sandbox
   4. Verify appropriate error (sandbox-error or similar)
   5. Verify no process leaks or zombie processes"
  (skip "Requires real sandbox from task 5"))

(test exec-binary-output-spec
  "SPEC: exec-in-sandbox should handle binary output gracefully

   Test plan:
   1. Run command that outputs binary data (e.g., cat /bin/sh)
   2. Verify:
      - No crash
      - Output captured (may be garbled UTF-8)
      - exit-code correct"
  (skip "Requires real sandbox from task 5"))

(test exec-empty-command-spec
  "SPEC: exec-in-sandbox should handle edge case commands

   Test plan:
   1. Run with cmd=\"\" (empty string)
   2. Verify appropriate error or exit code
   3. Run with cmd containing only whitespace
   4. Verify behavior is reasonable"
  (skip "Requires real sandbox from task 5"))

(test exec-shell-metacharacters-spec
  "SPEC: exec-in-sandbox should handle shell metacharacters via /bin/sh -c

   Test plan:
   1. Run \"echo foo | wc -l\"
   2. Verify pipe works (output is \"1\")
   3. Run \"echo $PATH\"
   4. Verify shell variable expansion works
   5. Run \"echo foo && echo bar\"
   6. Verify both commands run"
  (skip "Requires real sandbox from task 5"))

;;; ── Test Helper for Manual/Integration Testing ──────────────────────

(defun run-exec-integration-tests ()
  "Helper function to run exec tests that require privileged container.

   This should be called after:
   1. Docker container is running with --privileged
   2. Sandbox struct is implemented (task 5)
   3. A test sandbox is created with real mounts

   Usage:
     (in-package :squashd-test)
     (run-exec-integration-tests)"
  (format t "~%Integration tests require:~%")
  (format t "  1. Running in privileged container~%")
  (format t "  2. Sandbox struct from task 5~%")
  (format t "  3. Real filesystem mounts~%~%")
  (format t "Run with: (fiveam:run! 'exec-tests)~%"))

;;; ── Immediate Tests (No Sandbox Required) ────────────────────────────
;;; These tests can run right now without task 5 being complete.

(test pipe-write-read-test
  "Test that we can write to pipe and read from it"
  (multiple-value-bind (read-fd write-fd)
      (squashd::make-pipe)
    (unwind-protect
         (let ((test-data "hello pipe"))
           ;; Write to pipe
           (cffi:with-foreign-string (buf test-data)
             (let ((written (squashd::%sys-write write-fd buf (length test-data))))
               (is (= written (length test-data)))))

           ;; Read from pipe
           (cffi:with-foreign-object (buf :char 256)
             (let ((nread (squashd::%read read-fd buf 256)))
               (is (= nread (length test-data)))
               (let ((read-str (cffi:foreign-string-to-lisp buf :count nread)))
                 (is (string= test-data read-str))))))
      ;; Cleanup
      (squashd::close-fd-safe read-fd)
      (squashd::close-fd-safe write-fd))))

(test constants-test
  "Verify exec constants are defined correctly"
  (is (= 1 squashd::+stdout-fileno+))
  (is (= 2 squashd::+stderr-fileno+))
  (is (= 65536 squashd::+max-output+))
  (is (= 100 squashd::+poll-interval-ms+)))
