(in-package #:squashd)

;;; Network namespace setup.
;;; Creates isolated network namespaces with veth pairs, NAT, DNS forwarding,
;;; and optional egress filtering. All operations shell out to ip(8) and
;;; iptables(8) via run-command/run-command-ok from syscalls.lisp.

(declaim (optimize (speed 2) (safety 2)))

;;; ── Data Structure ─────────────────────────────────────────────────────

(defstruct netns-handle
  (name       "" :type simple-string)
  (index      0  :type (unsigned-byte 8))
  (veth-host  "" :type simple-string)
  (chain-name nil :type (or null simple-string))
  (host-dns   nil :type (or null simple-string)))

;;; ── Index Allocation ───────────────────────────────────────────────────
;;;
;;; Each sandbox gets a unique index 1-254 for its /30 subnet.
;;; 10.200.<index>.0/30 gives: .1 = host, .2 = sandbox.

(defvar *netns-index-lock* (bt:make-lock "netns-index"))
(defvar *netns-index-bitmap* (make-array 256 :element-type 'bit :initial-element 0)
  "Bit vector tracking allocated subnet indices (1-254).")

(defun allocate-netns-index ()
  "Allocate a free subnet index (1-254). Signals sandbox-error if exhausted."
  (bt:with-lock-held (*netns-index-lock*)
    (loop for i from 1 below 255
          when (zerop (aref *netns-index-bitmap* i))
            do (setf (aref *netns-index-bitmap* i) 1)
               (return i)
          finally (error 'sandbox-error
                         :id "netns"
                         :message "no free netns indices (255 sandboxes active)"))))

(defun release-netns-index (index)
  "Release a previously allocated subnet index."
  (when (and index (plusp index) (< index 255))
    (bt:with-lock-held (*netns-index-lock*)
      (setf (aref *netns-index-bitmap* index) 0))))

(defun reserve-netns-index (index)
  "Mark an existing subnet index as allocated (used during recovery)."
  (when (and index (plusp index) (< index 255))
    (bt:with-lock-held (*netns-index-lock*)
      (setf (aref *netns-index-bitmap* index) 1))))

;;; ── Helpers ────────────────────────────────────────────────────────────

(defun parse-first-nameserver (resolv-conf-path)
  "Parse the first 'nameserver X.X.X.X' line from resolv.conf.
   Returns the IP string or NIL."
  (ignore-errors
    (with-open-file (s resolv-conf-path :direction :input :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              when (and (> (length line) 11)
                        (string= "nameserver " line :end2 11))
                do (return (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (subseq line 11))))))))

;;; ── Egress Filtering ───────────────────────────────────────────────────

(defun apply-egress-rules (id veth-host allow-net)
  "Create an iptables chain for egress filtering on VETH-HOST.
   ALLOW-NET is a list of allowed CIDR strings (e.g. (\"10.0.0.0/8\" \"8.8.8.8/32\")).
   Returns the chain name."
  (declare (type simple-string id veth-host)
           (type list allow-net))
  (let ((chain (format nil "SQ-~A" id)))
    ;; Create the chain
    (run-command-ok "iptables" "-N" chain)

    ;; Allow established/related connections back
    (run-command-ok "iptables" "-A" chain
                    "-m" "conntrack" "--ctstate" "ESTABLISHED,RELATED"
                    "-j" "ACCEPT")

    ;; Allow DNS (UDP + TCP port 53) — always needed for resolution
    (run-command-ok "iptables" "-A" chain "-p" "udp" "--dport" "53" "-j" "ACCEPT")
    (run-command-ok "iptables" "-A" chain "-p" "tcp" "--dport" "53" "-j" "ACCEPT")

    ;; Allow each specified CIDR
    (dolist (cidr allow-net)
      (run-command-ok "iptables" "-A" chain "-d" cidr "-j" "ACCEPT"))

    ;; Default drop everything else
    (run-command-ok "iptables" "-A" chain "-j" "DROP")

    ;; Attach chain to FORWARD for this interface
    (run-command-ok "iptables" "-I" "FORWARD" "-i" veth-host "-j" chain)

    chain))

;;; ── Setup ──────────────────────────────────────────────────────────────

(defun setup-netns (config id allow-net)
  "Set up network namespace with veth pair, NAT, DNS forwarding, optional egress filtering.
   CONFIG is the daemon config (unused for now but reserved for future host-interface settings).
   ID is the sandbox identifier.
   ALLOW-NET is a list of allowed CIDRs, or NIL for unrestricted.
   Returns a netns-handle."
  (declare (ignore config)
           (type simple-string id))
  (let* ((index (allocate-netns-index))
         (name (format nil "squash-~A" id))
         (veth-host (format nil "sq-~A-h" id))
         (veth-sandbox (format nil "sq-~A-s" id))
         (subnet (format nil "10.200.~D.0/30" index))
         (host-addr (format nil "10.200.~D.1/30" index))
         (sandbox-addr (format nil "10.200.~D.2/30" index))
         (gateway (format nil "10.200.~D.1" index)))

    ;; Create network namespace
    (unless (run-command-ok "ip" "netns" "add" name)
      (release-netns-index index)
      (error 'sandbox-error :id id :message "failed to create netns"))

    ;; Create veth pair
    (unless (run-command-ok "ip" "link" "add" veth-host "type" "veth"
                            "peer" "name" veth-sandbox)
      (run-command "ip" "netns" "delete" name)
      (release-netns-index index)
      (error 'sandbox-error :id id :message "failed to create veth pair"))

    ;; Move sandbox end into netns
    (run-command-ok "ip" "link" "set" veth-sandbox "netns" name)

    ;; Configure host side
    (run-command-ok "ip" "addr" "add" host-addr "dev" veth-host)
    (run-command-ok "ip" "link" "set" veth-host "up")

    ;; Configure sandbox side
    (run-command-ok "ip" "netns" "exec" name
                    "ip" "addr" "add" sandbox-addr "dev" veth-sandbox)
    (run-command-ok "ip" "netns" "exec" name
                    "ip" "link" "set" veth-sandbox "up")
    (run-command-ok "ip" "netns" "exec" name
                    "ip" "link" "set" "lo" "up")
    (run-command-ok "ip" "netns" "exec" name
                    "ip" "route" "add" "default" "via" gateway)

    ;; Enable IP forwarding
    (write-string-to-file "/proc/sys/net/ipv4/ip_forward" "1")

    ;; NAT: masquerade traffic from this sandbox's subnet
    (run-command-ok "iptables" "-t" "nat" "-A" "POSTROUTING"
                    "-s" subnet "-j" "MASQUERADE")

    ;; DNS DNAT: redirect sandbox DNS queries to the host's nameserver
    (let ((host-dns (parse-first-nameserver "/etc/resolv.conf")))
      (when host-dns
        (run-command-ok "iptables" "-t" "nat" "-A" "PREROUTING"
                        "-s" subnet "-d" gateway "-p" "udp" "--dport" "53"
                        "-j" "DNAT" "--to-destination" host-dns)
        (run-command-ok "iptables" "-t" "nat" "-A" "PREROUTING"
                        "-s" subnet "-d" gateway "-p" "tcp" "--dport" "53"
                        "-j" "DNAT" "--to-destination" host-dns))

      ;; Egress filtering (only if allow-net is a non-empty list)
      (let ((chain-name (when (and allow-net (consp allow-net))
                          (apply-egress-rules id veth-host allow-net))))

        (make-netns-handle :name name
                           :index index
                           :veth-host veth-host
                           :chain-name chain-name
                           :host-dns host-dns)))))

;;; ── Teardown ───────────────────────────────────────────────────────────

(defun teardown-netns (handle)
  "Teardown network namespace and all associated iptables rules.
   Ignores all errors — cleanup must be best-effort."
  (when handle
    (let ((subnet (format nil "10.200.~D.0/30" (netns-handle-index handle)))
          (gateway (format nil "10.200.~D.1" (netns-handle-index handle))))

      ;; 1. Remove egress filter chain
      (when (netns-handle-chain-name handle)
        (ignore-errors
          (run-command "iptables" "-D" "FORWARD"
                       "-i" (netns-handle-veth-host handle)
                       "-j" (netns-handle-chain-name handle)))
        (ignore-errors
          (run-command "iptables" "-F" (netns-handle-chain-name handle)))
        (ignore-errors
          (run-command "iptables" "-X" (netns-handle-chain-name handle))))

      ;; 2. Remove NAT MASQUERADE rule
      (ignore-errors
        (run-command "iptables" "-t" "nat" "-D" "POSTROUTING"
                     "-s" subnet "-j" "MASQUERADE"))

      ;; 3. Remove DNS DNAT rules
      (when (netns-handle-host-dns handle)
        (ignore-errors
          (run-command "iptables" "-t" "nat" "-D" "PREROUTING"
                       "-s" subnet "-d" gateway "-p" "udp" "--dport" "53"
                       "-j" "DNAT" "--to-destination" (netns-handle-host-dns handle)))
        (ignore-errors
          (run-command "iptables" "-t" "nat" "-D" "PREROUTING"
                       "-s" subnet "-d" gateway "-p" "tcp" "--dport" "53"
                       "-j" "DNAT" "--to-destination" (netns-handle-host-dns handle))))

      ;; 4. Delete veth pair (automatically removes sandbox end)
      (ignore-errors
        (run-command "ip" "link" "delete" (netns-handle-veth-host handle)))

      ;; 5. Delete the network namespace
      (ignore-errors
        (run-command "ip" "netns" "delete" (netns-handle-name handle)))

      ;; 6. Release the subnet index for reuse
      (release-netns-index (netns-handle-index handle)))))
