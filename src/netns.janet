# Network namespace — veth pair, NAT, DNS DNAT.
#
# Uses array-based os/execute — no shell string interpolation,
# no injection risk from sandbox IDs or network parameters.

(import lock)

(def netns-lock (lock/make-lock))
(var netns-index-bitmap @{}) # index -> true when allocated

(defn- allocate-index []
  (lock/with-lock netns-lock
    (for i 1 254
      (when (not (get netns-index-bitmap i))
        (put netns-index-bitmap i true)
        (break i)))
    (error "no free netns indices")))

(defn- release-index [index]
  (when (and index (> index 0) (< index 255))
    (lock/with-lock netns-lock
      (put netns-index-bitmap index nil))))

(defn- run-ok
  "Run command as array via execvp. Returns true if exit code 0."
  [& args]
  (try
    (= 0 (os/execute (tuple ;(map string args)) :p))
    ([e] false)))

(defn- run
  "Run command as array. Returns exit code."
  [& args]
  (try
    (os/execute (tuple ;(map string args)) :p)
    ([e] -1)))

(defn- parse-first-nameserver [path]
  (try
    (do
      (def content (slurp path))
      (when content
        (var found nil)
        (each line (string/split "\n" content)
          (def trimmed (string/trim line))
          (when (and (not found)
                     (> (length trimmed) 11)
                     (string/has-prefix? "nameserver " trimmed))
            (set found (string/trim (string/slice trimmed 11)))))
        found))
    ([e] nil)))

(defn- allow-net-hosts [allow-net]
  "Normalize allow-net input into an array of host strings."
  (def hosts @[])
  (when allow-net
    (if (string? allow-net)
      (array/push hosts allow-net)
      (each host allow-net
        (when (string? host)
          (array/push hosts host)))))
  hosts)

(defn- apply-egress-rules
  "Create per-sandbox FORWARD chain and allow only listed hosts."
  [id veth-host allow-net]
  (def hosts (allow-net-hosts allow-net))
  (when (= (length hosts) 0)
    (return nil))

  (def chain (string "SQ-" id))
  (run-ok "iptables" "-N" chain)
  (run-ok "iptables" "-I" "FORWARD" "-i" veth-host "-j" chain)

  # Allow established/related return traffic.
  (run-ok "iptables" "-A" chain "-m" "conntrack" "--ctstate" "ESTABLISHED,RELATED" "-j" "ACCEPT")

  # DNS always allowed so hostname resolution still works.
  (run-ok "iptables" "-A" chain "-p" "udp" "--dport" "53" "-j" "ACCEPT")
  (run-ok "iptables" "-A" chain "-p" "tcp" "--dport" "53" "-j" "ACCEPT")

  (each host hosts
    (if (string/find "*" host)
      (eprintf "netns: wildcard %s requires proxy mode, skipping\n" host)
      # Host can be IP/CIDR/domain. iptables resolves domains at rule insert time.
      (run-ok "iptables" "-A" chain "-d" host "-j" "ACCEPT")))

  # Default deny.
  (run-ok "iptables" "-A" chain "-j" "DROP")
  chain)

(defn setup-netns
  "Create netns with veth pair, NAT, DNS DNAT. Returns handle table."
  [id allow-net]
  (def index (allocate-index))
  (def name (string "squash-" id))
  # Linux veth names max 15 chars: "sq-" (3) + short-id (10) + "-h" (2) = 15
  (def max-id-len 10)
  (def short-id (if (> (length id) max-id-len) (string/slice id 0 max-id-len) id))
  (def veth-host (string "sq-" short-id "-h"))
  (def veth-sandbox (string "sq-" short-id "-s"))
  (def subnet (string/format "10.200.%d.0/30" index))
  (def host-addr (string/format "10.200.%d.1/30" index))
  (def sandbox-addr (string/format "10.200.%d.2/30" index))
  (def gateway (string/format "10.200.%d.1" index))

  (def ok
    (and
      (run-ok "ip" "netns" "add" name)
      (run-ok "ip" "link" "add" veth-host "type" "veth" "peer" "name" veth-sandbox)
      (run-ok "ip" "link" "set" veth-sandbox "netns" name)
      (run-ok "ip" "addr" "add" host-addr "dev" veth-host)
      (run-ok "ip" "link" "set" veth-host "up")
      (run-ok "ip" "netns" "exec" name "ip" "addr" "add" sandbox-addr "dev" veth-sandbox)
      (run-ok "ip" "netns" "exec" name "ip" "link" "set" veth-sandbox "up")
      (run-ok "ip" "netns" "exec" name "ip" "link" "set" "lo" "up")
      (run-ok "ip" "netns" "exec" name "ip" "route" "add" "default" "via" gateway)))

  (when (not ok)
    (release-index index)
    (run "ip" "netns" "delete" name)
    (run "ip" "link" "delete" veth-host)
    (error (string/format "netns setup failed for %s" id)))

  # Enable IP forwarding
  (try (spit "/proc/sys/net/ipv4/ip_forward" "1") ([e] nil))

  # NAT masquerade
  (run-ok "iptables" "-t" "nat" "-A" "POSTROUTING" "-s" subnet "-j" "MASQUERADE")

  # DNS DNAT — forward sandbox DNS queries to host nameserver
  (def host-dns (parse-first-nameserver "/etc/resolv.conf"))
  (when host-dns
    (run-ok "iptables" "-t" "nat" "-A" "PREROUTING"
            "-s" subnet "-d" gateway "-p" "udp" "--dport" "53"
            "-j" "DNAT" "--to-destination" host-dns)
    (run-ok "iptables" "-t" "nat" "-A" "PREROUTING"
            "-s" subnet "-d" gateway "-p" "tcp" "--dport" "53"
            "-j" "DNAT" "--to-destination" host-dns))

  (def chain (apply-egress-rules id veth-host allow-net))

  @{:name name :index index :veth-host veth-host :host-dns host-dns :chain chain})

(defn teardown-netns
  "Tear down netns, veth, iptables rules. Best-effort, never throws."
  [handle]
  (when handle
    (def index (get handle :index))
    (def name (get handle :name))
    (def veth-host (get handle :veth-host))
    (def subnet (string/format "10.200.%d.0/30" index))
    (def gateway (string/format "10.200.%d.1" index))
    (def host-dns (get handle :host-dns))
    (def chain (get handle :chain))

    (when chain
      (try (run "iptables" "-D" "FORWARD" "-i" veth-host "-j" chain) ([e] nil))
      (try (run "iptables" "-F" chain) ([e] nil))
      (try (run "iptables" "-X" chain) ([e] nil)))
    (try (run "iptables" "-t" "nat" "-D" "POSTROUTING" "-s" subnet "-j" "MASQUERADE") ([e] nil))
    (when host-dns
      (try (run "iptables" "-t" "nat" "-D" "PREROUTING"
                "-s" subnet "-d" gateway "-p" "udp" "--dport" "53"
                "-j" "DNAT" "--to-destination" host-dns) ([e] nil))
      (try (run "iptables" "-t" "nat" "-D" "PREROUTING"
                "-s" subnet "-d" gateway "-p" "tcp" "--dport" "53"
                "-j" "DNAT" "--to-destination" host-dns) ([e] nil)))
    (try (run "ip" "link" "delete" veth-host) ([e] nil))
    (try (run "ip" "netns" "delete" name) ([e] nil))
    (release-index index)))
