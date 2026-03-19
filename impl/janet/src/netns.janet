# Network namespace — REMOVED in unprivileged mode.
# Sandboxes now use bubblewrap for isolation (no veth pairs, no iptables).
# This stub exists for API compatibility.

(defn setup-netns [id allow-net] nil)
(defn teardown-netns [handle] nil)
