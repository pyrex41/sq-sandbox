#!/usr/bin/env bash
# composefs-validate.sh — end-to-end validation of the composefs LayerStore.
#
# The composefs LayerStore (SQUASH_LAYER_BACKEND=composefs) shells out to real
# kernel mounts (erofs, overlay with a data-only '::' lower + redirect_dir/
# metacopy) and the mkcomposefs/composefs-info tools. None of that can be
# exercised on a non-Linux dev host, so the unit suite's composefs tests SKIP
# there. This script provides the environment where they actually RUN, plus an
# explicit check of the "loop-free on kernel >= 6.12" claim.
#
# Run on a Linux host with kernel >= 6.12, as root (mounts required):
#   sudo ./test/composefs-validate.sh
# or via the packaged image (any >= 6.12 host with Docker):
#   docker build -t sq-composefs-validate -f test/Dockerfile.composefs .
#   docker run --rm --privileged sq-composefs-validate
#
# Exit codes: 0 = validated; 1 = environment missing a hard requirement
# (tools/kernel features) — loud, never a silent pass; 2 = a test FAILED.
set -euo pipefail

ALLOW_OLD_KERNEL="${COMPOSEFS_ALLOW_OLD_KERNEL:-0}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GO_DIR="$REPO_ROOT/impl/go"

say()  { printf '\n\033[1m== %s ==\033[0m\n' "$*"; }
ok()   { printf '  \033[32mOK\033[0m   %s\n' "$*"; }
warn() { printf '  \033[33mWARN\033[0m %s\n' "$*"; }
die()  { printf '  \033[31mFAIL\033[0m %s\n' "$*" >&2; exit "${2:-1}"; }

# ── Phase 0: capability report ───────────────────────────────────────────────
say "Capability report"
KREL="$(uname -r)"
printf '  kernel        : %s\n' "$KREL"
printf '  /proc/filesystems erofs   : %s\n' "$(grep -qw erofs /proc/filesystems && echo yes || echo 'no (modprobe on demand)')"
printf '  /proc/filesystems overlay : %s\n' "$(grep -qw overlay /proc/filesystems && echo yes || echo 'no (modprobe on demand)')"
for p in redirect_dir metacopy; do
  printf '  overlay param %-12s: %s\n' "$p" "$([ -e "/sys/module/overlay/parameters/$p" ] && echo present || echo ABSENT)"
done
for t in mkcomposefs composefs-info fsverity; do
  printf '  tool %-13s: %s\n' "$t" "$(command -v "$t" >/dev/null 2>&1 && command -v "$t" || echo 'absent')"
done

# ── Phase 1: hard requirements (fail loud, never silently skip) ──────────────
say "Hard requirements"
[ "$(uname -s)" = Linux ] || die "not Linux ($(uname -s)); composefs requires a Linux kernel"
[ "$(id -u)" = 0 ] || die "must run as root (erofs/overlay mounts) — re-run with sudo"
command -v mkcomposefs   >/dev/null 2>&1 || die "mkcomposefs not installed (package: composefs)"
command -v composefs-info >/dev/null 2>&1 || die "composefs-info not installed (package: composefs)"
command -v go >/dev/null 2>&1 || die "go toolchain not installed"
[ -e /sys/module/overlay/parameters/redirect_dir ] || { modprobe overlay 2>/dev/null || true; }
[ -e /sys/module/overlay/parameters/redirect_dir ] || die "overlayfs lacks redirect_dir — kernel not composefs-capable"
[ -e /sys/module/overlay/parameters/metacopy ]     || die "overlayfs lacks metacopy — kernel not composefs-capable"
ok "tools + overlayfs features present"

# Kernel >= 6.12 gate (file-backed, loop-free EROFS). Older kernels still mount
# EROFS via an auto-loop, so correctness is validated either way — but the
# loop-free claim is only meaningful at >= 6.12.
KMAJ="${KREL%%.*}"; KREST="${KREL#*.}"; KMIN="${KREST%%.*}"; KMIN="${KMIN%%-*}"
LOOPFREE_EXPECTED=0
if [ "$KMAJ" -gt 6 ] || { [ "$KMAJ" -eq 6 ] && [ "$KMIN" -ge 12 ]; }; then
  LOOPFREE_EXPECTED=1; ok "kernel $KREL >= 6.12 — loop-free file-backed EROFS expected"
else
  if [ "$ALLOW_OLD_KERNEL" = 1 ]; then
    warn "kernel $KREL < 6.12 — correctness validated loop-backed; loop-free claim NOT checked (COMPOSEFS_ALLOW_OLD_KERNEL=1)"
  else
    die "kernel $KREL < 6.12 — file-backed EROFS / loop-free path unavailable. Set COMPOSEFS_ALLOW_OLD_KERNEL=1 to validate correctness loop-backed anyway." 1
  fi
fi

# ── Phase 2: run the composefs test suite for real (no skips expected) ───────
say "Go test suite (composefs LayerStore + GC + mount round-trip)"
cd "$GO_DIR"
# -v so SKIP lines are visible; the privileged mount round-trip and the EROFS/GC
# mark-sweep should now RUN (not skip), since tools + kernel are present.
TEST_OUT="$(go test ./manager/ ./config/ -count=1 -v \
  -run 'Composefs|GC|LayerStore|Probe|Snapshot|MountRoundTrip' 2>&1)" || {
    printf '%s\n' "$TEST_OUT"; die "composefs tests FAILED" 2; }
printf '%s\n' "$TEST_OUT" | grep -E '^(=== RUN|--- (PASS|FAIL|SKIP)|ok|PASS|FAIL)' || true
if printf '%s\n' "$TEST_OUT" | grep -q 'TestComposefsMountRoundTripPrivileged.*SKIP\|--- SKIP.*MountRoundTrip'; then
  die "mount round-trip test SKIPPED despite root+tools+kernel — environment is not actually exercising mounts" 1
fi
ok "composefs test suite passed (mount round-trip executed)"

# ── Phase 3: loop-device report (informational) ──────────────────────────────
# The composefsLayerStore mounts each layer with `mount -t erofs <file>`. Even on
# kernels with CONFIG_EROFS_FS_BACKED_BY_FILE=y (e.g. Fly 6.12.91-fly), util-linux
# loop-wraps a regular-file source, so this allocates ONE loop device per layer —
# it is NOT loop-free. Loop-free requires the fd-based mount API (tracked TODO).
# We therefore REPORT the loop behavior rather than fail on it; the real win is the
# kernel-native read path, not loop/mount-count reduction vs squashfuse.
say "EROFS mount loop-device report (informational)"
work="$(mktemp -d)"; trap 'umount -l "$work/mp" 2>/dev/null || true; rm -rf "$work"' EXIT
mkdir -p "$work/src/etc" "$work/mp" "$work/objects"
echo marker > "$work/src/etc/marker"
mkcomposefs --digest-store="$work/objects" --use-epoch "$work/src" "$work/layer.cfs"
before="$(losetup -a | wc -l)"
mount -t erofs -o ro "$work/layer.cfs" "$work/mp"
after="$(losetup -a | wc -l)"
[ "$(cat "$work/mp/etc/marker")" = marker ] || die "file not readable through erofs mount" 2
if [ "$after" -gt "$before" ]; then
  warn "EROFS mount is LOOP-BACKED (one loop/layer): loops $before -> $after. Loop-free file-backed EROFS (CONFIG_EROFS_FS_BACKED_BY_FILE) needs the fd-based mount API — tracked TODO."
else
  ok "EROFS mounted loop-free (file-backed): loops $before -> $after"
fi
if [ "$LOOPFREE_EXPECTED" = 1 ]; then
  ok "kernel $KREL >= 6.12 — loop-free is achievable here once MountLayer uses the fd-based mount API"
fi

say "Validation complete"
ok "composefs LayerStore validated on kernel $KREL"
