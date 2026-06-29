---
date: 2026-04-27T21:24:44-05:00
researcher: Reuben Brooks
git_commit: 2b7376bd9e960f825071b1e4e1916f32be832748
branch: pr-7-fixes
repository: pyrex41/sq-sandbox
topic: "Does a PorteuX-style solid-layer build for browser-base make sense for sq-sandbox?"
tags: [research, codebase, sq-mkmod, modules, browser-base, gui-base, overlayfs, porteux]
status: implemented
last_updated: 2026-04-27
last_updated_by: Cursor Agent
---

# Research: PorteuX-style "solid layer" approach for sq-sandbox modules

**Date**: 2026-04-27T21:24:44-05:00
**Researcher**: Reuben Brooks
**Git Commit**: 2b7376bd9e960f825071b1e4e1916f32be832748
**Branch**: pr-7-fixes
**Repository**: pyrex41/sq-sandbox

## Research Question

The user proposes moving away from `apk add --root "$empty_root"` for browser-base
toward a PorteuX-style flow: build inside a live merged base+gui sandbox, package
the resulting upper as `510-browser-base.squashfs`, and pin module compatibility
via `/etc/sq-module-manifest`. Does this make sense given the current builder,
activation flow, and existing browser-base implementation?

Three sub-questions:
1. Switch `preset_browser_base` from empty-root apk to live-merged-root + `from-sandbox`?
2. Bundle a single `500-desktop-browser.squashfs` instead of separate `gui-base` + `browser-base`?
3. Add `/etc/sq-module-manifest` pinning?

## Summary

### Post-Implementation Update

The recommended path from this note has now been implemented on branch
`pr-7-fixes`:

- `sq-mkmod preset browser-base` is now the PorteuX-style live builder path. It
  creates a temporary sandbox from `000-base-alpine + 500-gui-base`, installs the
  browser packages in that merged root, and packages only the builder
  `upper/data` as `510-browser-base.squashfs`.
- The old empty-root closure remains available as
  `sq-mkmod preset browser-base-raw`.
- Module manifests are emitted both in-layer at
  `/etc/sq-module/<name>.manifest.json` and as sidecars at
  `<name>.squashfs.manifest.json`.
- The Go manager performs warn-only compatibility checks from sidecar manifests
  during sandbox creation.
- A Docker/Rodney smoke test verified the live-built browser layer: noVNC loaded,
  Chromium opened inside the desktop session, and `https://example.com` rendered.

The sections below remain as the decision record that motivated those changes.

The PorteuX-style "build inside a live merged root, package the upper as a delta"
flow is **already mechanically supported** by `sq-mkmod from-sandbox`
([`shared/bin/sq-mkmod:498-503`](shared/bin/sq-mkmod#L498-L503)) — it is the
documented recommended path for `build-tools`, `rust`, and any base-specific
packaging ([`shared/bin/sq-mkmod:545-551`](shared/bin/sq-mkmod#L545-L551)).
What is missing today is that the two big GUI presets (`gui-base`, `browser-base`)
short-circuit this and use empty-root apk instead
([`shared/bin/sq-mkmod:207-217`](shared/bin/sq-mkmod#L207-L217),
[`shared/bin/sq-mkmod:379-399`](shared/bin/sq-mkmod#L379-L399)). The runtime
overlay-stack model already behaves the way PorteuX does: numeric-prefix sort with
higher = higher priority ([`shared/bin/sq-init:97-101`](shared/bin/sq-init#L97-L101)),
optional snapshot above all numbered modules ([`shared/bin/sq-init:79-86`](shared/bin/sq-init#L79-L86)).
There is no per-module manifest beyond a single integer in
`000-base-alpine.version` ([`shared/bin/sq-mkbase:86`](shared/bin/sq-mkbase#L86),
checked at [`shared/bin/sq-init:16-29`](shared/bin/sq-init#L16-L29)) and two
filesystem markers `/etc/sq-gui-module` / `/etc/sq-browser-module` that contain
only the module name ([`shared/bin/sq-mkmod:357`](shared/bin/sq-mkmod#L357),
[`shared/bin/sq-mkmod:485`](shared/bin/sq-mkmod#L485)).

## Detailed Findings

### Module composition / activation (the runtime side)

- Modules live as squashfs files in `$DATA/modules`, named with a 3-digit numeric
  prefix that encodes priority. The convention is documented at
  [`README.md:362-371`](README.md#L362-L371): `000` base, `10x` languages,
  `11x` build tools, `20x` services, `50x` desktop/GUI, `9xx` checkpoints.
- A sandbox is the directory `$DATA/sandboxes/<id>/` with `images/<mod>.squashfs/`
  mountpoints, `upper/`, `work/`, `merged/`, and `.meta/`
  ([`impl/go/manager/sandbox.go:79`](impl/go/manager/sandbox.go#L79)).
- On `create_sandbox` the Go manager mounts each requested layer's squashfs, then
  builds lowerdirs and mounts overlay
  ([`impl/go/manager/manager.go:714-746`](impl/go/manager/manager.go#L714-L746)).
- On daemon restart `sq-init` walks `$SANDBOXES/*/`, remounts each module from
  `.meta/layers`, and re-builds the overlay
  ([`shared/bin/sq-init:47-107`](shared/bin/sq-init#L47-L107)).
- **Layer ordering** is purely lexical-reverse on the `NNN-` prefix:
  `ls -1d $sdir/images/[0-9]*.squashfs | sort -r`
  ([`shared/bin/sq-init:97`](shared/bin/sq-init#L97)). With overlayfs's left-most-
  is-highest semantics this means `510-browser-base` sits above `500-gui-base`
  sits above `000-base-alpine` — the same model PorteuX uses for its xzm modules.
- Optional snapshot squashfs is prepended to lower (highest priority of all)
  ([`shared/bin/sq-init:79-86`](shared/bin/sq-init#L79-L86)).

### Module *building* (`sq-mkmod`)

`sq-mkmod` ([`shared/bin/sq-mkmod`](shared/bin/sq-mkmod)) supports three input
strategies:

1. **Empty-root apk** — used by `preset_python`, `preset_nodejs`, `preset_golang`,
   `preset_tailscale`, `preset_gui_base`, `preset_browser_base`. apk is invoked
   against an isolated `/tmp/sq-mkmod.$$/rootfs` with `--initdb --no-cache`.
   The relevant calls:
   - `preset_gui_base`: [`shared/bin/sq-mkmod:207-217`](shared/bin/sq-mkmod#L207-L217)
     installs `xvfb xfce4 xfce4-terminal x11vnc novnc websockify dbus-x11 xauth font-dejavu`.
   - `preset_browser_base`: [`shared/bin/sq-mkmod:379-399`](shared/bin/sq-mkmod#L379-L399)
     installs `chromium dillo firefox firefox-esr ca-certificates nss dbus dbus-x11
     xdg-utils font-dejavu font-noto eudev-libs libssh libtheora libxcb libxshmfence
     numactl xcb-util mesa-dri-gallium`.
2. **`from-dir`** — squashes any host directory ([`shared/bin/sq-mkmod:492-496`](shared/bin/sq-mkmod#L492-L496)).
3. **`from-sandbox`** — packages the live `upper/` of an existing sandbox as a
   new module ([`shared/bin/sq-mkmod:498-503`](shared/bin/sq-mkmod#L498-L503)).
   The `list` command explicitly recommends this for base-specific packages
   ([`shared/bin/sq-mkmod:545-551`](shared/bin/sq-mkmod#L545-L551)).

The `apt`/Debian branches of both GUI presets *already* tell the user to use
`from-sandbox` instead ([`shared/bin/sq-mkmod:218-225`](shared/bin/sq-mkmod#L218-L225),
[`shared/bin/sq-mkmod:400-407`](shared/bin/sq-mkmod#L400-L407)) — i.e., the
PorteuX-style flow is already the prescribed path for non-Alpine hosts; only the
Alpine branch takes the empty-root shortcut.

### Existing browser-base coherence workarounds

The `sq-browser-open` script that 510-browser-base ships
([`shared/bin/sq-mkmod:413-483`](shared/bin/sq-mkmod#L413-L483)) carries an
`LD_PRELOAD` block at lines 442–455 that force-loads `libxcb-shm`, `libxcb-sync`,
`libxcb-xfixes`, `libnuma`, `libssh`, `libtheoraenc`, `libudev`, and
`libxkbcommon` before launching Chromium, with the inline comment:

> Alpine's Chromium stack has a few musl-era libraries whose optional symbols are
> resolved only when their extension libraries are already in the process. Preload
> the small set Chromium commonly needs in this minimal live root.

The most recent commit [`2b7376b`](commit) is titled
*"fix: make GUI browser module smoke-testable — Prefer a lightweight X browser
for reliable in-session GUI launches while keeping Firefox and Chromium available
for later runtime hardening."* — it added `dillo`/`firefox-esr` to the apk list
and made `sq-browser-open` prefer `dillo` over Firefox over Chromium
([`shared/bin/sq-mkmod:467-479`](shared/bin/sq-mkmod#L467-L479)).

Both signals (the LD_PRELOAD list, the `dillo` fallback) are workarounds for
incoherence between the 510 layer and the 500 layer at runtime.

### Manifest / version pinning

Single sentinel exists today: `$MODULES/000-base-alpine.version`, written as a
literal `2` ([`shared/bin/sq-mkbase:86`](shared/bin/sq-mkbase#L86)), read by
sq-init and compared against an in-script `_base_current=2` constant
([`shared/bin/sq-init:15-29`](shared/bin/sq-init#L15-L29)). When the file is
missing or stale, sq-init rebuilds the base.

Two name-only filesystem markers exist:
- `/etc/sq-gui-module` containing the literal `500-gui-base`
  ([`shared/bin/sq-mkmod:357`](shared/bin/sq-mkmod#L357)).
- `/etc/sq-browser-module` containing the literal `510-browser-base`
  ([`shared/bin/sq-mkmod:485`](shared/bin/sq-mkmod#L485)).

There is no per-module record of:
- base distro / version it was built against
- apk repo URLs / mirror state at build time
- apk world (the package list) installed
- upstream modules it was layered on at build time
- build timestamp or content hash

S3 distribution is keyed only by module name: `squash_it` pushes `<name>.squashfs`
([`shared/bin/sq-mkmod:33-35`](shared/bin/sq-mkmod#L33-L35)); manager pulls
lazily by name on first reference
([`impl/go/manager/manager.go:716-722`](impl/go/manager/manager.go#L716-L722));
no compatibility metadata travels with the module.

### Module-name selection at runtime

The Go side resolves module names through opts → config → built-in default
([`impl/go/manager/gui.go:382-398`](impl/go/manager/gui.go#L382-L398)):

```go
func guiModule(cfg *config.Config, opts *GUIOpts) string {
    if opts != nil && opts.Module != "" { return opts.Module }
    if cfg != nil && cfg.GUIModule != "" { return cfg.GUIModule }
    return "500-gui-base"
}
```

Environment overrides exist: `SQUASH_GUI_MODULE` and `SQUASH_BROWSER_MODULE`
([`README.md:493-494`](README.md#L493-L494)). Tests already exercise an
alternate `510-gui-minimal` ([`impl/go/manager/gui_test.go:77-78`](impl/go/manager/gui_test.go#L77-L78)).

## Code References

- `shared/bin/sq-mkmod:194-360` — `preset_gui_base`, empty-root apk path
- `shared/bin/sq-mkmod:368-488` — `preset_browser_base`, empty-root apk path; embedded `sq-browser-open` with LD_PRELOAD list
- `shared/bin/sq-mkmod:498-503` — `from_sandbox` (the PorteuX-style packaging primitive)
- `shared/bin/sq-mkmod:545-551` — documented build-in-sandbox recipe
- `shared/bin/sq-mkbase:86` — single integer version sentinel for the base
- `shared/bin/sq-init:16-29` — base version compare/rebuild
- `shared/bin/sq-init:47-107` — sandbox remount loop
- `shared/bin/sq-init:79-101` — overlay lower-string assembly (snapshot first, then `sort -r` of NNN-prefixed mounts)
- `shared/bin/sq-mount-overlay:23-53` — fuse-overlayfs/kernel-overlayfs mount
- `shared/bin/sq-mount-layer:21-54` — squashfuse/loop mount
- `impl/go/manager/manager.go:714-746` — sandbox-create mount sequence
- `impl/go/manager/gui.go:382-398` — gui/browser module-name resolution
- `impl/go/manager/manager.go:342, 718` — lazy S3 pull on missing module
- `README.md:262-300` — documented user-facing model: separate layers, `features:["gui","browser"]`
- `README.md:362-371` — module numbering convention
- Most recent commit `2b7376b` — added dillo as Chromium-fallback workaround

## Architecture Documentation

- **Activation model is already PorteuX-shaped.** Numeric-prefix priority,
  squashfs-per-module, overlayfs union with optional snapshot on top. No change
  needed here.
- **Build model is split.** Empty-root apk for the standalone presets (Python,
  Node, Go, Tailscale, gui-base, browser-base on Alpine); `from-sandbox` for
  everything base-specific (build-tools, Rust, all Debian/Ubuntu builds, custom
  user modules). The two big GUI layers are the only large/complex apk closures
  that take the empty-root path.
- **No cross-module compatibility metadata exists.** S3 distributes name-only
  artifacts. The single integer base-version sentinel is the entire pinning
  vocabulary.
- **Override knobs already exist.** Both gui and browser module names are
  config/env/opts-overridable, so swapping in a `500-desktop-browser` (option 2)
  or a re-cut `510-browser-base` (option 1) is a one-line change for callers.

## Historical Context (from thoughts/)

- `thoughts/shared/research/2026-03-15-sq-sandbox-mount-architecture.md` — covers
  FUSE visibility/mount-propagation issues in sidecar deployments; does not
  discuss layer-build coherence or PorteuX comparison.
- No prior research, plan, or commit explicitly evaluates the empty-root vs.
  live-build trade-off for GUI/browser layers.

## Related Research

- `thoughts/shared/research/2026-03-15-sq-sandbox-mount-architecture.md`
- `thoughts/shared/plans/2026-02-22-nanosquash-implementation.md`

## Open Questions

- Does `preset_gui_base` itself need the same live-build treatment, or only
  `preset_browser_base`? (500 builds against an empty root with apk's own
  baselayout, so its libs are internally coherent; the failure mode only appears
  when a *second* empty-root layer is stacked above it.)
- Should the manifest live as a single `/etc/sq-module/<name>.manifest` JSON, or
  as a sidecar `<name>.squashfs.manifest` file next to the squashfs (so it is
  readable without mounting)? Sidecar makes S3 cheaper to scan; in-fs survives
  rebuilds and travels inside the layer.

---

## Evaluation (per user request — "does this make sense?")

The user explicitly asked for an opinion, so this section steps outside the
documentary scope above.

### (1) Switch `preset_browser_base` to live-build + `from-sandbox` — **Yes, recommended.**

The current empty-root apk for browser-base is the most likely root cause of the
LD_PRELOAD list and the `dillo`-fallback that landed in `2b7376b`. When apk
resolves chromium against an empty `/lib/apk/db/installed`, it pulls a complete
secondary distro (musl, busybox, alpine-baselayout, libxcb, mesa, dbus, fonts,
…) into the upper. Stacked above `500-gui-base` at runtime, those duplicated
libs win the overlay race — and they are not the same minor versions XFCE was
built against in 500. That is the exact divergence the LD_PRELOAD hack is
papering over.

A `from-sandbox` build against a live `(000-base-alpine + 500-gui-base)` root
sees 500's installed-db in the merged view, so apk only writes the chromium-
specific delta into `upper/`. No duplicate musl, no duplicate libxcb, and the
final 510 layer is *by construction* coherent with the 500 it was built on.

The change is small: a new code path in `preset_browser_base` that calls the
existing sandbox API (`sq-ctl create … 000-base-alpine,500-gui-base` →
`sq-ctl exec … "apk add chromium …"` → `sq-mkmod from-sandbox … 510-browser-base`
→ `sq-ctl destroy …`). Almost all of the machinery is already there.

Expected payoff: drop the LD_PRELOAD block, demote `dillo` from default to
explicit-opt-in, and 510 becomes a true ~50–80MB delta instead of a ~200MB
near-distro.

### (2) Bundle `500-desktop-browser` instead of split layers — **No, not as the default.**

Single-transaction install would be the strongest coherence guarantee, but it
loses real value:
- Every GUI sandbox would carry chromium+firefox even when not needed (the
  README explicitly justifies the split: *"Browser support is a separate layer
  because Chromium is large"* at [`README.md:270`](README.md#L270)).
- S3 cache invalidation: a chromium upgrade re-pushes the entire desktop layer.
- The module-override knobs (`SQUASH_GUI_MODULE`, `SQUASH_BROWSER_MODULE`,
  `GUIOpts.Module`, the existing `510-gui-minimal` test) become less useful.

Option (1) gets ~95% of the coherence benefit at none of these costs. Keep
bundle-mode in your back pocket if even live-built 510 layers turn out to drift
in practice — but the LD_PRELOAD evidence suggests (1) alone fixes the observed
failures.

### (3) `/etc/sq-module-manifest` pinning — **Yes, low cost / high value.**

Even with (1) in place, S3 distributes layers across machines, so a freshly-
rebuilt `500-gui-base.squashfs` on host A can land in front of a stale
`510-browser-base.squashfs` on host B that was built against the previous 500.
A manifest catches this:

```yaml
# /etc/sq-module/510-browser-base.manifest
name: 510-browser-base
built_at: 2026-04-27T18:35:17Z
base: 000-base-alpine@v2
built_against: [500-gui-base@<sha-of-squashfs>]
apk_repos: [.../alpine/v3.21/main, .../v3.21/community]
apk_added: [chromium, dillo, firefox-esr, ...]
```

At sandbox create, the manager reads each selected module's manifest, walks the
`built_against` chain, and warns (initially) or refuses (eventually) on
mismatch. Implementation cost is small: extend `squash_it` to emit the manifest
into the rootfs before `mksquashfs`, and add a manifest-read step in
`manager.createSandbox`. The existing `000-base-alpine.version` pattern can be
generalized into this.

### Recommended order

1. **First**: add live-build (`from-sandbox`) path to `preset_browser_base` —
   smallest diff, removes the LD_PRELOAD smell, lets you delete or demote the
   `dillo` fallback. This alone is probably enough for the immediate symptom.
2. **Then**: introduce manifest emission + a soft `WARN` at activation when the
   chain mismatches. No behavior change, just visibility.
3. **Only if needed**: the bundled `500-desktop-browser` variant, kept as an
   alternate preset rather than replacing the split.

### What to watch out for

- The live-build path needs the builder host to be able to *create a sandbox*
  to do the build, which means the daemon must be running. Today `sq-mkmod`
  works standalone with no daemon dependency. Either gate the new flow behind
  a "daemon available?" check, or refactor the inner pieces (`sq-mount-layer`
  + `sq-mount-overlay` + a chroot/unshare exec) into a `sq-mkmod`-internal
  builder that doesn't require a running daemon.
- Snapshot interaction: the snapshot mount sits *above* all numbered modules
  ([`shared/bin/sq-init:79-86`](shared/bin/sq-init#L79-L86)). A user with an
  old snapshot taken before a 510 rebuild will still get the snapshot's view of
  the world. Manifest-aware activation should ideally check the snapshot too,
  but that is a follow-up.
- Don't break the existing override path: `SQUASH_GUI_MODULE=510-gui-minimal`
  must still work without a manifest, with a clearly-logged "no manifest" note.
