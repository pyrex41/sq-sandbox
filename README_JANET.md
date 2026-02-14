# sq-sandbox — Janet Implementation

A Janet implementation of the sq-sandbox container runtime daemon.

## Requirements

- [Janet](https://janet-lang.org/) 1.23+ (for FFI)
- Linux (x86_64 or aarch64) — mount/umount syscalls
- Root or CAP_SYS_ADMIN for mount operations

## Install Janet

```bash
# macOS
brew install janet

# From source
git clone https://github.com/janet-lang/janet.git
cd janet && make && sudo make install
```

## Install Dependencies

```bash
jpm install json
```

Or add to `project.janet` and run `jpm install`.

## Run

```bash
# From project root
JANET_PATH="$(pwd)/src:/opt/homebrew/lib/janet" janet main.janet

# With temp data dir (for testing without root)
SQUASH_DATA=/tmp/squash-janet JANET_PATH="$(pwd)/src:/opt/homebrew/lib/janet" janet main.janet

# Test health
curl http://localhost:8080/cgi-bin/health
# -> {"status":"ok","version":"4"}
```

## Project Structure

```
sq-sandbox-janet/
├── main.janet           # Entry point
├── project.janet        # jpm project config
├── src/
│   ├── config.janet     # Config from env
│   ├── validate.janet   # Input validation
│   ├── syscalls.janet   # FFI: mount, umount2
│   ├── mounts.janet     # squashfs, tmpfs, overlay
│   ├── sandbox.janet    # Create, destroy
│   ├── manager.janet    # Sandbox registry
│   └── api.janet        # HTTP handlers
├── bin/                 # Shared scripts (sq-mkbase, etc.)
└── static/              # Web UI
```

## Implemented

- [x] Config from environment
- [x] Input validation (valid-id, valid-label)
- [x] Mount/unmount (squashfs, tmpfs, overlay) via FFI
- [x] Sandbox create/destroy (minimal — no netns, cgroup yet)
- [x] HTTP API: health, list sandboxes, create sandbox
- [x] Manager with mutex

## TODO

- [ ] Network namespace (netlink or shell out to ip)
- [ ] Cgroups
- [ ] Exec (fork/chroot)
- [ ] Snapshot/restore
- [ ] S3 sync
- [ ] Secret proxy (shell out to Go)
- [ ] Reaper
- [ ] Init/recovery

## Comparison to Other Implementations

| Feature        | Rust | Zig | Odin | CL  | Janet |
|----------------|------|-----|------|-----|-------|
| LOC            | ~11k | ~9k | ~3k  | ~3k | ~500  |
| FFI            | nix  | @cImport | c_* | CFFI | ffi/defbind |
| HTTP           | axum | std.http | custom | Woo | net/server |
| Proxy          | native | stub | Go | Go | Go (planned) |
