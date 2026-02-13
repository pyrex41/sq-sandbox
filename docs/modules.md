# Modules

Modules are numbered squashfs layers stacked on a base filesystem.
The numbering convention determines load order and purpose.

## Numbering convention

| Range | Purpose           | Examples                           |
|-------|-------------------|------------------------------------|
| 000   | Base rootfs       | base-alpine, base-debian           |
| 01x   | System config     | dns, locale, timezone              |
| 10x   | Language runtimes | python312, nodejs22, golang        |
| 11x   | Build tools       | gcc, make, cmake, git              |
| 12x   | Libraries         | openssl, libffi, zlib              |
| 20x   | Services/daemons  | tailscale, nginx, postgres         |
| 9xx   | Checkpoints       | (auto-generated from upper layer)  |

Layers are merged bottom-up by overlayfs. On filename conflict, the highest
number prefix wins.

## Presets

Build with `sq-mkmod preset <name>`:

| Preset       | Module name     | Source                           |
|--------------|-----------------|----------------------------------|
| `python3.12` | `100-python312` | python-build-standalone (GitHub) |
| `nodejs22`   | `100-nodejs22`  | nodejs.org binary                |
| `golang`     | `100-golang`    | go.dev binary                    |
| `tailscale`  | `200-tailscale` | pkgs.tailscale.com binary        |

`rust` and `build-tools` presets exist as stubs — they print instructions to
install inside a running sandbox instead.

## Building custom modules

### From a directory

Package any directory tree as a squashfs module:

```sh
sq-mkmod from-dir /path/to/tree 110-my-tools
```

The directory should contain the filesystem tree as it would appear in the
sandbox root. For example, to add `/usr/local/bin/mytool`:

```
tree/
  usr/
    local/
      bin/
        mytool
```

### From a running sandbox

Capture a sandbox's writable upper layer as a reusable module:

```sh
sq-mkmod from-sandbox my-sandbox 110-my-customizations
```

This is useful for iterative development: install packages interactively in a
sandbox, then freeze the result as a module for future use.

### Workflow

1. Create a sandbox with a base + runtime
2. Install packages, configure tools via exec
3. `sq-mkmod from-sandbox <id> <NNN-name>` to freeze
4. New sandboxes can include the module in their `layers` list
