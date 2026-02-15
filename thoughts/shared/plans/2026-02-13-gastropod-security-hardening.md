# Gastropod Security Hardening & JSON Hygiene

## Overview

Address security vulnerabilities and Gastropod JSON compliance gaps found during codebase audit. Security issues are real and exploitable. JSON issues are correctness/hygiene.

## Current State

The architecture is Gastropod-correct (POSIX shell, CGI, jq, env vars, no frameworks). The issues are in implementation details: unquoted variables enabling injection, missing input validation on path-constructing parameters, `eval` with untrusted data, and hand-rolled JSON in several functions.

## Desired End State

- All user-supplied values that construct filesystem paths are validated (alphanumeric + dash + underscore + dot)
- No shell injection via exec workdir or secret proxy headers
- No `eval` in hot paths with user-influenced arguments
- All JSON construction uses `jq -n --arg` (no string interpolation)
- POST endpoints validate Content-Type

### Verification:
- Manual test: attempt path traversal via snapshot label `../../etc/passwd` — should get 400
- Manual test: attempt double-quote injection in exec workdir — should execute safely inside sandbox
- Manual test: `curl -X POST -d 'not json'` without Content-Type header — should get 415
- All existing API operations still work (create, exec, snapshot, restore, activate, destroy)

## What We're NOT Doing

- Changing handler file structure (set -eu stays in common.sh, implicit exit 0 stays)
- Making execution logging opt-in (audit logs are features, not hidden state)
- Constant-time token comparison (low severity for this use case)
- Rewriting sq-ctl (CLI tool, not attack surface)

## Accepted Gastropod Deviations

These deviate from strict Gastropod but are justified:

1. **`json_err()` uses string wrapper around jq-escaped value** — The inner value IS escaped via `jq -Rs`. The outer `{"error":...}` wrapper is static structure with one dynamic hole. Rewriting to `jq -n --arg` adds a subprocess for every error response with zero safety benefit. Keeping as-is.

2. **`active_snapshot` in sandbox_info uses string interpolation for JSON** — Line 885: `active_snap="\"$(cat ...)\""`. This constructs a JSON string from a validated label (after Phase 1 validation). The alternative is another jq invocation. Keeping as-is.

## Phase 1: Input Validation (Security)

### Overview
Add validation functions and apply them to all user-supplied values that construct filesystem paths or shell commands.

### Changes Required:

#### 1. Add validation helpers to common.sh

**File**: `cgi-bin/common.sh`
**After line 11** (after BACKEND definition), add:

```sh
# ── Input validation ──────────────────────────────────────────────────

# Validate identifier: alphanumeric, dash, underscore only
valid_id() {
    case "$1" in
        "") return 1 ;;
        *[!a-zA-Z0-9_-]*) return 1 ;;
    esac
    return 0
}

# Validate label: alphanumeric, dash, underscore, dot only (for snapshot labels)
valid_label() {
    case "$1" in
        "") return 1 ;;
        *[!a-zA-Z0-9_.-]*) return 1 ;;
    esac
    return 0
}

# Validate module name: NNN-name format, same charset as label
valid_module() {
    case "$1" in
        "") return 1 ;;
        *[!a-zA-Z0-9_.-]*) return 1 ;;
    esac
    return 0
}
```

#### 2. Validate snapshot labels in both backends

**File**: `cgi-bin/common.sh`

In `_chroot_snapshot_sandbox()` (line 474), after the label assignment on line 475:
```sh
valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
```

In `_firecracker_snapshot_sandbox()` (line 730), after the label assignment on line 731:
```sh
valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
```

#### 3. Validate snapshot labels on restore

**File**: `cgi-bin/common.sh`

In `_chroot_restore_sandbox()` (line 496), after line 497:
```sh
valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
```

In `_firecracker_restore_sandbox()` (line 753), after line 754:
```sh
valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
```

#### 4. Validate module names in mod_exists

**File**: `cgi-bin/common.sh`

Change `mod_exists()` (line 116) to validate first:
```sh
mod_exists() {
    valid_module "$1" || return 1
    [ -f "$(mod_path "$1")" ] && return 0
    _s3_enabled && sq-s3 pull "modules/$1.squashfs" "$(mod_path "$1")" 2>/dev/null && return 0
    return 1
}
```

#### 5. Validate module name in activate handler

**File**: `cgi-bin/api/sandboxes`

After line 78 (`[ -z "$mod" ] && ...`), add:
```sh
valid_module "$mod" || { json_err 400 "module: alphanumeric/dash/underscore/dot only"; exit 0; }
```

### Success Criteria:

#### Automated Verification:
- [ ] `curl -X POST .../snapshot -d '{"label":"../../etc/evil"}' -H 'Content-Type: application/json'` returns 400
- [ ] `curl -X POST .../restore -d '{"label":"../../../tmp/x"}' -H 'Content-Type: application/json'` returns 400
- [ ] `curl -X POST .../activate -d '{"module":"../../etc/passwd"}' -H 'Content-Type: application/json'` returns 400
- [ ] Normal operations still work: create sandbox, snapshot with label "test-1", restore "test-1"

---

## Phase 2: Fix Command Injection in Exec (Security)

### Overview
The exec handler constructs `/bin/sh -c "cd $workdir ...; $cmd"`. A double-quote in `$workdir` escapes the string and injects host-side commands. Fix by using single quotes for the static parts and properly embedding variables.

### Changes Required:

#### 1. Fix chroot exec

**File**: `cgi-bin/common.sh`

Replace lines 421-425:
```sh
    timeout "$timeout_s" \
        unshare --mount --pid --fork --map-root-user \
        chroot "$s/merged" \
        /bin/sh -c "cd $workdir 2>/dev/null || true; $cmd" \
        >"$out" 2>"$err" || rc=$?
```

With:
```sh
    timeout "$timeout_s" \
        unshare --mount --pid --fork --map-root-user \
        chroot "$s/merged" \
        /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd" \
        >"$out" 2>"$err" || rc=$?
```

This passes `$workdir` and `$cmd` as positional arguments to the inner shell, so they never get interpreted by the host shell. The inner shell's `$1` and `$2` are properly quoted.

#### 2. Fix vsock handler exec

**File**: `vm/sq-vsock-handler`

Replace line 47-48:
```sh
    timeout "$tout" chroot "$MERGED" /bin/sh -c "cd $wd 2>/dev/null || true; $cmd" \
        >"$out" 2>"$err" || rc=$?
```

With:
```sh
    timeout "$tout" chroot "$MERGED" \
        /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$wd" "$cmd" \
        >"$out" 2>"$err" || rc=$?
```

### Success Criteria:

#### Automated Verification:
- [ ] Exec with normal command works: `{"cmd":"echo hello","workdir":"/"}`
- [ ] Exec with workdir containing special chars stays sandboxed: `{"cmd":"echo ok","workdir":"/tmp\"test"}`
- [ ] Exec with multi-word commands works: `{"cmd":"echo hello && echo world","workdir":"/"}`

---

## Phase 3: Eliminate eval in sq-s3 (Security)

### Overview
Replace `eval` with direct argument passing. The `eval` exists because `_aws_args()` returns a string that needs word-splitting. Fix by building argument arrays and calling commands directly.

### Changes Required:

**File**: `bin/sq-s3`

#### 1. Replace _aws_args with direct calls

Replace the awscli functions (lines 34-61) with versions that call aws directly:

```sh
_awscli_push() {
    local src="$1" key="$2"
    if [ -n "$ENDPOINT" ]; then
        aws s3 cp "$src" "s3://$BUCKET/${PREFIX}$key" --endpoint-url "$ENDPOINT" --region "$REGION" --quiet 2>&1
    else
        aws s3 cp "$src" "s3://$BUCKET/${PREFIX}$key" --region "$REGION" --quiet 2>&1
    fi
}

_awscli_pull() {
    local key="$1" dest="$2"
    if [ -n "$ENDPOINT" ]; then
        aws s3 cp "s3://$BUCKET/${PREFIX}$key" "$dest" --endpoint-url "$ENDPOINT" --region "$REGION" --quiet 2>&1
    else
        aws s3 cp "s3://$BUCKET/${PREFIX}$key" "$dest" --region "$REGION" --quiet 2>&1
    fi
}

_awscli_exists() {
    local key="$1"
    if [ -n "$ENDPOINT" ]; then
        aws s3api head-object --bucket "$BUCKET" --key "${PREFIX}$key" --endpoint-url "$ENDPOINT" --region "$REGION" >/dev/null 2>&1
    else
        aws s3api head-object --bucket "$BUCKET" --key "${PREFIX}$key" --region "$REGION" >/dev/null 2>&1
    fi
}

_awscli_list() {
    local prefix="$1"
    local raw
    if [ -n "$ENDPOINT" ]; then
        raw=$(aws s3api list-objects-v2 --bucket "$BUCKET" --prefix "${PREFIX}$prefix" --endpoint-url "$ENDPOINT" --region "$REGION" --query 'Contents[].Key' --output text 2>/dev/null)
    else
        raw=$(aws s3api list-objects-v2 --bucket "$BUCKET" --prefix "${PREFIX}$prefix" --region "$REGION" --query 'Contents[].Key' --output text 2>/dev/null)
    fi
    printf '%s\n' "$raw" | tr '\t' '\n' | sed "s|^${PREFIX}||"
}
```

#### 2. Replace eval in curl transport

Replace `_curl_s3` (lines 145-200). The curl calls use eval because headers are built as a string. Fix by writing headers to a temp file and using `curl -H @file` approach, or by making individual `-H` calls explicit:

```sh
_curl_s3() {
    local method="$1" key="$2" file="${3:-}" payload_hash="${4:-}"

    local url_path="/$BUCKET/${PREFIX}$key"
    local query=""

    if [ "$method" = "LIST" ]; then
        method="GET"
        query="list-type=2&prefix=${PREFIX}$key"
        url_path="/$BUCKET"
        payload_hash=$(_sha256 "")
    fi

    [ -z "$payload_hash" ] && payload_hash=$(_sha256 "")

    if [ -n "$ENDPOINT" ]; then
        url_path="/$BUCKET/${PREFIX}$key"
        [ "$1" = "LIST" ] && url_path="/$BUCKET"
    else
        url_path="/${PREFIX}$key"
    fi

    local sig_output=$(_sigv4_sign "$method" "$url_path" "$query" "$payload_hash")
    local auth_header=$(echo "$sig_output" | sed -n '1p')
    local datetime=$(echo "$sig_output" | sed -n '2p')
    local content_hash=$(echo "$sig_output" | sed -n '3p')

    local base_url=$(_s3_base_url)
    local full_url="$base_url/${PREFIX}$key"
    [ -n "$query" ] && full_url="$base_url/?$query"

    case "$method" in
        PUT)
            curl -s -f -X PUT \
                -H "Authorization: $auth_header" \
                -H "x-amz-content-sha256: $content_hash" \
                -H "x-amz-date: $datetime" \
                -T "$file" "$full_url" 2>/dev/null
            ;;
        GET)
            if [ -n "$file" ]; then
                curl -s -f -X GET \
                    -H "Authorization: $auth_header" \
                    -H "x-amz-content-sha256: $content_hash" \
                    -H "x-amz-date: $datetime" \
                    -o "$file" "$full_url" 2>/dev/null
            else
                curl -s -f -X GET \
                    -H "Authorization: $auth_header" \
                    -H "x-amz-content-sha256: $content_hash" \
                    -H "x-amz-date: $datetime" \
                    "$full_url" 2>/dev/null
            fi
            ;;
        HEAD)
            curl -s -f -I \
                -H "Authorization: $auth_header" \
                -H "x-amz-content-sha256: $content_hash" \
                -H "x-amz-date: $datetime" \
                "$full_url" >/dev/null 2>&1
            ;;
    esac
}
```

### Success Criteria:

#### Automated Verification:
- [ ] `sq-s3 push`, `sq-s3 pull`, `sq-s3 list` still work with both awscli and curl transports
- [ ] No `eval` remains in sq-s3: `grep -c 'eval' bin/sq-s3` returns 0

---

## Phase 4: Fix sq-secret-proxy eval (Security)

### Overview
The secret proxy builds curl arguments as a string and eval's them. Headers from untrusted HTTP requests flow into this eval. Fix by writing headers to a file and using `@` syntax or xargs.

### Changes Required:

**File**: `bin/sq-secret-proxy`

Replace the curl invocation (lines 92-105) with a tmpfile-based approach:

```sh
    # Build curl config file
    local curl_cfg
    curl_cfg=$(mktemp)
    while IFS= read -r h; do
        [ -z "$h" ] && continue
        printf 'header = "%s"\n' "$h" >> "$curl_cfg"
    done < "$header_file"
    printf 'url = "%s"\n' "$url" >> "$curl_cfg"
    printf 'request = "%s"\n' "$method" >> "$curl_cfg"
    printf -- '-s\n-S\n--max-time 30\n' >> "$curl_cfg"
    printf -- '-D "%s.hdr"\n' "$response_file" >> "$curl_cfg"
    printf -- '-o "%s"\n' "$response_file" >> "$curl_cfg"

    if [ -n "$body" ]; then
        local body_file
        body_file=$(mktemp)
        printf '%s' "$body" > "$body_file"
        printf 'data-binary = "@%s"\n' "$body_file" >> "$curl_cfg"
        curl -K "$curl_cfg" 2>/dev/null
        rm -f "$body_file"
    else
        curl -K "$curl_cfg" 2>/dev/null
    fi

    rm -f "$curl_cfg"
```

Note: `curl -K` reads options from a config file, avoiding eval entirely. The `"` quoting in the config file handles values with spaces.

### Success Criteria:

#### Automated Verification:
- [ ] Secret proxy forwards normal requests correctly
- [ ] Secret proxy replaces placeholders for allowed hosts
- [ ] No `eval` remains in sq-secret-proxy: `grep -c 'eval' bin/sq-secret-proxy` returns 0

---

## Phase 5: JSON Hygiene (Gastropod Compliance)

### Overview
Replace all hand-rolled JSON construction with `jq -n --arg`. Add Content-Type validation to POST endpoints.

### Changes Required:

#### 1. Fix list_modules()

**File**: `cgi-bin/common.sh`

Replace lines 122-147:
```sh
list_modules() {
    local seen=""
    {
        for f in "$MODULES"/*.squashfs; do
            [ ! -f "$f" ] && continue
            local name=$(basename "$f" .squashfs)
            local size=$(stat -c%s "$f" 2>/dev/null || echo 0)
            jq -n --arg name "$name" --argjson size "$size" --arg loc "local" \
                '{name:$name,size:$size,location:$loc}'
            seen="$seen $name "
        done
        if _s3_enabled; then
            sq-s3 list "modules/" 2>/dev/null | while read -r key; do
                [ -z "$key" ] && continue
                local rname=$(basename "$key" .squashfs)
                case "$seen" in *" $rname "*) continue ;; esac
                jq -n --arg name "$rname" --argjson size 0 --arg loc "remote" \
                    '{name:$name,size:$size,location:$loc}'
            done
        fi
    } | jq -s '.'
}
```

#### 2. Fix list_sandboxes()

**File**: `cgi-bin/common.sh`

Replace lines 917-927:
```sh
list_sandboxes() {
    {
        for d in "$SANDBOXES"/*/; do
            [ ! -d "$d/.meta" ] && continue
            local id=$(basename "$d")
            sandbox_info "$id"
        done
    } | jq -s '.'
}
```

#### 3. Fix snapshot JSONL logging

**File**: `cgi-bin/common.sh`

Replace lines 486-487 (chroot snapshot):
```sh
    jq -n --arg label "$label" --arg created "$(date -Iseconds)" --argjson size "$size" \
        '{label:$label,created:$created,size:$size}' >> "$s/.meta/snapshots.jsonl"
```

Replace lines 743-744 (firecracker snapshot):
```sh
    jq -n --arg label "$label" --arg created "$(date -Iseconds)" --argjson size "$size" \
        '{label:$label,created:$created,size:$size}' >> "$s/.meta/snapshots.jsonl"
```

#### 4. Fix snapshot response in handler

**File**: `cgi-bin/api/sandboxes`

Replace line 89:
```sh
        json_ok "$(jq -n --arg snap "$(basename "$snapfile" .squashfs)" --argjson size "$size" '{snapshot:$snap,size:$size}')"
```

#### 5. Fix sq-firecracker Firecracker API calls

**File**: `bin/sq-firecracker`

Replace curl -d calls with jq-constructed JSON. Example for boot-source (line 34-36):
```sh
    jq -n --arg kernel "$KERNEL" --arg args "$boot_args" \
        '{kernel_image_path:$kernel,boot_args:$args}' \
    | curl -s --unix-socket "$sock" -X PUT "http://localhost/boot-source" \
        -H 'Content-Type: application/json' -d @-
```

Apply same pattern to all 7 curl calls in fc_start and fc_add_drive.

#### 6. Add Content-Type validation

**File**: `cgi-bin/common.sh`

Add helper after `read_body()`:
```sh
require_json() {
    case "${CONTENT_TYPE:-}" in
        application/json*) ;;
        *) json_err 415 "expected Content-Type: application/json"; exit 0 ;;
    esac
}
```

**File**: `cgi-bin/api/sandboxes`

Add `require_json` call before `read_body` on each POST endpoint:
- Line 18 (create): add `require_json` before `body=$(read_body)`
- Line 64 (exec): add `require_json` before `body=$(read_body)`
- Line 76 (activate): add `require_json` before `body=$(read_body)`
- Line 85 (snapshot): add `require_json` before `body=$(read_body)`
- Line 94 (restore): add `require_json` before `body=$(read_body)`

### Success Criteria:

#### Automated Verification:
- [ ] `grep -n 'out="$out' cgi-bin/common.sh` returns no matches (no string-concatenated JSON)
- [ ] `grep -n 'printf.*{.*\\\"' cgi-bin/common.sh` returns no matches (no printf JSON)
- [ ] `grep -c 'eval' bin/sq-firecracker` returns 0
- [ ] POST without Content-Type returns 415: `curl -X POST .../exec -d '{"cmd":"echo hi"}'`
- [ ] POST with Content-Type works: `curl -X POST -H 'Content-Type: application/json' .../exec -d '{"cmd":"echo hi"}'`
- [ ] `GET /cgi-bin/api/modules` returns valid JSON array
- [ ] `GET /cgi-bin/api/sandboxes` returns valid JSON array

---

## Testing Strategy

### Manual Testing:
1. Full sandbox lifecycle: create → exec → activate → snapshot → restore → destroy
2. Path traversal attempts on snapshot labels, module names
3. Special character injection in exec workdir
4. S3 push/pull operations still work
5. Secret proxy header replacement still works

## References

- Gastropod directive: `.claude/skills/gastro-gastropod/SKILL.md`
- Security audit: conversation context (2026-02-13 review)
