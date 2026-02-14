// exec.zig — Sandbox execution via fork, pipe, namespace isolation, chroot, execve.
//
// Security-sensitive: the ordering of setns, unshare, chroot, and chdir
// matters. The child enters the network namespace FIRST (setns requires
// the host mount namespace to resolve /var/run/netns/), then unshares
// mount/PID/IPC/UTS, then chroots into the merged overlayfs, then chdir
// to the workdir, and finally execve.

const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;
const log = std.log.scoped(.exec);
const json_mod = @import("json.zig");

const c = @cImport({
    @cInclude("sys/mount.h");
    @cInclude("sched.h");
    @cInclude("unistd.h");
    @cInclude("sys/wait.h");
    @cInclude("signal.h");
    @cInclude("poll.h");
    @cInclude("fcntl.h");
    @cInclude("errno.h");
});

/// Maximum bytes captured from stdout/stderr (64KB each).
pub const max_output_bytes: usize = 65536;

/// Request to execute a command inside a sandbox.
pub const ExecRequest = struct {
    /// Command string passed to /bin/sh -c.
    cmd: []const u8,
    /// Working directory inside the chroot. Defaults to "/".
    workdir: []const u8 = "/",
    /// Timeout in seconds. 0 = no timeout.
    timeout_s: u64 = 300,
};

/// Result of a sandbox execution.
pub const ExecResult = struct {
    exit_code: i32,
    stdout: []const u8,
    stderr: []const u8,
    started: i64,
    finished: i64,
    timed_out: bool,
    seq: u32,

    pub fn deinit(self: *ExecResult, allocator: std.mem.Allocator) void {
        if (self.stdout.len > 0) allocator.free(self.stdout);
        if (self.stderr.len > 0) allocator.free(self.stderr);
        self.* = undefined;
    }
};

/// Atomic counter for exec log sequence numbering.
/// One instance per sandbox, shared across all exec calls for that sandbox.
pub const SeqCounter = struct {
    value: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),

    /// Atomically increment and return the new sequence number (1-based).
    pub fn next(self: *SeqCounter) u32 {
        return self.value.fetchAdd(1, .monotonic) + 1;
    }

    /// Get the current count without incrementing.
    pub fn current(self: *const SeqCounter) u32 {
        return self.value.load(.monotonic);
    }
};

/// Parameters describing the sandbox environment for exec.
/// Other modules (sandbox.zig, manager.zig) populate this from their state.
pub const SandboxContext = struct {
    /// Absolute path to the merged overlayfs directory (chroot target).
    merged_path: [*:0]const u8,
    /// Network namespace name (e.g. "squash-myid"). Null if no netns.
    netns_name: ?[]const u8 = null,
    /// Cgroup path (e.g. "/sys/fs/cgroup/squash-myid"). Null if no cgroup.
    cgroup_path: ?[]const u8 = null,
    /// Absolute path to the exec log directory for this sandbox. Null to skip logging.
    log_dir: ?[]const u8 = null,
    /// Atomic sequence counter shared across execs for this sandbox.
    seq_counter: ?*SeqCounter = null,
};

pub const ExecError = error{
    ForkFailed,
    PipeCreationFailed,
    NetnsOpenFailed,
    PollFailed,
    WaitFailed,
    OutputAlloc,
};

/// Execute a command inside a sandboxed environment.
///
/// Flow:
///   1. Create pipes for stdout/stderr capture
///   2. Open netns fd (if applicable) — must happen BEFORE fork while we
///      still have access to the host filesystem
///   3. Fork
///   4. Child: close pipe read ends, dup2 write ends to stdout/stderr,
///      enter cgroup, setns into netns, unshare mount/PID/IPC/UTS,
///      chroot, chdir, execve /bin/sh -c <cmd>
///   5. Parent: close pipe write ends, poll-read stdout/stderr with
///      timeout, waitpid, build result
pub fn execInSandbox(
    allocator: std.mem.Allocator,
    ctx: SandboxContext,
    req: ExecRequest,
) ExecError!ExecResult {
    const started = std.time.timestamp();

    // 1. Create pipes
    const stdout_pipe = createPipe() orelse return ExecError.PipeCreationFailed;
    defer posix.close(stdout_pipe.read);
    const stderr_pipe = createPipe() orelse return ExecError.PipeCreationFailed;
    defer posix.close(stderr_pipe.read);

    // 2. Open netns fd before fork (requires host mount namespace)
    const netns_fd = openNetns(ctx.netns_name);
    defer if (netns_fd) |fd| posix.close(fd);

    // 3. Fork
    const fork_result = posix.fork() catch return ExecError.ForkFailed;

    if (fork_result == 0) {
        // ═══ CHILD PROCESS ═══
        // This code path never returns on success (execve replaces the process).
        // On failure, _exit(126) or _exit(127).
        childExec(
            stdout_pipe,
            stderr_pipe,
            netns_fd,
            ctx,
            req,
        );
        // childExec calls _exit and never returns
        unreachable;
    }

    // ═══ PARENT PROCESS ═══
    const child_pid = fork_result;

    // Close write ends — only the child writes to these
    posix.close(stdout_pipe.write);
    posix.close(stderr_pipe.write);

    // Read stdout/stderr with timeout via poll()
    var stdout_buf: [max_output_bytes]u8 = undefined;
    var stderr_buf: [max_output_bytes]u8 = undefined;
    var stdout_len: usize = 0;
    var stderr_len: usize = 0;
    var timed_out = false;

    const deadline_ms: i64 = if (req.timeout_s > 0)
        std.time.milliTimestamp() + @as(i64, @intCast(req.timeout_s)) * 1000
    else
        std.math.maxInt(i64); // effectively no timeout

    var stdout_eof = false;
    var stderr_eof = false;

    while (!stdout_eof or !stderr_eof) {
        const now_ms = std.time.milliTimestamp();
        if (now_ms >= deadline_ms) {
            // Timeout — kill child process group
            killChild(child_pid);
            timed_out = true;
            break;
        }

        const remaining_ms = deadline_ms - now_ms;
        const poll_timeout: i32 = @intCast(@min(remaining_ms, std.math.maxInt(i32)));

        var fds = [2]posix.pollfd{
            .{
                .fd = if (stdout_eof) -1 else stdout_pipe.read,
                .events = posix.POLL.IN,
                .revents = 0,
            },
            .{
                .fd = if (stderr_eof) -1 else stderr_pipe.read,
                .events = posix.POLL.IN,
                .revents = 0,
            },
        };

        const poll_rc = posix.poll(&fds, poll_timeout) catch {
            // poll error — kill child and break
            killChild(child_pid);
            break;
        };

        if (poll_rc == 0) {
            // poll timeout — check deadline again at top of loop
            continue;
        }

        // Read stdout
        if (!stdout_eof) {
            if (fds[0].revents & posix.POLL.IN != 0) {
                if (stdout_len < max_output_bytes) {
                    const n = posix.read(stdout_pipe.read, stdout_buf[stdout_len..]) catch 0;
                    if (n == 0) {
                        stdout_eof = true;
                    } else {
                        stdout_len += n;
                    }
                } else {
                    // Buffer full — drain and discard
                    var discard: [4096]u8 = undefined;
                    const n = posix.read(stdout_pipe.read, &discard) catch 0;
                    if (n == 0) stdout_eof = true;
                }
            }
            if (fds[0].revents & (posix.POLL.HUP | posix.POLL.ERR) != 0) {
                // Drain remaining data after HUP
                if (stdout_len < max_output_bytes) {
                    while (true) {
                        const n = posix.read(stdout_pipe.read, stdout_buf[stdout_len..]) catch break;
                        if (n == 0) break;
                        stdout_len += n;
                        if (stdout_len >= max_output_bytes) break;
                    }
                }
                stdout_eof = true;
            }
        }

        // Read stderr
        if (!stderr_eof) {
            if (fds[1].revents & posix.POLL.IN != 0) {
                if (stderr_len < max_output_bytes) {
                    const n = posix.read(stderr_pipe.read, stderr_buf[stderr_len..]) catch 0;
                    if (n == 0) {
                        stderr_eof = true;
                    } else {
                        stderr_len += n;
                    }
                } else {
                    var discard: [4096]u8 = undefined;
                    const n = posix.read(stderr_pipe.read, &discard) catch 0;
                    if (n == 0) stderr_eof = true;
                }
            }
            if (fds[1].revents & (posix.POLL.HUP | posix.POLL.ERR) != 0) {
                if (stderr_len < max_output_bytes) {
                    while (true) {
                        const n = posix.read(stderr_pipe.read, stderr_buf[stderr_len..]) catch break;
                        if (n == 0) break;
                        stderr_len += n;
                        if (stderr_len >= max_output_bytes) break;
                    }
                }
                stderr_eof = true;
            }
        }
    }

    // Wait for child to exit
    const exit_code = waitForChild(child_pid, timed_out);
    const finished = std.time.timestamp();

    // Determine final exit code
    const final_exit_code: i32 = if (timed_out) 124 else exit_code;

    // Get sequence number and write exec log
    const seq: u32 = if (ctx.seq_counter) |counter| counter.next() else 0;

    // TODO: Implement writeExecLog for execution logging
    // if (ctx.log_dir != null and ctx.seq_counter != null) {
    //     writeExecLog(
    //         ctx.log_dir.?,
    //         seq,
    //         req,
    //         final_exit_code,
    //         started,
    //         finished,
    //         stdout_buf[0..stdout_len],
    //         stderr_buf[0..stderr_len],
    //         timed_out,
    //     );
    // }

    // Copy output to caller-owned memory
    const stdout_copy = if (stdout_len > 0)
        allocator.dupe(u8, stdout_buf[0..stdout_len]) catch return ExecError.OutputAlloc
    else
        &[_]u8{};

    const stderr_copy = if (stderr_len > 0)
        (allocator.dupe(u8, stderr_buf[0..stderr_len]) catch {
            if (stdout_len > 0) allocator.free(stdout_copy);
            return ExecError.OutputAlloc;
        })
    else
        &[_]u8{};

    return ExecResult{
        .exit_code = final_exit_code,
        .stdout = stdout_copy,
        .stderr = stderr_copy,
        .started = started,
        .finished = finished,
        .timed_out = timed_out,
        .seq = seq,
    };
}

// ── Internal helpers ────────────────────────────────────────────────────

const Pipe = struct {
    read: posix.fd_t,
    write: posix.fd_t,
};

fn createPipe() ?Pipe {
    const fds = posix.pipe2(.{ .CLOEXEC = true }) catch return null;
    return .{ .read = fds[0], .write = fds[1] };
}

/// Open the network namespace file descriptor.
/// Returns null if no netns is configured.
fn openNetns(netns_name: ?[]const u8) ?posix.fd_t {
    const name = netns_name orelse return null;
    if (name.len == 0) return null;

    var path_buf: [280]u8 = undefined;
    const ns_path = std.fmt.bufPrintZ(&path_buf, "/var/run/netns/{s}", .{name}) catch return null;

    return posix.openZ(ns_path, .{}, 0) catch |err| {
        log.warn("failed to open netns {s}: {}", .{ name, err });
        return null;
    };
}

/// Child process: set up namespaces, chroot, and exec.
/// This function NEVER returns — it calls _exit or execve.
fn childExec(
    stdout_pipe: Pipe,
    stderr_pipe: Pipe,
    netns_fd: ?posix.fd_t,
    ctx: SandboxContext,
    req: ExecRequest,
) noreturn {
    // Close read ends of pipes (parent reads these)
    posix.close(stdout_pipe.read);
    posix.close(stderr_pipe.read);

    // Redirect stdout and stderr to pipe write ends
    posix.dup2(stdout_pipe.write, posix.STDOUT_FILENO) catch exitChild(126);
    posix.dup2(stderr_pipe.write, posix.STDERR_FILENO) catch exitChild(126);
    // Close the original write fds now that they're dup'd
    posix.close(stdout_pipe.write);
    posix.close(stderr_pipe.write);

    // Enter cgroup (best-effort — if cgroup doesn't exist, continue)
    if (ctx.cgroup_path) |cg_path| {
        addSelfToCgroup(cg_path);
    }

    // Enter network namespace via setns(fd, CLONE_NEWNET)
    // This MUST happen before unshare(CLONE_NEWNS) because setns needs
    // access to /var/run/netns/ which is on the host mount namespace.
    if (netns_fd) |fd| {
        const rc = c.setns(fd, c.CLONE_NEWNET);
        if (rc != 0) exitChild(126);
    }

    // Create new mount, PID, IPC, UTS namespaces
    const ns_flags = c.CLONE_NEWNS | c.CLONE_NEWPID | c.CLONE_NEWIPC | c.CLONE_NEWUTS;
    if (c.unshare(ns_flags) != 0) exitChild(126);

    // After unshare(CLONE_NEWPID), the current process is still in the old
    // PID namespace. We need to fork once more so the child gets PID 1 in
    // the new namespace. This matches the shell behavior of
    // `unshare --pid --fork`.
    const inner_pid = posix.fork() catch exitChild(126);
    if (inner_pid != 0) {
        // Intermediate parent: wait for inner child and exit with its status
        const wait_result = posix.waitpid(inner_pid, 0);
        if (posix.W.IFEXITED(wait_result.status)) {
            exitChild(@intCast(posix.W.EXITSTATUS(wait_result.status)));
        } else if (posix.W.IFSIGNALED(wait_result.status)) {
            exitChild(128 + @as(u8, @intCast(posix.W.TERMSIG(wait_result.status))));
        } else {
            exitChild(126);
        }
    }

    // Inner child: PID 1 in the new PID namespace

    // Chroot into the merged overlayfs
    posix.chroot(ctx.merged_path) catch exitChild(126);

    // Change to requested working directory (best-effort)
    var workdir_buf: [4096]u8 = undefined;
    const workdir_z = std.fmt.bufPrintZ(&workdir_buf, "{s}", .{req.workdir}) catch exitChild(126);
    posix.chdirZ(workdir_z) catch {};

    // Build null-terminated command string for /bin/sh -c
    var cmd_buf: [65536]u8 = undefined;
    if (req.cmd.len >= cmd_buf.len) exitChild(126);
    @memcpy(cmd_buf[0..req.cmd.len], req.cmd);
    cmd_buf[req.cmd.len] = 0;
    const cmd_z: [*:0]const u8 = @ptrCast(&cmd_buf);

    // Set up minimal environment.
    // Use /bin/sh -l -c so that /etc/profile.d/*.sh are sourced,
    // which is where squash-secrets.sh exports placeholder env vars.
    const env_home: [*:0]const u8 = "HOME=/root";
    const env_path: [*:0]const u8 = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
    const env_term: [*:0]const u8 = "TERM=xterm";

    const argv = [_:null]?[*:0]const u8{
        "/bin/sh",
        "-l",
        "-c",
        cmd_z,
    };
    const envp = [_:null]?[*:0]const u8{
        env_home,
        env_path,
        env_term,
    };

    _ = posix.execveZ("/bin/sh", &argv, &envp);
    // execve failed
    exitChild(127);
}

/// Write our PID to the cgroup's cgroup.procs file.
fn addSelfToCgroup(cg_path: []const u8) void {
    var procs_buf: [280]u8 = undefined;
    const procs_path = std.fmt.bufPrint(&procs_buf, "{s}/cgroup.procs", .{cg_path}) catch return;

    const file = std.fs.openFileAbsolute(procs_path, .{ .mode = .write_only }) catch return;
    defer file.close();

    var pid_buf: [16]u8 = undefined;
    const pid_str = std.fmt.bufPrint(&pid_buf, "{d}", .{c.getpid()}) catch return;
    _ = file.write(pid_str) catch {};
}

/// Exit the child process without running any Zig cleanup (atexit, defer, etc).
fn exitChild(code: u8) noreturn {
    // Use the C _exit to avoid any buffered I/O issues
    c._exit(code);
}

/// Kill a child process. Sends SIGKILL.
fn killChild(pid: posix.pid_t) void {
    // First try SIGTERM for graceful shutdown
    posix.kill(pid, posix.SIG.TERM) catch {};
    // Brief grace period then SIGKILL
    std.time.sleep(100 * std.time.ns_per_ms);
    posix.kill(pid, posix.SIG.KILL) catch {};
}

/// Wait for child process and return its exit code.
fn waitForChild(pid: posix.pid_t, was_killed: bool) i32 {
    _ = was_killed;
    const wait_result = posix.waitpid(pid, 0);

    if (posix.W.IFEXITED(wait_result.status)) {
        return @as(i32, @intCast(posix.W.EXITSTATUS(wait_result.status)));
    } else if (posix.W.IFSIGNALED(wait_result.status)) {
        return @as(i32, 128) + @as(i32, @intCast(posix.W.TERMSIG(wait_result.status)));
    }
    return -1;
}

// ── Tests ───────────────────────────────────────────────────────────────

test "ExecRequest defaults" {
    const req = ExecRequest{ .cmd = "echo hello" };
    try std.testing.expectEqualStrings("/", req.workdir);
    try std.testing.expectEqual(@as(u64, 300), req.timeout_s);
}

test "ExecResult deinit frees allocations" {
    const allocator = std.testing.allocator;
    const stdout_data = try allocator.dupe(u8, "hello");
    const stderr_data = try allocator.dupe(u8, "world");

    var result = ExecResult{
        .exit_code = 0,
        .stdout = stdout_data,
        .stderr = stderr_data,
        .started = 100,
        .finished = 200,
        .timed_out = false,
        .seq = 1,
    };
    result.deinit(allocator);
    // If deinit didn't free, the testing allocator would detect a leak
}

test "ExecResult deinit handles empty output" {
    const allocator = std.testing.allocator;
    var result = ExecResult{
        .exit_code = 0,
        .stdout = &[_]u8{},
        .stderr = &[_]u8{},
        .started = 100,
        .finished = 200,
        .timed_out = false,
        .seq = 1,
    };
    result.deinit(allocator);
}

test "createPipe returns valid fds" {
    if (@import("builtin").os.tag != .linux) return error.SkipZigTest;

    const pipe = createPipe() orelse return error.SkipZigTest;
    defer posix.close(pipe.read);
    defer posix.close(pipe.write);

    // Write to write end, read from read end
    const msg = "test pipe";
    _ = posix.write(pipe.write, msg) catch return error.SkipZigTest;
    var buf: [64]u8 = undefined;
    const n = posix.read(pipe.read, &buf) catch return error.SkipZigTest;
    try std.testing.expectEqualStrings(msg, buf[0..n]);
}

test "openNetns returns null for null name" {
    try std.testing.expectEqual(@as(?posix.fd_t, null), openNetns(null));
}

test "openNetns returns null for empty name" {
    try std.testing.expectEqual(@as(?posix.fd_t, null), openNetns(""));
}

test "openNetns returns null for nonexistent namespace" {
    // This should fail gracefully (log warning, return null)
    const result = openNetns("nonexistent-netns-12345");
    try std.testing.expectEqual(@as(?posix.fd_t, null), result);
}

test "SandboxContext defaults" {
    const ctx = SandboxContext{ .merged_path = "/tmp/merged" };
    try std.testing.expectEqual(@as(?[]const u8, null), ctx.netns_name);
    try std.testing.expectEqual(@as(?[]const u8, null), ctx.cgroup_path);
}

test "max_output_bytes is 64KB" {
    try std.testing.expectEqual(@as(usize, 65536), max_output_bytes);
}
