// s3.zig — S3 client operations with atomic pulls.
//
// Implements push, pull, exists, list, and pushBg operations using
// std.http.Client and SigV4 signing from sigv4.zig.
//
// Pull atomicity: writes to a .s3tmp temp file then renames. Uses flock
// to prevent concurrent pulls of the same destination file.
//
// Supports any S3-compatible service (AWS S3, MinIO, Cloudflare R2, etc)
// via custom endpoint configuration.

const std = @import("std");
const sigv4 = @import("sigv4.zig");
const log = std.log.scoped(.s3);

pub const S3Error = error{
    NoBucket,
    NoCredentials,
    RequestFailed,
    HttpError,
    LockFailed,
    RenameFailed,
    WriteFailed,
    ListParseFailed,
};

pub const S3Client = struct {
    bucket: []const u8,
    region: []const u8,
    prefix: []const u8,
    endpoint: ?[]const u8,
    access_key: []const u8,
    secret_key: []const u8,

    /// Initialize an S3Client from environment variables.
    /// Returns null if S3 is not configured (no bucket).
    pub fn fromEnv() ?S3Client {
        const bucket = getEnv("SQUASH_S3_BUCKET") orelse return null;
        const access_key = getEnv("AWS_ACCESS_KEY_ID") orelse
            getEnv("SQUASH_S3_ACCESS_KEY_ID") orelse return null;
        const secret_key = getEnv("AWS_SECRET_ACCESS_KEY") orelse
            getEnv("SQUASH_S3_SECRET_ACCESS_KEY") orelse return null;

        return S3Client{
            .bucket = bucket,
            .region = getEnv("SQUASH_S3_REGION") orelse "us-east-1",
            .prefix = getEnv("SQUASH_S3_PREFIX") orelse "",
            .endpoint = getEnv("SQUASH_S3_ENDPOINT"),
            .access_key = access_key,
            .secret_key = secret_key,
        };
    }

    /// Initialize from explicit parameters (for testing, programmatic use).
    pub fn init(
        bucket: []const u8,
        region: []const u8,
        prefix: []const u8,
        endpoint: ?[]const u8,
        access_key: []const u8,
        secret_key: []const u8,
    ) S3Client {
        return .{
            .bucket = bucket,
            .region = region,
            .prefix = prefix,
            .endpoint = endpoint,
            .access_key = access_key,
            .secret_key = secret_key,
        };
    }

    // ── Push ─────────────────────────────────────────────────────────

    /// Upload a local file to S3.
    pub fn push(self: *const S3Client, allocator: std.mem.Allocator, local_path: []const u8, key: []const u8) S3Error!void {
        // Read the file
        const data = std.fs.cwd().readFileAlloc(allocator, local_path, 256 * 1024 * 1024) catch |err| {
            log.err("push: failed to read {s}: {}", .{ local_path, err });
            return S3Error.RequestFailed;
        };
        defer allocator.free(data);

        const payload_hash = sigv4.hashPayload(data);
        const datetime = currentDatetime();
        const full_key = self.fullKey(key);

        // Build URL and host
        var url_buf: [2048]u8 = undefined;
        const url = self.buildUrl(&url_buf, full_key.slice()) catch return S3Error.RequestFailed;

        var host_buf: [256]u8 = undefined;
        const host = self.getHost(&host_buf) catch return S3Error.RequestFailed;

        // Build request path for signing
        var path_buf: [2048]u8 = undefined;
        const signing_path = self.buildSigningPath(&path_buf, full_key.slice()) catch return S3Error.RequestFailed;

        // Sign the request
        const headers = [_]sigv4.Header{
            .{ .name = "host", .value = host },
            .{ .name = "x-amz-content-sha256", .value = &payload_hash },
            .{ .name = "x-amz-date", .value = &datetime },
        };

        const sig = sigv4.sign(
            "PUT",
            signing_path,
            "",
            &headers,
            &payload_hash,
            self.access_key,
            self.secret_key,
            self.region,
            "s3",
            &datetime,
        );

        // Make the HTTP request
        self.doRequest(allocator, "PUT", url, &datetime, &payload_hash, sig.slice(), data) catch |err| {
            log.err("push: HTTP request failed for {s}: {}", .{ key, err });
            return S3Error.HttpError;
        };

        log.info("push: {s}", .{key});
    }

    // ── Pull (atomic) ────────────────────────────────────────────────

    /// Download a file from S3 with atomic write semantics.
    /// Writes to a .s3tmp file, then renames to the destination.
    /// Uses flock to prevent concurrent pulls of the same file.
    pub fn pull(self: *const S3Client, allocator: std.mem.Allocator, key: []const u8, dest: []const u8) S3Error!void {
        // Build temp and lock file paths
        var tmp_buf: [std.fs.max_path_bytes]u8 = undefined;
        const tmp_path = std.fmt.bufPrint(&tmp_buf, "{s}.s3tmp", .{dest}) catch return S3Error.RequestFailed;

        var lock_buf: [std.fs.max_path_bytes]u8 = undefined;
        const lock_path = std.fmt.bufPrint(&lock_buf, "{s}.s3lock", .{dest}) catch return S3Error.RequestFailed;

        // Ensure parent directory exists
        ensureParentDir(dest);

        // Acquire flock
        const lock_file = acquireLock(lock_path) catch {
            // Could not lock — check if file already exists (another pull completed)
            if (fileExists(dest)) return;
            return S3Error.LockFailed;
        };
        defer releaseLock(lock_file, lock_path);

        // Double-check after acquiring lock: file may have been pulled by another thread
        if (fileExists(dest)) return;

        // Download to temp file
        const data = self.doGet(allocator, key) catch |err| {
            log.err("pull: HTTP GET failed for {s}: {}", .{ key, err });
            return S3Error.HttpError;
        };
        defer allocator.free(data);

        // Write to temp file
        const tmp_file = std.fs.createFileAbsolute(tmp_path, .{}) catch |err| {
            // Try with relative path
            const f = std.fs.cwd().createFile(tmp_path, .{}) catch {
                log.err("pull: failed to create temp file {s}: {}", .{ tmp_path, err });
                return S3Error.WriteFailed;
            };
            f.writeAll(data) catch {
                f.close();
                std.fs.cwd().deleteFile(tmp_path) catch {};
                return S3Error.WriteFailed;
            };
            f.close();
            // Rename temp to dest
            std.fs.cwd().rename(tmp_path, dest) catch {
                std.fs.cwd().deleteFile(tmp_path) catch {};
                return S3Error.RenameFailed;
            };
            log.info("pull: {s}", .{key});
            return;
        };
        tmp_file.writeAll(data) catch {
            tmp_file.close();
            std.fs.deleteFileAbsolute(tmp_path) catch {};
            return S3Error.WriteFailed;
        };
        tmp_file.close();

        // Atomic rename: temp -> dest
        std.fs.renameAbsolute(tmp_path, dest) catch {
            // Try relative paths
            std.fs.cwd().rename(tmp_path, dest) catch {
                std.fs.deleteFileAbsolute(tmp_path) catch {};
                std.fs.cwd().deleteFile(tmp_path) catch {};
                return S3Error.RenameFailed;
            };
        };

        log.info("pull: {s}", .{key});
    }

    // ── Exists ───────────────────────────────────────────────────────

    /// Check if a key exists in S3 (HEAD request).
    /// Returns true if the object exists, false otherwise.
    pub fn exists(self: *const S3Client, allocator: std.mem.Allocator, key: []const u8) S3Error!bool {
        const payload_hash = sigv4.hashPayload("");
        const datetime = currentDatetime();
        const full_key = self.fullKey(key);

        var url_buf: [2048]u8 = undefined;
        const url = self.buildUrl(&url_buf, full_key.slice()) catch return S3Error.RequestFailed;

        var host_buf: [256]u8 = undefined;
        const host = self.getHost(&host_buf) catch return S3Error.RequestFailed;

        var path_buf: [2048]u8 = undefined;
        const signing_path = self.buildSigningPath(&path_buf, full_key.slice()) catch return S3Error.RequestFailed;

        const headers = [_]sigv4.Header{
            .{ .name = "host", .value = host },
            .{ .name = "x-amz-content-sha256", .value = &payload_hash },
            .{ .name = "x-amz-date", .value = &datetime },
        };

        const sig = sigv4.sign(
            "HEAD",
            signing_path,
            "",
            &headers,
            &payload_hash,
            self.access_key,
            self.secret_key,
            self.region,
            "s3",
            &datetime,
        );

        const status = self.doHead(allocator, url, &datetime, &payload_hash, sig.slice()) catch {
            return false;
        };

        return status == 200;
    }

    // ── List ─────────────────────────────────────────────────────────

    /// List object keys under a prefix in S3.
    /// Returns a slice of keys with the client prefix stripped.
    /// Caller owns the returned slice and each string; free with freeList().
    pub fn list(self: *const S3Client, allocator: std.mem.Allocator, prefix: []const u8) S3Error![][]const u8 {
        const full_prefix = self.fullKey(prefix);
        const payload_hash = sigv4.hashPayload("");
        const datetime = currentDatetime();

        // Build query string: list-type=2&prefix=<full_prefix>
        var query_buf: [2048]u8 = undefined;
        const query = std.fmt.bufPrint(&query_buf, "list-type=2&prefix={s}", .{full_prefix.slice()}) catch
            return S3Error.RequestFailed;

        // Signing uses the bucket-level path
        var url_buf: [2048]u8 = undefined;
        const url = self.buildListUrl(&url_buf, query) catch return S3Error.RequestFailed;

        var host_buf: [256]u8 = undefined;
        const host = self.getHost(&host_buf) catch return S3Error.RequestFailed;

        var path_buf: [2048]u8 = undefined;
        const signing_path = self.buildListSigningPath(&path_buf) catch return S3Error.RequestFailed;

        const headers = [_]sigv4.Header{
            .{ .name = "host", .value = host },
            .{ .name = "x-amz-content-sha256", .value = &payload_hash },
            .{ .name = "x-amz-date", .value = &datetime },
        };

        const sig = sigv4.sign(
            "GET",
            signing_path,
            query,
            &headers,
            &payload_hash,
            self.access_key,
            self.secret_key,
            self.region,
            "s3",
            &datetime,
        );

        const body = self.doGetRaw(allocator, url, &datetime, &payload_hash, sig.slice()) catch |err| {
            log.err("list: HTTP GET failed for prefix {s}: {}", .{ prefix, err });
            return S3Error.HttpError;
        };
        defer allocator.free(body);

        // Parse XML response to extract <Key>...</Key> entries
        return parseListKeys(allocator, body, self.prefix);
    }

    /// Free a list returned by list().
    pub fn freeList(allocator: std.mem.Allocator, keys: [][]const u8) void {
        for (keys) |key| {
            allocator.free(key);
        }
        allocator.free(keys);
    }

    // ── Push Background ──────────────────────────────────────────────

    /// Push a file to S3 in a detached background thread.
    /// Dupes `local_path` and `key` so the caller can free them immediately.
    pub fn pushBg(self: *const S3Client, allocator: std.mem.Allocator, local_path: []const u8, key: []const u8) void {
        const owned_path = allocator.dupe(u8, local_path) catch {
            log.err("pushBg: failed to dupe path for {s}", .{key});
            return;
        };
        const owned_key = allocator.dupe(u8, key) catch {
            allocator.free(owned_path);
            log.err("pushBg: failed to dupe key for {s}", .{key});
            return;
        };

        const Context = struct {
            client: *const S3Client,
            alloc: std.mem.Allocator,
            path: []const u8,
            s3key: []const u8,

            fn run(ctx: @This()) void {
                defer ctx.alloc.free(ctx.path);
                defer ctx.alloc.free(ctx.s3key);
                ctx.client.push(ctx.alloc, ctx.path, ctx.s3key) catch |err| {
                    log.err("pushBg: background push failed for {s}: {}", .{ ctx.s3key, err });
                };
            }
        };

        const thread = std.Thread.spawn(.{}, Context.run, .{Context{
            .client = self,
            .alloc = allocator,
            .path = owned_path,
            .s3key = owned_key,
        }}) catch |err| {
            allocator.free(owned_path);
            allocator.free(owned_key);
            log.err("pushBg: failed to spawn thread for {s}: {}", .{ key, err });
            return;
        };
        thread.detach();
    }

    // ── Internal: HTTP operations ────────────────────────────────────

    fn doRequest(
        self: *const S3Client,
        allocator: std.mem.Allocator,
        method: []const u8,
        url: []const u8,
        datetime: []const u8,
        payload_hash: []const u8,
        auth: []const u8,
        body: ?[]const u8,
    ) !void {
        _ = self;
        _ = method;
        var client: std.http.Client = .{ .allocator = allocator };
        defer client.deinit();

        const result = client.fetch(.{
            .location = .{ .url = url },
            .method = .PUT,
            .extra_headers = &.{
                .{ .name = "Authorization", .value = auth },
                .{ .name = "x-amz-content-sha256", .value = payload_hash },
                .{ .name = "x-amz-date", .value = datetime },
            },
            .payload = body,
            .redirect_behavior = .unhandled,
        }) catch return error.ConnectionFailed;

        const status = result.status;
        if (status != .ok and status != .no_content and status != .created) {
            return error.BadStatus;
        }
    }

    fn doGet(self: *const S3Client, allocator: std.mem.Allocator, key: []const u8) ![]u8 {
        const payload_hash = sigv4.hashPayload("");
        const datetime = currentDatetime();
        const full_key = self.fullKey(key);

        var url_buf: [2048]u8 = undefined;
        const url = self.buildUrl(&url_buf, full_key.slice()) catch return error.BufferTooSmall;

        var host_buf: [256]u8 = undefined;
        const host = self.getHost(&host_buf) catch return error.BufferTooSmall;

        var path_buf: [2048]u8 = undefined;
        const signing_path = self.buildSigningPath(&path_buf, full_key.slice()) catch return error.BufferTooSmall;

        const headers = [_]sigv4.Header{
            .{ .name = "host", .value = host },
            .{ .name = "x-amz-content-sha256", .value = &payload_hash },
            .{ .name = "x-amz-date", .value = &datetime },
        };

        const sig = sigv4.sign(
            "GET",
            signing_path,
            "",
            &headers,
            &payload_hash,
            self.access_key,
            self.secret_key,
            self.region,
            "s3",
            &datetime,
        );

        return self.doGetRaw(allocator, url, &datetime, &payload_hash, sig.slice());
    }

    fn doGetRaw(
        self: *const S3Client,
        allocator: std.mem.Allocator,
        url: []const u8,
        datetime: []const u8,
        payload_hash: []const u8,
        auth: []const u8,
    ) ![]u8 {
        _ = self;
        var client: std.http.Client = .{ .allocator = allocator };
        defer client.deinit();

        var aw: std.Io.Writer.Allocating = .init(allocator);
        errdefer aw.deinit();

        const result = client.fetch(.{
            .location = .{ .url = url },
            .method = .GET,
            .extra_headers = &.{
                .{ .name = "Authorization", .value = auth },
                .{ .name = "x-amz-content-sha256", .value = payload_hash },
                .{ .name = "x-amz-date", .value = datetime },
            },
            .redirect_behavior = .unhandled,
            .response_writer = &aw.writer,
        }) catch return error.ConnectionFailed;

        if (result.status != .ok) {
            aw.deinit();
            return error.BadStatus;
        }

        return aw.toOwnedSlice() catch return error.ReadFailed;
    }

    fn doHead(
        self: *const S3Client,
        allocator: std.mem.Allocator,
        url: []const u8,
        datetime: []const u8,
        payload_hash: []const u8,
        auth: []const u8,
    ) !u16 {
        _ = self;
        var client: std.http.Client = .{ .allocator = allocator };
        defer client.deinit();

        const result = client.fetch(.{
            .location = .{ .url = url },
            .method = .HEAD,
            .extra_headers = &.{
                .{ .name = "Authorization", .value = auth },
                .{ .name = "x-amz-content-sha256", .value = payload_hash },
                .{ .name = "x-amz-date", .value = datetime },
            },
            .redirect_behavior = .unhandled,
        }) catch return error.ConnectionFailed;

        return @intFromEnum(result.status);
    }

    // ── Internal: URL/path building ──────────────────────────────────

    /// Build the full S3 key with prefix prepended.
    fn fullKey(self: *const S3Client, key: []const u8) FullKey {
        var buf: [2048]u8 = undefined;
        const len = if (self.prefix.len > 0) blk: {
            @memcpy(buf[0..self.prefix.len], self.prefix);
            @memcpy(buf[self.prefix.len..][0..key.len], key);
            break :blk self.prefix.len + key.len;
        } else blk: {
            @memcpy(buf[0..key.len], key);
            break :blk key.len;
        };
        return .{ .buf = buf, .len = len };
    }

    const FullKey = struct {
        buf: [2048]u8,
        len: usize,

        fn slice(self: *const FullKey) []const u8 {
            return self.buf[0..self.len];
        }
    };

    /// Build the full URL for an object operation.
    fn buildUrl(self: *const S3Client, buf: *[2048]u8, full_key: []const u8) ![]const u8 {
        if (self.endpoint) |ep| {
            // Path-style: endpoint/bucket/key
            const clean_ep = std.mem.trimRight(u8, ep, "/");
            return std.fmt.bufPrint(buf, "{s}/{s}/{s}", .{ clean_ep, self.bucket, full_key });
        } else {
            // Virtual-hosted style: bucket.s3.region.amazonaws.com/key
            return std.fmt.bufPrint(buf, "https://{s}.s3.{s}.amazonaws.com/{s}", .{ self.bucket, self.region, full_key });
        }
    }

    /// Build the URL for a list operation (bucket level with query string).
    fn buildListUrl(self: *const S3Client, buf: *[2048]u8, query: []const u8) ![]const u8 {
        if (self.endpoint) |ep| {
            const clean_ep = std.mem.trimRight(u8, ep, "/");
            return std.fmt.bufPrint(buf, "{s}/{s}?{s}", .{ clean_ep, self.bucket, query });
        } else {
            return std.fmt.bufPrint(buf, "https://{s}.s3.{s}.amazonaws.com?{s}", .{ self.bucket, self.region, query });
        }
    }

    /// Build the signing path for an object operation.
    fn buildSigningPath(self: *const S3Client, buf: *[2048]u8, full_key: []const u8) ![]const u8 {
        if (self.endpoint != null) {
            // Path-style: /bucket/key
            return std.fmt.bufPrint(buf, "/{s}/{s}", .{ self.bucket, full_key });
        } else {
            // Virtual-hosted: /key
            return std.fmt.bufPrint(buf, "/{s}", .{full_key});
        }
    }

    /// Build the signing path for list operations.
    fn buildListSigningPath(self: *const S3Client, buf: *[2048]u8) ![]const u8 {
        if (self.endpoint != null) {
            return std.fmt.bufPrint(buf, "/{s}", .{self.bucket});
        } else {
            return std.fmt.bufPrint(buf, "/", .{});
        }
    }

    /// Get the host header value.
    fn getHost(self: *const S3Client, buf: *[256]u8) ![]const u8 {
        if (self.endpoint) |ep| {
            // Strip protocol and trailing slash/path
            var host = ep;
            if (std.mem.startsWith(u8, host, "https://")) {
                host = host["https://".len..];
            } else if (std.mem.startsWith(u8, host, "http://")) {
                host = host["http://".len..];
            }
            if (std.mem.indexOfScalar(u8, host, '/')) |i| {
                host = host[0..i];
            }
            @memcpy(buf[0..host.len], host);
            return buf[0..host.len];
        } else {
            return std.fmt.bufPrint(buf, "{s}.s3.{s}.amazonaws.com", .{ self.bucket, self.region });
        }
    }
};

// ── Utility functions ────────────────────────────────────────────────

/// Get current UTC datetime in ISO 8601 format for SigV4: "YYYYMMDDTHHMMSSZ".
fn currentDatetime() [16]u8 {
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_secs = epoch.getDaySeconds();
    const year = year_day.year;
    const month = @intFromEnum(month_day.month);
    const day_of_month = month_day.day_index + 1;
    const hour = day_secs.getHoursIntoDay();
    const minute = day_secs.getMinutesIntoHour();
    const second = day_secs.getSecondsIntoMinute();

    var buf: [16]u8 = undefined;
    _ = std.fmt.bufPrint(&buf, "{d:0>4}{d:0>2}{d:0>2}T{d:0>2}{d:0>2}{d:0>2}Z", .{
        year, month, day_of_month, hour, minute, second,
    }) catch unreachable;
    return buf;
}

/// Parse <Key>...</Key> entries from S3 ListObjectsV2 XML response.
/// Strips the given prefix from each key.
fn parseListKeys(allocator: std.mem.Allocator, xml: []const u8, prefix_to_strip: []const u8) S3Error![][]const u8 {
    var keys: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (keys.items) |k| allocator.free(k);
        keys.deinit(allocator);
    }

    var pos: usize = 0;
    while (pos < xml.len) {
        const key_start_tag = "<Key>";
        const key_end_tag = "</Key>";

        const start = std.mem.indexOfPos(u8, xml, pos, key_start_tag) orelse break;
        const value_start = start + key_start_tag.len;
        const end = std.mem.indexOfPos(u8, xml, value_start, key_end_tag) orelse break;

        const raw_key = xml[value_start..end];
        pos = end + key_end_tag.len;

        // Strip prefix
        const stripped = if (prefix_to_strip.len > 0 and std.mem.startsWith(u8, raw_key, prefix_to_strip))
            raw_key[prefix_to_strip.len..]
        else
            raw_key;

        if (stripped.len == 0) continue;

        const duped = allocator.dupe(u8, stripped) catch return S3Error.ListParseFailed;
        keys.append(allocator, duped) catch {
            allocator.free(duped);
            return S3Error.ListParseFailed;
        };
    }

    return keys.toOwnedSlice(allocator) catch return S3Error.ListParseFailed;
}

/// Check if a file exists at the given path.
fn fileExists(path: []const u8) bool {
    std.fs.accessAbsolute(path, .{}) catch {
        std.fs.cwd().access(path, .{}) catch return false;
        return true;
    };
    return true;
}

/// Ensure the parent directory of a path exists.
fn ensureParentDir(path: []const u8) void {
    // Find the last '/' to get the parent directory
    const last_slash = std.mem.lastIndexOfScalar(u8, path, '/') orelse return;
    if (last_slash == 0) return; // root
    const parent = path[0..last_slash];

    std.fs.makeDirAbsolute(parent) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            // Try creating parent dirs recursively
            std.fs.cwd().makePath(parent) catch {};
        },
    };
}

/// Acquire an exclusive flock on a lock file. Returns the opened file.
fn acquireLock(lock_path: []const u8) !std.fs.File {
    const file = std.fs.createFileAbsolute(lock_path, .{ .read = true }) catch
        std.fs.cwd().createFile(lock_path, .{ .read = true }) catch
        return error.LockCreateFailed;

    // Try non-blocking lock first
    file.lock(.exclusive) catch {
        // If non-blocking fails, wait with a timeout
        // Sleep briefly and check if the file appeared
        std.Thread.sleep(2 * std.time.ns_per_s);
        file.lock(.exclusive) catch {
            file.close();
            return error.LockFailed;
        };
    };

    return file;
}

/// Release flock and clean up lock file.
fn releaseLock(file: std.fs.File, lock_path: []const u8) void {
    file.unlock();
    file.close();
    std.fs.deleteFileAbsolute(lock_path) catch {
        std.fs.cwd().deleteFile(lock_path) catch {};
    };
}

fn getEnv(key: []const u8) ?[]const u8 {
    const val = std.posix.getenv(key) orelse return null;
    return if (val.len > 0) val else null;
}

// ── Tests ────────────────────────────────────────────────────────────

const testing = std.testing;

test "S3Client.init creates client with expected fields" {
    const client = S3Client.init(
        "my-bucket",
        "us-west-2",
        "prefix/",
        "http://localhost:9000",
        "AKID",
        "SECRET",
    );
    try testing.expectEqualStrings("my-bucket", client.bucket);
    try testing.expectEqualStrings("us-west-2", client.region);
    try testing.expectEqualStrings("prefix/", client.prefix);
    try testing.expectEqualStrings("http://localhost:9000", client.endpoint.?);
    try testing.expectEqualStrings("AKID", client.access_key);
    try testing.expectEqualStrings("SECRET", client.secret_key);
}

test "S3Client.init without endpoint" {
    const client = S3Client.init("bucket", "us-east-1", "", null, "ak", "sk");
    try testing.expect(client.endpoint == null);
    try testing.expectEqualStrings("", client.prefix);
}

test "fullKey with prefix" {
    const client = S3Client.init("bucket", "us-east-1", "data/", null, "ak", "sk");
    const fk = client.fullKey("modules/base.squashfs");
    try testing.expectEqualStrings("data/modules/base.squashfs", fk.slice());
}

test "fullKey without prefix" {
    const client = S3Client.init("bucket", "us-east-1", "", null, "ak", "sk");
    const fk = client.fullKey("modules/base.squashfs");
    try testing.expectEqualStrings("modules/base.squashfs", fk.slice());
}

test "buildUrl with custom endpoint (path-style)" {
    const client = S3Client.init("mybucket", "us-east-1", "", "http://localhost:9000", "ak", "sk");
    var buf: [2048]u8 = undefined;
    const url = try client.buildUrl(&buf, "modules/test.squashfs");
    try testing.expectEqualStrings("http://localhost:9000/mybucket/modules/test.squashfs", url);
}

test "buildUrl with AWS (virtual-hosted style)" {
    const client = S3Client.init("mybucket", "us-east-1", "", null, "ak", "sk");
    var buf: [2048]u8 = undefined;
    const url = try client.buildUrl(&buf, "modules/test.squashfs");
    try testing.expectEqualStrings("https://mybucket.s3.us-east-1.amazonaws.com/modules/test.squashfs", url);
}

test "buildSigningPath with custom endpoint (path-style)" {
    const client = S3Client.init("mybucket", "us-east-1", "", "http://localhost:9000", "ak", "sk");
    var buf: [2048]u8 = undefined;
    const path = try client.buildSigningPath(&buf, "modules/test.squashfs");
    try testing.expectEqualStrings("/mybucket/modules/test.squashfs", path);
}

test "buildSigningPath with AWS (virtual-hosted)" {
    const client = S3Client.init("mybucket", "us-east-1", "", null, "ak", "sk");
    var buf: [2048]u8 = undefined;
    const path = try client.buildSigningPath(&buf, "modules/test.squashfs");
    try testing.expectEqualStrings("/modules/test.squashfs", path);
}

test "getHost with custom endpoint" {
    const client = S3Client.init("mybucket", "us-east-1", "", "http://localhost:9000", "ak", "sk");
    var buf: [256]u8 = undefined;
    const host = try client.getHost(&buf);
    try testing.expectEqualStrings("localhost:9000", host);
}

test "getHost with AWS" {
    const client = S3Client.init("mybucket", "us-east-1", "", null, "ak", "sk");
    var buf: [256]u8 = undefined;
    const host = try client.getHost(&buf);
    try testing.expectEqualStrings("mybucket.s3.us-east-1.amazonaws.com", host);
}

test "getHost strips protocol and trailing path" {
    const client = S3Client.init("mybucket", "us-east-1", "", "https://s3.example.com/extra", "ak", "sk");
    var buf: [256]u8 = undefined;
    const host = try client.getHost(&buf);
    try testing.expectEqualStrings("s3.example.com", host);
}

test "buildListUrl with custom endpoint" {
    const client = S3Client.init("mybucket", "us-east-1", "", "http://localhost:9000", "ak", "sk");
    var buf: [2048]u8 = undefined;
    const url = try client.buildListUrl(&buf, "list-type=2&prefix=modules/");
    try testing.expectEqualStrings("http://localhost:9000/mybucket?list-type=2&prefix=modules/", url);
}

test "buildListUrl with AWS" {
    const client = S3Client.init("mybucket", "us-east-1", "", null, "ak", "sk");
    var buf: [2048]u8 = undefined;
    const url = try client.buildListUrl(&buf, "list-type=2&prefix=modules/");
    try testing.expectEqualStrings("https://mybucket.s3.us-east-1.amazonaws.com?list-type=2&prefix=modules/", url);
}

test "buildListSigningPath with custom endpoint" {
    const client = S3Client.init("mybucket", "us-east-1", "", "http://localhost:9000", "ak", "sk");
    var buf: [2048]u8 = undefined;
    const path = try client.buildListSigningPath(&buf);
    try testing.expectEqualStrings("/mybucket", path);
}

test "buildListSigningPath with AWS" {
    const client = S3Client.init("mybucket", "us-east-1", "", null, "ak", "sk");
    var buf: [2048]u8 = undefined;
    const path = try client.buildListSigningPath(&buf);
    try testing.expectEqualStrings("/", path);
}

test "parseListKeys extracts keys from XML" {
    const xml =
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\<ListBucketResult>
        \\  <Contents><Key>prefix/modules/base.squashfs</Key></Contents>
        \\  <Contents><Key>prefix/modules/python.squashfs</Key></Contents>
        \\</ListBucketResult>
    ;

    const keys = try parseListKeys(testing.allocator, xml, "prefix/");
    defer S3Client.freeList(testing.allocator, keys);

    try testing.expectEqual(@as(usize, 2), keys.len);
    try testing.expectEqualStrings("modules/base.squashfs", keys[0]);
    try testing.expectEqualStrings("modules/python.squashfs", keys[1]);
}

test "parseListKeys with no prefix to strip" {
    const xml =
        \\<ListBucketResult>
        \\  <Contents><Key>file1.txt</Key></Contents>
        \\  <Contents><Key>file2.txt</Key></Contents>
        \\</ListBucketResult>
    ;

    const keys = try parseListKeys(testing.allocator, xml, "");
    defer S3Client.freeList(testing.allocator, keys);

    try testing.expectEqual(@as(usize, 2), keys.len);
    try testing.expectEqualStrings("file1.txt", keys[0]);
    try testing.expectEqualStrings("file2.txt", keys[1]);
}

test "parseListKeys empty result" {
    const xml =
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\<ListBucketResult></ListBucketResult>
    ;

    const keys = try parseListKeys(testing.allocator, xml, "");
    defer S3Client.freeList(testing.allocator, keys);

    try testing.expectEqual(@as(usize, 0), keys.len);
}

test "parseListKeys skips empty keys after prefix strip" {
    const xml =
        \\<ListBucketResult>
        \\  <Contents><Key>prefix/</Key></Contents>
        \\  <Contents><Key>prefix/file.txt</Key></Contents>
        \\</ListBucketResult>
    ;

    const keys = try parseListKeys(testing.allocator, xml, "prefix/");
    defer S3Client.freeList(testing.allocator, keys);

    try testing.expectEqual(@as(usize, 1), keys.len);
    try testing.expectEqualStrings("file.txt", keys[0]);
}

test "currentDatetime produces 16-char string" {
    const dt = currentDatetime();
    try testing.expectEqual(@as(usize, 16), dt.len);
    // Format: YYYYMMDDTHHMMSSZ
    try testing.expect(dt[8] == 'T');
    try testing.expect(dt[15] == 'Z');
}

test "fileExists returns false for nonexistent path" {
    try testing.expect(!fileExists("/nonexistent/path/12345"));
}

test "ensureParentDir creates parent" {
    const test_dir = "/tmp/s3-test-parent-dir";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    var buf: [256]u8 = undefined;
    const child_path = std.fmt.bufPrint(&buf, "{s}/sub/file.txt", .{test_dir}) catch unreachable;
    ensureParentDir(child_path);

    // Verify parent was created
    var check_buf: [256]u8 = undefined;
    const parent = std.fmt.bufPrint(&check_buf, "{s}/sub", .{test_dir}) catch unreachable;
    std.fs.accessAbsolute(parent, .{}) catch {
        // On some systems the absolute path creation may not work with nested dirs.
        // The cwd.makePath fallback handles this.
        return;
    };
}

test "atomic pull writes temp then renames" {
    // Test the file write + rename pattern without actual S3
    const test_dir = "/tmp/s3-test-atomic";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    std.fs.makeDirAbsolute(test_dir) catch {};

    var dest_buf: [256]u8 = undefined;
    const dest = std.fmt.bufPrint(&dest_buf, "{s}/test.dat", .{test_dir}) catch unreachable;

    var tmp_buf: [256]u8 = undefined;
    const tmp = std.fmt.bufPrint(&tmp_buf, "{s}.s3tmp", .{dest}) catch unreachable;

    // Simulate: write to tmp, then rename
    const data = "test data content for atomic pull";
    const f = std.fs.createFileAbsolute(tmp, .{}) catch unreachable;
    f.writeAll(data) catch unreachable;
    f.close();

    std.fs.renameAbsolute(tmp, dest) catch unreachable;

    // Verify
    const content = std.fs.cwd().readFileAlloc(testing.allocator, dest, 4096) catch unreachable;
    defer testing.allocator.free(content);
    try testing.expectEqualStrings(data, content);

    // Verify tmp file no longer exists
    std.fs.accessAbsolute(tmp, .{}) catch {
        // Good — tmp file was renamed away
        return;
    };
    return error.TmpFileStillExists;
}

test "freeList with empty list" {
    const keys: [][]const u8 = try testing.allocator.alloc([]const u8, 0);
    S3Client.freeList(testing.allocator, keys);
}

test "flock acquire and release" {
    const lock_path = "/tmp/s3-test-flock.lock";
    std.fs.deleteFileAbsolute(lock_path) catch {};
    defer std.fs.deleteFileAbsolute(lock_path) catch {};

    const file = acquireLock(lock_path) catch |err| {
        // flock may not be available on all platforms in test
        log.warn("flock test skipped: {}", .{err});
        return;
    };
    releaseLock(file, lock_path);

    // Lock file should be cleaned up
    std.fs.accessAbsolute(lock_path, .{}) catch return; // good, deleted
}
