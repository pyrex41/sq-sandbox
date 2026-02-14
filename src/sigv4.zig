// sigv4.zig — AWS Signature V4 signing for S3.
//
// Implements the canonical request, string-to-sign, signing key derivation
// (HMAC chain: date → region → service → aws4_request), and final
// Authorization header generation using only Zig stdlib crypto.

const std = @import("std");
const Sha256 = std.crypto.hash.sha2.Sha256;
const HmacSha256 = std.crypto.auth.hmac.sha2.HmacSha256;

/// Result of signing a request.
pub const SignResult = struct {
    /// The full Authorization header value.
    authorization: [authorization_max_len]u8,
    authorization_len: usize,

    pub fn slice(self: *const SignResult) []const u8 {
        return self.authorization[0..self.authorization_len];
    }
};

// AWS4-HMAC-SHA256 Credential=<key>/<scope>, SignedHeaders=<hdrs>, Signature=<sig>
// Conservative upper bound: algorithm(17) + " Credential="(12) + access_key(128) + "/"(1)
// + scope(64) + ", SignedHeaders="(16) + signed_headers(512) + ", Signature="(13) + sig(64)
const authorization_max_len = 1024;

/// Sign an AWS request using Signature V4.
///
/// Parameters:
///   method       - HTTP method (e.g. "GET", "PUT")
///   uri          - URI path (e.g. "/test.txt"), already URI-encoded
///   query        - canonical query string (sorted, URI-encoded), empty string if none
///   headers      - slice of Header structs (will be sorted/normalized internally)
///   payload_hash - hex-encoded SHA-256 of the request body (64 chars)
///   access_key   - AWS access key ID
///   secret_key   - AWS secret access key
///   region       - AWS region (e.g. "us-east-1")
///   service      - AWS service (e.g. "s3")
///   datetime     - ISO 8601 timestamp "YYYYMMDDTHHMMSSZ" (16 chars)
pub fn sign(
    method: []const u8,
    uri: []const u8,
    query: []const u8,
    headers: []const Header,
    payload_hash: []const u8,
    access_key: []const u8,
    secret_key: []const u8,
    region: []const u8,
    service: []const u8,
    datetime: []const u8,
) SignResult {
    const datestamp = datetime[0..8]; // "YYYYMMDD"

    // 1. Build canonical request and hash it.
    var canon_buf: [4096]u8 = undefined;
    var signed_headers_buf: [512]u8 = undefined;
    const canonical = buildCanonicalRequest(
        &canon_buf,
        &signed_headers_buf,
        method,
        uri,
        query,
        headers,
        payload_hash,
    );
    const canonical_request = canonical.request;
    const signed_headers = canonical.signed_headers;

    var canon_hash: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(canonical_request, &canon_hash, .{});
    const canon_hash_hex = std.fmt.bytesToHex(&canon_hash, .lower);

    // 2. Build string to sign.
    var sts_buf: [256]u8 = undefined;
    const string_to_sign = std.fmt.bufPrint(&sts_buf, "AWS4-HMAC-SHA256\n{s}\n{s}/{s}/{s}/aws4_request\n{s}", .{
        datetime,
        datestamp,
        region,
        service,
        &canon_hash_hex,
    }) catch unreachable;

    // 3. Derive signing key.
    const signing_key = deriveSigningKey(secret_key, datestamp, region, service);

    // 4. Calculate signature.
    var signature_raw: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&signature_raw, string_to_sign, &signing_key);
    const signature_hex = std.fmt.bytesToHex(&signature_raw, .lower);

    // 5. Build Authorization header.
    var result = SignResult{
        .authorization = undefined,
        .authorization_len = 0,
    };
    const auth = std.fmt.bufPrint(&result.authorization, "AWS4-HMAC-SHA256 Credential={s}/{s}/{s}/{s}/aws4_request, SignedHeaders={s}, Signature={s}", .{
        access_key,
        datestamp,
        region,
        service,
        signed_headers,
        &signature_hex,
    }) catch unreachable;
    result.authorization_len = auth.len;

    return result;
}

pub const Header = struct {
    name: []const u8,
    value: []const u8,
};

const CanonicalResult = struct {
    request: []const u8,
    signed_headers: []const u8,
};

/// Build the canonical request string and signed headers list.
fn buildCanonicalRequest(
    buf: *[4096]u8,
    signed_buf: *[512]u8,
    method: []const u8,
    uri: []const u8,
    query: []const u8,
    headers: []const Header,
    payload_hash: []const u8,
) CanonicalResult {
    // Sort headers by lowercase name. Use a fixed-size array for sorting indices.
    var indices: [64]usize = undefined;
    for (0..headers.len) |i| {
        indices[i] = i;
    }
    const idx_slice = indices[0..headers.len];
    std.mem.sortUnstable(usize, idx_slice, headers, struct {
        pub fn lessThan(hdrs: []const Header, a: usize, b: usize) bool {
            return asciiLessThan(hdrs[a].name, hdrs[b].name);
        }
    }.lessThan);

    // Build signed headers string (lowercase, semicolon-separated).
    var sh_pos: usize = 0;
    for (idx_slice, 0..) |idx, i| {
        if (i > 0) {
            signed_buf[sh_pos] = ';';
            sh_pos += 1;
        }
        const name = headers[idx].name;
        for (name) |ch| {
            signed_buf[sh_pos] = std.ascii.toLower(ch);
            sh_pos += 1;
        }
    }
    const signed_headers = signed_buf[0..sh_pos];

    // Build canonical request.
    var pos: usize = 0;

    // Method
    @memcpy(buf[pos..][0..method.len], method);
    pos += method.len;
    buf[pos] = '\n';
    pos += 1;

    // URI
    @memcpy(buf[pos..][0..uri.len], uri);
    pos += uri.len;
    buf[pos] = '\n';
    pos += 1;

    // Query string
    @memcpy(buf[pos..][0..query.len], query);
    pos += query.len;
    buf[pos] = '\n';
    pos += 1;

    // Canonical headers (lowercase name, trimmed value, each followed by \n)
    for (idx_slice) |idx| {
        const name = headers[idx].name;
        for (name) |ch| {
            buf[pos] = std.ascii.toLower(ch);
            pos += 1;
        }
        buf[pos] = ':';
        pos += 1;
        const val = trimSpaces(headers[idx].value);
        @memcpy(buf[pos..][0..val.len], val);
        pos += val.len;
        buf[pos] = '\n';
        pos += 1;
    }

    // Empty line after headers
    buf[pos] = '\n';
    pos += 1;

    // Signed headers
    @memcpy(buf[pos..][0..signed_headers.len], signed_headers);
    pos += signed_headers.len;
    buf[pos] = '\n';
    pos += 1;

    // Payload hash
    @memcpy(buf[pos..][0..payload_hash.len], payload_hash);
    pos += payload_hash.len;

    return .{
        .request = buf[0..pos],
        .signed_headers = signed_headers,
    };
}

/// Derive the SigV4 signing key via HMAC chain.
///   kDate    = HMAC("AWS4" + secret, datestamp)
///   kRegion  = HMAC(kDate, region)
///   kService = HMAC(kRegion, service)
///   kSigning = HMAC(kService, "aws4_request")
pub fn deriveSigningKey(
    secret_key: []const u8,
    datestamp: []const u8,
    region: []const u8,
    service: []const u8,
) [HmacSha256.mac_length]u8 {
    // Build "AWS4" + secret_key
    var key_buf: [256]u8 = undefined;
    const prefix = "AWS4";
    @memcpy(key_buf[0..prefix.len], prefix);
    @memcpy(key_buf[prefix.len..][0..secret_key.len], secret_key);
    const full_key = key_buf[0 .. prefix.len + secret_key.len];

    var k_date: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&k_date, datestamp, full_key);

    var k_region: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&k_region, region, &k_date);

    var k_service: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&k_service, service, &k_region);

    var k_signing: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&k_signing, "aws4_request", &k_service);

    return k_signing;
}

/// SHA-256 hash a payload and return the hex-encoded digest.
pub fn hashPayload(payload: []const u8) [64]u8 {
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(payload, &digest, .{});
    return std.fmt.bytesToHex(&digest, .lower);
}

/// URI-encode a string per SigV4 rules.
/// Unreserved chars: A-Z a-z 0-9 - . _ ~
/// Everything else becomes %XX (uppercase hex).
/// If preserve_slash is true, '/' is not encoded (used for URI paths).
pub fn uriEncode(output: []u8, input: []const u8, preserve_slash: bool) []const u8 {
    var pos: usize = 0;
    for (input) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '.' or ch == '_' or ch == '~') {
            output[pos] = ch;
            pos += 1;
        } else if (ch == '/' and preserve_slash) {
            output[pos] = '/';
            pos += 1;
        } else {
            output[pos] = '%';
            output[pos + 1] = hexDigitUpper(@truncate(ch >> 4));
            output[pos + 2] = hexDigitUpper(@truncate(ch & 0x0f));
            pos += 3;
        }
    }
    return output[0..pos];
}

fn hexDigitUpper(nibble: u4) u8 {
    const n: u8 = nibble;
    return if (n < 10) '0' + n else 'A' + n - 10;
}

/// Case-insensitive less-than for ASCII strings.
fn asciiLessThan(a: []const u8, b: []const u8) bool {
    const min_len = @min(a.len, b.len);
    for (a[0..min_len], b[0..min_len]) |ac, bc| {
        const la = std.ascii.toLower(ac);
        const lb = std.ascii.toLower(bc);
        if (la < lb) return true;
        if (la > lb) return false;
    }
    return a.len < b.len;
}

/// Trim leading and trailing ASCII whitespace.
fn trimSpaces(s: []const u8) []const u8 {
    var start: usize = 0;
    while (start < s.len and s[start] == ' ') start += 1;
    var end: usize = s.len;
    while (end > start and s[end - 1] == ' ') end -= 1;
    return s[start..end];
}

// ── Tests ────────────────────────────────────────────────────────────

const testing = std.testing;

test "empty payload hash is correct" {
    const expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    const result = hashPayload("");
    try testing.expectEqualStrings(expected, &result);
}

test "sigv4 GET vanilla — AWS test suite" {
    // From: aws-sig-v4-test-suite/get-vanilla
    // Credentials
    const access_key = "AKIDEXAMPLE";
    const secret_key = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY";
    const region = "us-east-1";
    const service = "service";
    const datetime = "20150830T123600Z";

    const headers = [_]Header{
        .{ .name = "Host", .value = "example.amazonaws.com" },
        .{ .name = "X-Amz-Date", .value = "20150830T123600Z" },
    };

    const payload_hash = hashPayload("");

    const result = sign(
        "GET",
        "/",
        "",
        &headers,
        &payload_hash,
        access_key,
        secret_key,
        region,
        service,
        datetime,
    );

    const expected = "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request, SignedHeaders=host;x-amz-date, Signature=5fa00fa31553b73ebf1942676e86291e8372ff2a2260956d9b8aae1d763fbf31";
    try testing.expectEqualStrings(expected, result.slice());
}

test "sigv4 POST vanilla — AWS test suite" {
    // From: aws-sig-v4-test-suite/post-vanilla
    const access_key = "AKIDEXAMPLE";
    const secret_key = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY";
    const region = "us-east-1";
    const service = "service";
    const datetime = "20150830T123600Z";

    const headers = [_]Header{
        .{ .name = "Host", .value = "example.amazonaws.com" },
        .{ .name = "X-Amz-Date", .value = "20150830T123600Z" },
    };

    const payload_hash = hashPayload("");

    const result = sign(
        "POST",
        "/",
        "",
        &headers,
        &payload_hash,
        access_key,
        secret_key,
        region,
        service,
        datetime,
    );

    const expected = "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request, SignedHeaders=host;x-amz-date, Signature=5da7c1a2acd57cee7505fc6676e4e544621c30862966e37dddb68e92efbe5d6b";
    try testing.expectEqualStrings(expected, result.slice());
}

test "sigv4 S3 GET object — AWS S3 docs" {
    // From: AWS S3 SigV4 documentation example
    const access_key = "AKIAIOSFODNN7EXAMPLE";
    const secret_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY";
    const region = "us-east-1";
    const service = "s3";
    const datetime = "20130524T000000Z";

    const payload_hash = hashPayload("");

    const headers = [_]Header{
        .{ .name = "host", .value = "examplebucket.s3.amazonaws.com" },
        .{ .name = "range", .value = "bytes=0-9" },
        .{ .name = "x-amz-content-sha256", .value = &payload_hash },
        .{ .name = "x-amz-date", .value = "20130524T000000Z" },
    };

    const result = sign(
        "GET",
        "/test.txt",
        "",
        &headers,
        &payload_hash,
        access_key,
        secret_key,
        region,
        service,
        datetime,
    );

    const expected = "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41";
    try testing.expectEqualStrings(expected, result.slice());
}

test "signing key derivation" {
    // Verify that deriving the signing key produces expected intermediate HMAC values.
    // Test with known credentials from the AWS test suite.
    const secret_key = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY";
    const datestamp = "20150830";
    const region = "us-east-1";
    const service = "service";

    const key = deriveSigningKey(secret_key, datestamp, region, service);

    // The signing key should be 32 bytes (SHA-256 output).
    try testing.expectEqual(@as(usize, 32), key.len);

    // Verify by signing the known string-to-sign and checking the final signature.
    const string_to_sign = "AWS4-HMAC-SHA256\n20150830T123600Z\n20150830/us-east-1/service/aws4_request\nbb579772317eb040ac9ed261061d46c1f17a8133879d6129b6e1c25292927e63";

    var signature: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&signature, string_to_sign, &key);
    const sig_hex = std.fmt.bytesToHex(&signature, .lower);

    try testing.expectEqualStrings("5fa00fa31553b73ebf1942676e86291e8372ff2a2260956d9b8aae1d763fbf31", &sig_hex);
}

test "hashPayload with content" {
    // SHA-256 of "hello" = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    const result = hashPayload("hello");
    try testing.expectEqualStrings("2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824", &result);
}

test "uriEncode preserves unreserved characters" {
    var buf: [256]u8 = undefined;
    const result = uriEncode(&buf, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~", false);
    try testing.expectEqualStrings("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~", result);
}

test "uriEncode encodes special characters" {
    var buf: [256]u8 = undefined;
    const result = uriEncode(&buf, "hello world", false);
    try testing.expectEqualStrings("hello%20world", result);
}

test "uriEncode encodes slash when preserve_slash is false" {
    var buf: [256]u8 = undefined;
    const result = uriEncode(&buf, "/test/path", false);
    try testing.expectEqualStrings("%2Ftest%2Fpath", result);
}

test "uriEncode preserves slash when preserve_slash is true" {
    var buf: [256]u8 = undefined;
    const result = uriEncode(&buf, "/test/path", true);
    try testing.expectEqualStrings("/test/path", result);
}

test "uriEncode encodes at sign and colon" {
    var buf: [256]u8 = undefined;
    const result = uriEncode(&buf, "user@host:80", false);
    try testing.expectEqualStrings("user%40host%3A80", result);
}

test "trimSpaces removes leading and trailing spaces" {
    try testing.expectEqualStrings("hello", trimSpaces("  hello  "));
    try testing.expectEqualStrings("hello", trimSpaces("hello"));
    try testing.expectEqualStrings("hello world", trimSpaces("  hello world  "));
    try testing.expectEqualStrings("", trimSpaces("   "));
    try testing.expectEqualStrings("", trimSpaces(""));
}

test "asciiLessThan case insensitive" {
    try testing.expect(asciiLessThan("Host", "x-amz-date"));
    try testing.expect(asciiLessThan("host", "x-amz-date"));
    try testing.expect(!asciiLessThan("x-amz-date", "host"));
    try testing.expect(!asciiLessThan("host", "host"));
}
