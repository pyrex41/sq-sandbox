package sigv4_test

// Standalone test for SigV4 signing logic.
// Duplicates the core signing functions from src/sigv4.odin so we can test
// in isolation without requiring the full squashd package to compile.

import "core:crypto/hash"
import "core:crypto/hmac"
import "core:fmt"
import "core:strings"
import "core:testing"
import "core:time"

SHA256_DIGEST_SIZE :: 32

SIGNED_HEADERS :: "host;x-amz-content-sha256;x-amz-date"
EMPTY_SHA256 :: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

Sigv4_Credentials :: struct {
	access_key: string,
	secret_key: string,
}

Sigv4_Params :: struct {
	method:       string,
	path:         string,
	query:        string,
	host:         string,
	payload_hash: string,
	region:       string,
	service:      string,
}

Sigv4_Result :: struct {
	authorization: string,
	datetime:      string,
	datestamp:     string,
}

// ---------------------------------------------------------------------------
// Signing implementation (mirrors src/sigv4.odin)
// ---------------------------------------------------------------------------

hex_encode_buf :: proc(src: []byte, allocator := context.allocator) -> string {
	hex_table := [16]byte{
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
	}
	buf := make([]byte, len(src) * 2, allocator)
	for i in 0 ..< len(src) {
		buf[i * 2]     = hex_table[src[i] >> 4]
		buf[i * 2 + 1] = hex_table[src[i] & 0x0f]
	}
	return string(buf)
}

sha256_hex_string :: proc(data: string, allocator := context.allocator) -> string {
	digest: [SHA256_DIGEST_SIZE]byte
	hash.hash_string_to_buffer(.SHA256, data, digest[:])
	return hex_encode_buf(digest[:], allocator)
}

sha256_hex_bytes :: proc(data: []byte, allocator := context.allocator) -> string {
	digest: [SHA256_DIGEST_SIZE]byte
	hash.hash_bytes_to_buffer(.SHA256, data, digest[:])
	return hex_encode_buf(digest[:], allocator)
}

derive_signing_key :: proc(
	out: ^[SHA256_DIGEST_SIZE]byte,
	secret_key: string,
	datestamp: string,
	region: string,
	service: string,
) {
	prefix :: "AWS4"
	k_secret_buf: [260]byte
	k_secret_len := len(prefix) + len(secret_key)
	k_secret: []byte
	if k_secret_len <= len(k_secret_buf) {
		copy(k_secret_buf[:], prefix)
		copy(k_secret_buf[len(prefix):], secret_key)
		k_secret = k_secret_buf[:k_secret_len]
	} else {
		k_secret = make([]byte, k_secret_len, context.temp_allocator)
		copy(k_secret, prefix)
		copy(k_secret[len(prefix):], secret_key)
	}

	k_date: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_date[:], transmute([]byte)datestamp, k_secret)

	k_region: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_region[:], transmute([]byte)region, k_date[:])

	k_service: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_service[:], transmute([]byte)service, k_region[:])

	hmac.sum(.SHA256, out[:], transmute([]byte)string("aws4_request"), k_service[:])
}

format_datetime :: proc(t: time.Time, allocator := context.allocator) -> string {
	y, mon, d := time.date(t)
	h, min, s := time.clock(t)
	return fmt.aprintf(
		"%04d%02d%02dT%02d%02d%02dZ",
		y, int(mon), d, h, min, s,
		allocator = allocator,
	)
}

build_canonical_request :: proc(
	params: ^Sigv4_Params,
	datetime: string,
	allocator := context.allocator,
) -> string {
	return fmt.aprintf(
		"%s\n%s\n%s\nhost:%s\nx-amz-content-sha256:%s\nx-amz-date:%s\n\n%s\n%s",
		params.method,
		params.path,
		params.query,
		params.host,
		params.payload_hash,
		datetime,
		SIGNED_HEADERS,
		params.payload_hash,
		allocator = allocator,
	)
}

sigv4_sign :: proc(
	creds: ^Sigv4_Credentials,
	params: ^Sigv4_Params,
	t: time.Time,
	allocator := context.allocator,
) -> Sigv4_Result {
	datetime := format_datetime(t, allocator)
	datestamp := strings.clone(datetime[:8], allocator)

	scope := fmt.aprintf(
		"%s/%s/%s/aws4_request",
		datestamp, params.region, params.service,
		allocator = allocator,
	)

	canonical := build_canonical_request(params, datetime, allocator)
	canonical_hash := sha256_hex_string(canonical, allocator)

	string_to_sign := fmt.aprintf(
		"AWS4-HMAC-SHA256\n%s\n%s\n%s",
		datetime, scope, canonical_hash,
		allocator = allocator,
	)

	signing_key: [SHA256_DIGEST_SIZE]byte
	derive_signing_key(
		&signing_key,
		creds.secret_key,
		datestamp,
		params.region,
		params.service,
	)

	sig_bytes: [SHA256_DIGEST_SIZE]byte
	hmac.sum(
		.SHA256,
		sig_bytes[:],
		transmute([]byte)string_to_sign,
		signing_key[:],
	)
	signature := hex_encode_buf(sig_bytes[:], allocator)

	authorization := fmt.aprintf(
		"AWS4-HMAC-SHA256 Credential=%s/%s, SignedHeaders=%s, Signature=%s",
		creds.access_key, scope, SIGNED_HEADERS, signature,
		allocator = allocator,
	)

	return Sigv4_Result{
		authorization = authorization,
		datetime      = datetime,
		datestamp     = datestamp,
	}
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

@(test)
test_sha256_empty :: proc(t: ^testing.T) {
	result := sha256_hex_string("")
	defer delete(result)
	testing.expect_value(t, result, EMPTY_SHA256)
}

@(test)
test_sha256_known :: proc(t: ^testing.T) {
	result := sha256_hex_string("abc")
	defer delete(result)
	testing.expect_value(t, result, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
}

@(test)
test_sha256_hex_bytes :: proc(t: ^testing.T) {
	data := transmute([]byte)string("abc")
	result := sha256_hex_bytes(data)
	defer delete(result)
	testing.expect_value(t, result, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
}

@(test)
test_hmac_sha256_rfc4231 :: proc(t: ^testing.T) {
	// RFC 4231 Test Case 2:
	//   Key  = "Jefe"
	//   Data = "what do ya want for nothing?"
	//   HMAC = 5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843
	key := transmute([]byte)string("Jefe")
	data := transmute([]byte)string("what do ya want for nothing?")
	tag: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, tag[:], data, key)
	result := hex_encode_buf(tag[:])
	defer delete(result)
	testing.expect_value(t, result, "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
}

@(test)
test_derive_signing_key_aws_example :: proc(t: ^testing.T) {
	// From AWS Signature V4 Examples:
	//   Secret: wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY
	//   Date:   20120215
	//   Region: us-east-1
	//   Service: iam
	//   Expected: f4780e2d9f65fa895f9c67b32ce1baf0b0d8a43505a000a1a9e090d414db404d
	signing_key: [SHA256_DIGEST_SIZE]byte
	derive_signing_key(
		&signing_key,
		"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
		"20120215",
		"us-east-1",
		"iam",
	)
	result := hex_encode_buf(signing_key[:])
	defer delete(result)
	testing.expect_value(t, result, "f4780e2d9f65fa895f9c67b32ce1baf0b0d8a43505a000a1a9e090d414db404d")
}

@(test)
test_format_datetime :: proc(t: ^testing.T) {
	// 2013-05-24 00:00:00 UTC
	ts := time.Time{_nsec = 1369353600 * 1_000_000_000}
	result := format_datetime(ts)
	defer delete(result)
	testing.expect_value(t, result, "20130524T000000Z")
}

@(test)
test_format_datetime_with_time :: proc(t: ^testing.T) {
	// 2015-08-30 12:36:00 UTC
	ts := time.Time{_nsec = 1440938160 * 1_000_000_000}
	result := format_datetime(ts)
	defer delete(result)
	testing.expect_value(t, result, "20150830T123600Z")
}

@(test)
test_sigv4_sign_e2e :: proc(t: ^testing.T) {
	creds := Sigv4_Credentials{
		access_key = "AKIAIOSFODNN7EXAMPLE",
		secret_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
	}

	params := Sigv4_Params{
		method       = "GET",
		path         = "/test.txt",
		query        = "",
		host         = "examplebucket.s3.amazonaws.com",
		payload_hash = EMPTY_SHA256,
		region       = "us-east-1",
		service      = "s3",
	}

	// 2013-05-24 00:00:00 UTC
	ts := time.Time{_nsec = 1369353600 * 1_000_000_000}

	result := sigv4_sign(&creds, &params, ts)
	defer {
		delete(result.authorization)
		delete(result.datetime)
		delete(result.datestamp)
	}

	testing.expect_value(t, result.datetime, "20130524T000000Z")
	testing.expect_value(t, result.datestamp, "20130524")

	// Verify credential prefix.
	expected_prefix := "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request"
	ok := len(result.authorization) > len(expected_prefix) &&
	      result.authorization[:len(expected_prefix)] == expected_prefix
	testing.expectf(t, ok,
		"authorization should start with credential prefix, got: %s",
		result.authorization,
	)

	// Verify signature is 64 hex characters.
	sig_start :: "Signature="
	sig_idx := find_substring(result.authorization, sig_start)
	testing.expectf(t, sig_idx >= 0, "authorization should contain 'Signature='")

	if sig_idx >= 0 {
		sig := result.authorization[sig_idx + len(sig_start):]
		testing.expect_value(t, len(sig), 64)
		for c in sig {
			is_hex := (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
			testing.expectf(t, is_hex, "signature char '%c' is not hex", c)
		}
	}
}

@(test)
test_sigv4_sign_deterministic :: proc(t: ^testing.T) {
	creds := Sigv4_Credentials{
		access_key = "TESTKEY",
		secret_key = "TESTSECRET",
	}

	params := Sigv4_Params{
		method       = "PUT",
		path         = "/bucket/object.bin",
		query        = "",
		host         = "s3.us-west-2.amazonaws.com",
		payload_hash = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
		region       = "us-west-2",
		service      = "s3",
	}

	ts := time.Time{_nsec = 1700000000 * 1_000_000_000}

	result1 := sigv4_sign(&creds, &params, ts)
	result2 := sigv4_sign(&creds, &params, ts)
	defer {
		delete(result1.authorization)
		delete(result1.datetime)
		delete(result1.datestamp)
		delete(result2.authorization)
		delete(result2.datetime)
		delete(result2.datestamp)
	}

	testing.expect_value(t, result1.authorization, result2.authorization)
	testing.expect_value(t, result1.datetime, result2.datetime)
	testing.expect_value(t, result1.datestamp, result2.datestamp)
}

@(test)
test_canonical_request_format :: proc(t: ^testing.T) {
	params := Sigv4_Params{
		method       = "GET",
		path         = "/test.txt",
		query        = "",
		host         = "examplebucket.s3.amazonaws.com",
		payload_hash = EMPTY_SHA256,
		region       = "us-east-1",
		service      = "s3",
	}
	datetime := "20130524T000000Z"

	canonical := build_canonical_request(&params, datetime)
	defer delete(canonical)

	// Verify it contains the expected components.
	testing.expect(t, strings.contains(canonical, "GET\n"), "should start with method")
	testing.expect(t, strings.contains(canonical, "/test.txt\n"), "should contain path")
	testing.expect(t, strings.contains(canonical, "host:examplebucket.s3.amazonaws.com\n"), "should contain host header")
	testing.expect(t, strings.contains(canonical, "x-amz-date:20130524T000000Z\n"), "should contain date header")
	testing.expect(t, strings.contains(canonical, SIGNED_HEADERS), "should contain signed headers list")

	// Verify the hash is 64 hex chars.
	canonical_hash := sha256_hex_string(canonical)
	defer delete(canonical_hash)
	testing.expect_value(t, len(canonical_hash), 64)
}

// --- Helper ---

find_substring :: proc(haystack, needle: string) -> int {
	if len(needle) > len(haystack) do return -1
	for i in 0 ..= len(haystack) - len(needle) {
		if haystack[i:i + len(needle)] == needle do return i
	}
	return -1
}
