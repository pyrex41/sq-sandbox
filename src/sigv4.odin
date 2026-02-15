package squashd

// ---------------------------------------------------------------------------
// AWS Signature V4 signing
//
// Implements the SigV4 algorithm used for authenticating requests to S3 and
// other AWS services.  Ported from the shell implementation in bin/sq-s3.
//
// References:
//   https://docs.aws.amazon.com/IAM/latest/UserGuide/create-signed-request.html
//   https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
// ---------------------------------------------------------------------------

import "core:crypto/hash"
import "core:crypto/hmac"
import "core:fmt"
import "core:strings"
import "core:time"

SHA256_DIGEST_SIZE :: 32
SIGNED_HEADERS :: "host;x-amz-content-sha256;x-amz-date"

// Credentials for AWS SigV4 signing.
Sigv4_Credentials :: struct {
	access_key: string,
	secret_key: string,
}

// Parameters for a single SigV4 signing operation.
Sigv4_Params :: struct {
	method:       string, // "GET", "PUT", "HEAD", "DELETE"
	path:         string, // URI path component, e.g. "/bucket/key"
	query:        string, // Query string without leading '?', empty if none
	host:         string, // Host header value
	payload_hash: string, // Hex-encoded SHA256 of the payload
	region:       string,
	service:      string, // Typically "s3"
}

// Result of signing — contains all values needed for the HTTP request.
Sigv4_Result :: struct {
	authorization: string, // Full Authorization header value
	datetime:      string, // ISO8601 x-amz-date value (YYYYMMDDTHHmmSSZ)
	datestamp:     string, // Date portion (YYYYMMDD)
}

// EMPTY_SHA256 is the hex-encoded SHA256 hash of an empty string.
// Pre-computed to avoid repeated hashing for GET/HEAD/DELETE requests.
EMPTY_SHA256 :: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

// sigv4_sign computes the AWS Signature V4 for the given request parameters.
// All returned strings are allocated with the provided allocator.
sigv4_sign :: proc(
	creds: ^Sigv4_Credentials,
	params: ^Sigv4_Params,
	t: time.Time,
	allocator := context.allocator,
) -> Sigv4_Result {
	datetime := _format_datetime(t, allocator)
	datestamp := strings.clone(datetime[:8], allocator)

	scope := fmt.aprintf(
		"%s/%s/%s/aws4_request",
		datestamp, params.region, params.service,
		allocator = allocator,
	)

	canonical := _build_canonical_request(params, datetime, allocator)

	canonical_hash := _sha256_hex_string(canonical, allocator)

	string_to_sign := fmt.aprintf(
		"AWS4-HMAC-SHA256\n%s\n%s\n%s",
		datetime, scope, canonical_hash,
		allocator = allocator,
	)

	signing_key: [SHA256_DIGEST_SIZE]byte
	_derive_signing_key(
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
	signature := _hex_encode_buf(sig_bytes[:], allocator)

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

// sigv4_sha256_hex computes SHA256 of the given data and returns the
// lowercase hex-encoded digest string.
sigv4_sha256_hex :: proc(data: string, allocator := context.allocator) -> string {
	return _sha256_hex_string(data, allocator)
}

// sigv4_sha256_hex_bytes computes SHA256 of the given byte slice and returns
// the lowercase hex-encoded digest string.
sigv4_sha256_hex_bytes :: proc(data: []byte, allocator := context.allocator) -> string {
	digest: [SHA256_DIGEST_SIZE]byte
	hash.hash_bytes_to_buffer(.SHA256, data, digest[:])
	return _hex_encode_buf(digest[:], allocator)
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// _sha256_hex_string computes SHA256 of a string, returning a heap-allocated hex string.
@(private)
_sha256_hex_string :: proc(data: string, allocator := context.allocator) -> string {
	digest: [SHA256_DIGEST_SIZE]byte
	hash.hash_string_to_buffer(.SHA256, data, digest[:])
	return _hex_encode_buf(digest[:], allocator)
}

// _hex_encode_buf returns a heap-allocated hex string from a byte slice.
@(private)
_hex_encode_buf :: proc(src: []byte, allocator := context.allocator) -> string {
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

// _build_canonical_request constructs the canonical request string per the
// SigV4 specification.
//
// Format:
//   HTTPMethod\n
//   CanonicalURI\n
//   CanonicalQueryString\n
//   CanonicalHeaders\n         (each header "name:value\n")
//   \n                         (blank line after headers)
//   SignedHeaders\n
//   HashedPayload
@(private)
_build_canonical_request :: proc(
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

// _derive_signing_key performs the HMAC-SHA256 chain to derive the signing key:
//   kDate    = HMAC("AWS4" + secret, datestamp)
//   kRegion  = HMAC(kDate, region)
//   kService = HMAC(kRegion, service)
//   kSigning = HMAC(kService, "aws4_request")
@(private)
_derive_signing_key :: proc(
	out: ^[SHA256_DIGEST_SIZE]byte,
	secret_key: string,
	datestamp: string,
	region: string,
	service: string,
) {
	// Build "AWS4" + secret_key on the stack if small enough.
	prefix :: "AWS4"
	k_secret_buf: [260]byte // "AWS4" (4) + up to 256-byte secret key
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

	// Chain: each step uses the previous HMAC output as the key.
	k_date: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_date[:], transmute([]byte)datestamp, k_secret)

	k_region: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_region[:], transmute([]byte)region, k_date[:])

	k_service: [SHA256_DIGEST_SIZE]byte
	hmac.sum(.SHA256, k_service[:], transmute([]byte)service, k_region[:])

	hmac.sum(.SHA256, out[:], transmute([]byte)string("aws4_request"), k_service[:])
}

// _format_datetime formats a time.Time as "YYYYMMDDTHHmmSSZ" for SigV4.
@(private)
_format_datetime :: proc(t: time.Time, allocator := context.allocator) -> string {
	y, mon, d := time.date(t)
	h, min, s := time.clock(t)
	return fmt.aprintf(
		"%04d%02d%02dT%02d%02d%02dZ",
		y, int(mon), d, h, min, s,
		allocator = allocator,
	)
}
