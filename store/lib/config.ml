(** Environment-based configuration for sq-store. *)

type t = {
  store_dir : string;  (** Irmin pack data directory *)
  sock_path : string;  (** Unix socket path *)
  data_dir : string;   (** sq-sandbox data directory (for sandbox paths) *)
  s3_bucket : string option;  (** S3 bucket for blob sync *)
  s3_endpoint : string option;  (** Custom S3 endpoint (R2/MinIO) *)
  s3_region : string;  (** S3 region *)
  s3_prefix : string;  (** S3 key prefix *)
}

let from_env () =
  let data_dir =
    try Sys.getenv "SQUASH_DATA" with Not_found -> "/data"
  in
  let store_dir =
    try Sys.getenv "SQUASH_STORE_DIR"
    with Not_found -> Filename.concat data_dir ".sq-store"
  in
  let sock_path =
    try Sys.getenv "SQUASH_STORE_SOCK"
    with Not_found -> Filename.concat data_dir ".sq-store.sock"
  in
  let s3_bucket =
    try
      let v = Sys.getenv "SQUASH_S3_BUCKET" in
      if v = "" then None else Some v
    with Not_found -> None
  in
  let s3_endpoint =
    try
      let v = Sys.getenv "SQUASH_S3_ENDPOINT" in
      if v = "" then None else Some v
    with Not_found -> None
  in
  let s3_region =
    try Sys.getenv "SQUASH_S3_REGION" with Not_found -> "us-east-1"
  in
  let s3_prefix =
    try Sys.getenv "SQUASH_S3_PREFIX" with Not_found -> ""
  in
  { store_dir; sock_path; data_dir; s3_bucket; s3_endpoint; s3_region; s3_prefix }
