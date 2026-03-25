(** Irmin-pack content-addressed store for sandbox snapshots. *)

module Hash : Irmin.Hash.S

module File_contents : sig
  type t = {
    mode : int;
    size : int;
    mtime : float;
    content : string;
  }

  val t : t Irmin.Type.t
  val merge : t option Irmin.Merge.t
end

module Store : Irmin.Generic_key.S
  with type contents = File_contents.t
   and type branch = string

type t = {
  repo : Store.Repo.t;
  config : Config.t;
}

val info : author:string -> string -> Store.info Lwt.t

val init : Config.t -> t Lwt.t

val branch : t -> string -> Store.t Lwt.t

val fork : t -> source_id:string -> target_id:string -> unit Lwt.t

val list_branches : t -> string list Lwt.t

val history : t -> string -> (string * string * string option * int) list Lwt.t
