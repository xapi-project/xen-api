type exnt = Unimplemented of string

val typ_of_exnt : exnt Rpc.Types.typ

val exnt : exnt Rpc.Types.def

exception DataExn of exnt

val error : exnt Idl.Error.t

val dbg : string Idl.Param.t

val unit : unit Idl.Param.t

val uri : string Rpc.Types.def
(** A URI representing the means for accessing the volume data. The
    interpretation of the URI is specific to the implementation. Xapi will
    choose which implementation to use based on the URI scheme. *)

(** List of blocks for copying. *)
type blocklist = {
    blocksize: int  (** Size of the individual blocks. *)
  ; ranges: (int64 * int64) list
        (** List of block ranges, where a range is a (start,length) pair,
          measured in units of [blocksize] *)
}

val blocklist : blocklist Rpc.Types.def
