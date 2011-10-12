
val really_write : Lwt_unix.file_descr -> string -> unit Lwt.t
module LwtIteratee :
  sig
    type 'a t =
      'a Iteratees.Iteratee(Lwt).t =
        IE_done of 'a
      | IE_cont of Iteratees.err option *
          (Iteratees.stream -> ('a t * Iteratees.stream) Lwt.t)
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ie_contM :
      (Iteratees.stream -> ('a t * Iteratees.stream) Lwt.t) -> Iteratees.stream ->
      ('a t * Iteratees.stream) Lwt.t
    val ie_doneM : 'a -> Iteratees.stream -> ('a t * Iteratees.stream) Lwt.t
    val ie_errM : Iteratees.err -> 'a t
    val peek : char option t
    val head : char option t
    val writer : (string -> 'a Lwt.t) -> unit t
    val break : (char -> bool) -> string t
    val heads : string -> int t
    val drop : int -> unit t
    val readn : int -> string t
    val read_int8 : int t
    val read_int16 : int t
    val read_int32 : int32 t
    val drop_while : (char -> bool) -> unit t
    val liftI : 'a t Lwt.t -> 'a t
    type 'a enumerator = 'a t -> 'a t Lwt.t
    val enum_eof : 'a t -> 'a t Lwt.t
    val enum_1chunk : string -> 'a t -> 'a t Lwt.t
    val enum_nchunk : string -> int -> 'a t -> 'a t Lwt.t
    val extract_result_from_iteratee : 'a t -> 'a
    type 'a enumeratee = 'a t -> 'a t t
    val take : int -> 'a t -> 'a t t
    val stream_printer : string -> 'a t -> 'a t t
    val modify : (string -> string) -> 'a t -> 'a t t
    type 'a either =
      'a Iteratees.Iteratee(Lwt).either =
        Left of 'a
      | Right of 'a
    val read_lines : string list either t
  end
val really_write : Lwt_unix.file_descr -> string -> unit Lwt.t
val lwt_fd_enumerator :
  Lwt_unix.file_descr -> 'a LwtIteratee.t -> 'a LwtIteratee.t Lwt.t
val lwt_enumerator : string -> 'a LwtIteratee.t -> 'a LwtIteratee.t Lwt.t
exception Host_not_found of string
val open_connection_fd : string -> int -> Lwt_unix.file_descr Lwt.t

