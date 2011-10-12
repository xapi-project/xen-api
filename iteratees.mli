type err = string
type stream = Eof of err option | Chunk of string
val string_of_stream : stream -> string
module type MonadIO =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
module Iteratee :
  functor (IO : MonadIO) ->
    sig
      type 'a t =
          IE_done of 'a
        | IE_cont of err option * (stream -> ('a t * stream) IO.t)
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val ie_contM : (stream -> ('a t * stream) IO.t) -> stream -> ('a t * stream) IO.t
      val ie_doneM : 'a -> stream -> ('a t * stream) IO.t
      val ie_errM : err -> 'a t
      val peek : char option t
      val head : char option t
      val writer : (string -> 'a IO.t) -> unit t
      val break : (char -> bool) -> string t
      val heads : string -> int t
      val drop : int -> unit t
      val readn : int -> string t
      val read_int8 : int t
      val read_int16 : int t
      val read_int32 : int32 t
      val drop_while : (char -> bool) -> unit t
      val liftI : 'a t IO.t -> 'a t
      type 'a enumerator = 'a t -> 'a t IO.t
      val enum_eof : 'a t -> 'a t IO.t
      val enum_1chunk : string -> 'a t -> 'a t IO.t
      val enum_nchunk : string -> int -> 'a t -> 'a t IO.t
      val extract_result_from_iteratee : 'a t -> 'a
      type 'a enumeratee = 'a t -> 'a t t
      val take : int -> 'a t -> 'a t t
      val stream_printer : string -> 'a t -> 'a t t
      val modify : (string -> string) -> 'a t -> 'a t t
      type 'a either = Left of 'a | Right of 'a
      val read_lines : string list either t
    end
