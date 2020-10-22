(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Iteratees module interface file 

    Refer to http://okmij.org/ftp/Haskell/Iteratee/ for additional information
    on the context and the design. *)

(** An error message type *)
type err = string

(** The stream type is the stuff provided by enumerators to give to iteratees *)
type stream = Eof of err option | Chunk of string

val string_of_stream : stream -> string

(** The most basic Monad module definition. *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Iteratee module, functorized over a monad module *)
module Iteratee : functor (IO : Monad) -> sig
  (** The type t describes the current state of the iteratee.
        It's either 'Done', in which case it's got some sort of
        value, or it's in the 'Cont' state, which mean's it
        hasn't finished processing - in this case it may be in
        an error state, or it may be awaiting more input.
    *)

  type 'a t =
    | IE_done of 'a
    | IE_cont of err option * (stream -> ('a t * stream) IO.t)

  val return : 'a -> 'a t
  (** Iteratee is itself a monad, with the usual return, bind *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ie_contM : (stream -> ('a t * stream) IO.t) -> 'b -> ('a t * 'b) IO.t

  val ie_doneM : 'a -> 'b -> ('a t * 'b) IO.t

  val ie_errM :
    err -> (stream -> ('a t * stream) IO.t) -> 'b -> ('a t * 'b) IO.t

  val state : 'a t -> string
  (** Return a string representation of the state of the monad *)

  val peek : char option t
  (** Here are the first iteratees
        peek - iteratee that look at the first character in the stream
        without consuming it
    *)

  val head : char option t
  (** head - iteratee that remove and return the first character in the stream *)

  val writer : (string -> unit IO.t) -> string -> unit t
  (** writer - a generic writer iteratee. Takes an argument of type
        'string -> unit IO.t' *)

  val break : (char -> bool) -> string t
  (** break - iteratee that stops consuming input when the supplied predicate is met,
        then returns the string so far *)

  val heads : string -> int t
  (** heads - iteratee that matches character for character the incoming stream and
        the string passed in, then returns the number of characters that
        matched *)

  val drop : int -> unit t
  (** drop - iteratee that consumes and ignores n characters of the stream *)

  val readn : int -> string t
  (** readn - iteratee that reads exactly n characters from the stream *)

  val read_int8 : int t
  (** read_int8 - reads an int8 from the stream *)

  val read_int16 : int t
  (** read_int16 - reads an int16 from the stream (bigendian byte order) *)

  val read_int32 : int32 t
  (** read_int32 - reads an int32 from the stream (bigendian byte order) *)

  val drop_while : (char -> bool) -> unit t
  (** drop_while - iteratee that drops characters from the stream while they
        satisfy the supplied predicate *)

  val accumulate : string t
  (** accumulate - Simply accumulate the stream until EOF *)

  val apply : (string -> unit) -> unit t
  (** apply - applies the chunks to the supplied function (for side effect) *)

  val liftI : 'a t IO.t -> 'a t
  (** liftI - turn an iteratee hiding inside the monad into an iteratee *)

  (** Enumerators *)

  (** An enumerator takes an 'a iteratee and gives you back an 'a iteratee *)
  type 'a enumerator = 'a t -> 'a t IO.t

  val enum_eof : 'a t -> 'a t IO.t
  (** enum_eof - Supply the EOF stream to the iteratee *)

  val enum_1chunk : string -> 'a t -> 'a t IO.t
  (** enum_1chunk - Give the supplied string to the iteratee in one chunk *)

  val enum_nchunk : string -> int -> 'a t -> 'a t IO.t
  (** enum_nchunk - Gives the supplied string to the iteratee in chunks of length n.
        Good for testing *)

  val extract_result_from_iteratee : 'a t -> 'a
  (** extract_result_from_iteratee - Given a 'done' iteratee, pull the result out *)

  (** Enumeratees *)

  (** An enumeratee is a function that takes an iteratee and returns a new iteratee.
        It acts as an iteratee to the outside world, but as an enumerator to the supplied
        iteratee *)
  type 'a enumeratee = 'a t -> 'a t t

  val take : int -> 'a t -> 'a t t
  (** take - takes exactly n characters from the input stream and applies them to
        the inner stream *)

  val stream_printer : string -> 'a t -> 'a t t
  (** stream_printer - given a name and an iteratee i, returns an iteratee that
        will print the chunks supplied before handing them off to the iteratee *)

  val modify : (string -> string) -> 'a t -> 'a t t
  (** modify - Modify the stream in some way before giving the result to the
        inner stream. For example, one could base64 encode things this way *)

  type 'a either = Left of 'a | Right of 'a

  val read_lines : string list either t
end
