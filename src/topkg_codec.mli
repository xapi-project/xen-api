(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg interprocess communication codec.

    See {!Topkg.Private.Codec} for documentation. *)

(** {1 Codec} *)

open Topkg_result

type error = Corrupted of (string * string) | Version of int * int
val pp_error : Format.formatter -> error -> unit
exception Error of error
val err : kind:string -> string -> 'a

type 'a t

val v : kind:string -> enc:('a -> string) -> dec:(string -> 'a) -> 'a t
val kind : 'a t -> string
val enc : 'a t -> 'a -> string
val dec : 'a t -> string -> 'a
val dec_result : 'a t -> string -> 'a result
val with_kind : string -> 'a t -> 'a t
val write : Topkg_fpath.t -> 'a t -> 'a -> unit result
val read : Topkg_fpath.t -> 'a t -> 'a result

val unit : unit t
val const : 'a -> 'a t
val bool : bool t
val int : int t
val string : string t
val option : 'a t -> 'a option t
val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
val list : 'a t -> 'a list t
val pair : 'a t -> 'b t -> ('a * 'b) t
val t2 : 'a t -> 'b t -> ('a * 'b) t
val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val alt : kind:string -> ('a -> int) -> 'a t array -> 'a t
val version : int -> 'a t -> 'a t
val view : ?kind:string -> ('a -> 'b) * ('b -> 'a) -> 'b t -> 'a t

val msg : [`Msg of string ] t
val result_error_msg : 'a t -> 'a result t

val fpath : Topkg_fpath.t t
val cmd : Topkg_cmd.t t

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
