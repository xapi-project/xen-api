(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Strings.

    See {!Topkg.String} for documentation. *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a

include module type of String

val head : string -> char option

val is_prefix : affix:string -> string -> bool
val is_suffix : affix:string -> string -> bool
val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool

val find_byte : ?start:int -> char -> string -> int option

val trim : string -> string
val cut : ?rev:bool -> sep:char -> string -> (string * string) option
val cuts : ?empty:bool -> sep:char -> string -> string list

val with_index_range : ?first:int -> ?last:int -> string -> string

val uppercase : string -> string

val parse_version : string -> (int * int * int * string option) option
val drop_initial_v : string -> string

val pp_text : Format.formatter -> string -> unit

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
