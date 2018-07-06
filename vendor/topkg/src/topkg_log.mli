(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Log

    Abridged [logs]. See {!Topkg.Log} for documentation. *)

(** {1 Log} *)

open Topkg_result

type level = App | Error | Warning | Info | Debug

val level : unit -> level option
val set_level : level option -> unit
val level_to_string : level option -> string
val level_of_string : string -> (level option, [`Msg of string]) Result.result

type 'a msgf =
  (?header:string -> ('a, Format.formatter, unit) Pervasives.format -> 'a) ->
  unit

val msg : level -> 'a msgf -> unit
val app : 'a msgf -> unit
val err : 'a msgf -> unit
val warn : 'a msgf -> unit
val info : 'a msgf -> unit
val debug : 'a msgf -> unit

val on_error_msg : ?level:level -> use:(unit -> 'a) -> 'a result -> 'a
val err_count : unit -> int
val warn_count : unit -> int

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
