(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Text processing helpers.

    See {!Topkg_care.Text}. *)

(** {1 Text} *)

open Bos_setup

type flavour = [ `Markdown | `Asciidoc ]

val flavour_of_fpath : Fpath.t -> flavour option
val head : ?flavour:flavour -> string -> (string * string) option
val header_title : ?flavour:flavour -> string -> string

val change_log_last_entry :
  ?flavour:flavour -> string -> (string * (string * string)) option

val change_log_file_last_entry :
  Fpath.t -> ((string * (string * string)), R.msg) result

val split_uri : ?rel:bool -> string -> (string * string * string) option

val find_pager : don't:bool -> (Cmd.t option, R.msg) result
val edit_file : Fpath.t -> (int, R.msg) result

module Pp : sig
  val name : string Fmt.t
  val version : string Fmt.t
  val commit : string Fmt.t
  val dirty : unit Fmt.t
  val path : Fpath.t Fmt.t
  val status : [`Ok | `Fail] Fmt.t
end

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
