(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** opam interaction.

    See {!Topkg_care.Opam}. *)

(** {1 opam} *)

open Bos_setup

val cmd : Cmd.t
val ensure_publish : unit -> (unit, R.msg) result
val submit : ?msg:string -> pkg_dir:Fpath.t -> (unit, R.msg) result
val ocaml_base_packages : String.set

module File : sig
  val field_names : String.set
  val fields : Fpath.t -> ((string list) String.map , R.msg) result
  val deps : ?opts:bool -> (string list) String.map -> String.set
end

module Descr : sig
  type t = string * string
  val of_string : string -> (t, R.msg) result
  val to_string : t -> string
  val of_readme :
    ?flavour:Topkg_care_text.flavour -> string -> (t, R.msg) result
  val of_readme_file : Fpath.t -> (t, R.msg) result
end

module Url : sig
  val v : uri:string -> checksum:string -> string
  val with_distrib_file : uri:string -> Fpath.t -> (string, R.msg) result
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
