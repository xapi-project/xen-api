(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {1 Distribution description}

    See {!section:Topkg.Pkg.distrib}. *)

(** {1 Distribution} *)

open Topkg_result

(* Watermarks *)

type watermark =
  string *
  [ `String of string | `Name | `Version | `Version_num | `Vcs of [ `Commit_id ]
  | `Opam of Topkg_fpath.t option * string * string ]

val define_watermarks :
  name:string -> version:string -> opam:Topkg_fpath.t ->
  watermark list -> (string * string) list

val watermark_file : (string * string) list -> Topkg_fpath.t -> unit result
val watermark_files :
  (string * string) list -> Topkg_fpath.t list -> unit result

(* Distribution *)

type t

val v :
  ?watermarks:watermark list ->
  ?files_to_watermark:(unit -> Topkg_fpath.t list result) ->
  ?massage:(unit -> unit result) ->
  ?exclude_paths:(unit -> Topkg_fpath.t list result) ->
  ?uri:string ->
  unit -> t

val watermarks : t -> watermark list
val files_to_watermark : t -> (unit -> Topkg_fpath.t list result)
val massage : t -> (unit -> unit result)
val exclude_paths : t -> (unit -> Topkg_fpath.t list result)
val uri : t -> string option
val codec : t Topkg_codec.t

(* Defaults *)

val default_watermarks : watermark list
val default_files_to_watermark : unit -> Topkg_fpath.t list result
val default_massage : unit -> unit result
val default_exclude_paths : unit -> Topkg_fpath.t list result

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
