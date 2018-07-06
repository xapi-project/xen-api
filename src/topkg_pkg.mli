(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package descriptions. *)

(** {1 Package} *)

open Topkg_result

type std_file
val std_file : ?install:bool -> Topkg_fpath.t -> std_file

type meta_file
val meta_file : ?lint:bool -> ?install:bool -> Topkg_fpath.t -> meta_file

type opam_file
val opam_file :
  ?lint:bool -> ?lint_deps_excluding:string list option -> ?install:bool ->
  Topkg_fpath.t -> opam_file

type t

val empty : t
val with_name_and_build_dir :
  ?name:string -> ?build_dir:Topkg_fpath.t -> t -> t

val v :
  ?delegate:Topkg_cmd.t ->
  ?readmes:std_file list ->
  ?licenses:std_file list ->
  ?change_logs:std_file list ->
  ?metas:meta_file list ->
  ?opams:opam_file list ->
  ?lint_files:Topkg_fpath.t list option ->
  ?lint_custom:(unit -> R.msg result list) ->
  ?distrib:Topkg_distrib.t ->
  ?publish:Topkg_publish.t ->
  ?build:Topkg_build.t -> string ->
  (Topkg_conf.t -> Topkg_install.t list result) -> t

val name : t -> string
val delegate : t -> Topkg_cmd.t option
val readmes : t -> Topkg_fpath.t list
val change_logs : t -> Topkg_fpath.t list
val licenses : t -> Topkg_fpath.t list
val distrib : t -> Topkg_distrib.t
val build : t -> Topkg_build.t
val install : t -> Topkg_conf.t -> Topkg_install.t list result
val codec : t Topkg_codec.t

(* Derived accessors *)

val build_dir : t -> Topkg_fpath.t
val opam : name:string -> t -> Topkg_fpath.t

(* Distrib *)

val distrib_uri : t -> string option
val distrib_prepare :
  t -> dist_build_dir:Topkg_fpath.t -> name:string -> version:string ->
  opam:Topkg_fpath.t -> Topkg_fpath.t list result

(* Publish *)

val publish_artefacts : t -> [ `Distrib | `Doc | `Alt of string ] list

(* Test *)

val test :
  t -> list:bool -> tests:string list -> args:Topkg_cmd.t option -> int result

(* Build *)

val build :
  t -> kind:[`Build | `Dry_run | `Raw of string list ] ->
  Topkg_conf.t -> Topkg_conf.os -> int result

(* Clean *)

val clean : t -> Topkg_conf.os -> int result

(* Lint *)

val lint_custom : t -> (unit -> R.msg result list) option
val lint_files : t -> Topkg_fpath.t list option
val lint_metas : t -> (Topkg_fpath.t * bool) list
val lint_opams : t -> (Topkg_fpath.t * bool * string list option) list

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
