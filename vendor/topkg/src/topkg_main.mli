(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Entry point for [pkg.ml] files. *)

(** {1 Main} *)

open Topkg_result

val describe :
  ?delegate:Topkg_cmd.t ->
  ?readmes:Topkg_pkg.std_file list ->
  ?licenses:Topkg_pkg.std_file list ->
  ?change_logs:Topkg_pkg.std_file list ->
  ?metas:Topkg_pkg.meta_file list ->
  ?opams:Topkg_pkg.opam_file list ->
  ?lint_files:Topkg_fpath.t list option ->
  ?lint_custom:(unit -> R.msg result list) ->
  ?distrib:Topkg_distrib.t ->
  ?publish:Topkg_publish.t ->
  ?build:Topkg_build.t ->
  string -> (Topkg_conf.t -> Topkg_install.t list result) -> unit

val disable : unit -> unit


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
