(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Preliminaries *)

include Topkg_result

let strf = Topkg_string.strf
module String = Topkg_string

type fpath = string
module Fpath = Topkg_fpath

module Cmd = Topkg_cmd
module Log = Topkg_log
module OS = Topkg_os
module Vcs = Topkg_vcs

(* Package description *)

module Conf = Topkg_conf
module Exts = Topkg_fexts
module Pkg = struct

  (* Install *)

  type install = Topkg_install.t
  type field = Topkg_install.field
  type exec_field = ?auto:bool -> field

  let nothing = Topkg_install.nothing
  let flatten = Topkg_install.flatten
  let bin = Topkg_install.bin
  let doc = Topkg_install.doc
  let etc = Topkg_install.etc
  let lib = Topkg_install.lib
  let lib_root = Topkg_install.lib_root
  let libexec = Topkg_install.libexec
  let libexec_root = Topkg_install.libexec_root
  let man = Topkg_install.man
  let misc = Topkg_install.misc
  let sbin = Topkg_install.sbin
  let share = Topkg_install.share
  let share_root = Topkg_install.share_root
  let stublibs = Topkg_install.stublibs
  let toplevel = Topkg_install.toplevel
  let unknown = Topkg_install.unknown
  let test = Topkg_install.test

  let mllib = Topkg_install.mllib
  let clib = Topkg_install.clib

  (* Distrib *)

  type watermark = Topkg_distrib.watermark
  type distrib = Topkg_distrib.t

  let distrib = Topkg_distrib.v
  let watermarks = Topkg_distrib.default_watermarks
  let files_to_watermark = Topkg_distrib.default_files_to_watermark
  let massage = Topkg_distrib.default_massage
  let exclude_paths = Topkg_distrib.default_exclude_paths

  (* Publish *)

  type publish = Topkg_publish.t
  let publish = Topkg_publish.v

  (* Build *)

  type build = Topkg_build.t
  let build = Topkg_build.v
  let build_cmd = Topkg_build.build_cmd
  let clean_cmd = Topkg_build.clean_cmd
  let ocb_tag = Topkg_build.ocb_tag
  let ocb_bool_tag = Topkg_build.ocb_bool_tag
  let ocb_bool_tags = Topkg_build.ocb_bool_tags

  (* Package *)

  type std_file = Topkg_pkg.std_file
  let std_file = Topkg_pkg.std_file

  type meta_file = Topkg_pkg.meta_file
  let meta_file = Topkg_pkg.meta_file

  type opam_file = Topkg_pkg.opam_file
  let opam_file = Topkg_pkg.opam_file

  (* Describe *)

  let describe = Topkg_main.describe
end

module Private = struct
  let disable_main = Topkg_main.disable
  module Codec = Topkg_codec
  module Pkg = Topkg_pkg
  module Ipc = Topkg_ipc
  module Opam = Topkg_opam
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
