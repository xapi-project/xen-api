#!/usr/bin/env ocaml
#use "topfind"

(* Bootstrap from source, note #mod_use is 4.01 *)
#require "bytes"
#require "result"
#directory "src"
#mod_use "topkg_result.ml"
#mod_use "topkg_string.ml"
#mod_use "topkg_log.ml"
#mod_use "topkg_fpath.ml"
#mod_use "topkg_cmd.ml"
#mod_use "topkg_os.ml"
#mod_use "topkg_vcs.ml"
#mod_use "topkg_codec.ml"
#mod_use "topkg_conf.ml"
#mod_use "topkg_fexts.ml"
#mod_use "topkg_opam.ml"
#mod_use "topkg_test.ml"
#mod_use "topkg_install.ml"
#mod_use "topkg_build.ml"
#mod_use "topkg_distrib.ml"
#mod_use "topkg_publish.ml"
#mod_use "topkg_pkg.ml"
#mod_use "topkg_ipc.ml"
#mod_use "topkg_main.ml"
#mod_use "topkg.ml"

open Topkg

let () =
  let metas = [ Pkg.meta_file ~install:false "pkg/META" ] in
  let opams =
    let install = false in
    let not_topkg_deps =
      Some ["fmt"; "logs"; "bos"; "cmdliner"; "webbrowser"; "opam-format"]
    in
    [ Pkg.opam_file ~install "topkg.opam" ~lint_deps_excluding:not_topkg_deps;
      Pkg.opam_file ~install "topkg-care.opam" ]
  in
  Pkg.describe ~metas ~opams "topkg" @@ fun c ->
  match (* bootstrap, Conf doesn't work, eqs *) Topkg_conf.pkg_name c with
  | "topkg" ->
      Ok [ Pkg.lib "pkg/META";
           Pkg.lib "topkg.opam" ~dst:"opam";
           Pkg.mllib ~api:["Topkg"] "src/topkg.mllib";
           Pkg.test "test/test"; ]
  | "topkg-care" ->
      Ok [ Pkg.lib "topkg-care.opam" ~dst:"opam";
           Pkg.mllib ~api:["Topkg_care"] "src-care/topkg_care.mllib";
           Pkg.bin "src-bin/topkg_bin" ~dst:"topkg";
           Pkg.bin "src-bin/toy_github_delegate"
             ~dst:"toy-github-topkg-delegate";
           Pkg.doc "test/unsupportive-delegate";
           Pkg.doc "test/echo-delegate" ]
  | other ->
      R.error_msgf "unknown package name: %s" other
