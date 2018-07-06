(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

let ocamlbuild_flags =
  Topkg_cmd.(empty % "-use-ocamlfind" % "-classic-display")

let build_cmd c os =
  let ocamlbuild = Topkg_conf.tool "ocamlbuild" os in
  let build_dir = Topkg_conf.build_dir c in
  let toolchain =
    match Topkg_conf.toolchain c with
    | Some toolchain -> Topkg_cmd.(v "-toolchain" % toolchain)
    | _ -> Topkg_cmd.empty
  in
  let debug = Topkg_cmd.(on (Topkg_conf.debug c) (v "-tag" % "debug")) in
  let profile = Topkg_cmd.(on (Topkg_conf.profile c) (v "-tag" % "profile")) in
  let jobs =
    let n = Topkg_conf.jobs c in
    Topkg_log.info (fun m -> m "using %d jobs" n);
    Topkg_cmd.(on (n != 1) (v "-j" % string_of_int n)) in
  Topkg_cmd.(ocamlbuild %% ocamlbuild_flags %% toolchain %% jobs %%
                          debug %% profile % "-build-dir" % build_dir)

let clean_cmd os ~build_dir =
  let ocamlbuild = Topkg_conf.tool "ocamlbuild" os in
  Topkg_cmd.(ocamlbuild %% ocamlbuild_flags %
             "-build-dir" % build_dir % "-clean")

type t =
  { prepare_on_pin : bool;
    dir : Topkg_fpath.t;
    pre : Topkg_conf.t -> unit result;
    cmd : Topkg_conf.t -> Topkg_conf.os -> Topkg_fpath.t list -> unit result;
    post : Topkg_conf.t -> unit result;
    clean : Topkg_conf.os -> build_dir:Topkg_fpath.t -> unit result; }

let with_dir b dir = { b with dir }

let nop = fun _ -> Ok ()

let cmd c os files =
  Topkg_os.Cmd.run @@ Topkg_cmd.(build_cmd c os %% of_list files)

let clean os ~build_dir =
  Topkg_os.Cmd.run @@ clean_cmd os ~build_dir

let v
    ?(prepare_on_pin = true) ?(dir = "_build") ?(pre = nop) ?(cmd = cmd)
    ?(post = nop) ?(clean = clean) () =
  { prepare_on_pin; dir; pre; cmd; post; clean; }

let prepare_on_pin b = b.prepare_on_pin
let dir b = b.dir
let pre b = b.pre
let cmd b = b.cmd
let post b = b.post
let clean b = b.clean
let codec =
  let prepare_on_pin = Topkg_codec.(with_kind "prepare_on_pin" @@ bool) in
  let dir = Topkg_codec.(with_kind "dir" @@ string) in
  let fields =
    let stub _ = invalid_arg "not executable outside package definition" in
    (fun b -> b.prepare_on_pin, b.dir),
    (fun (prepare_on_pin, dir)  ->
       { prepare_on_pin; dir; pre = stub; cmd = stub; post = stub;
         clean = stub })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"build" fields (pair prepare_on_pin dir))

let ocb_tag c key tag =
  let tag = Topkg_string.strf "%s(%a)" tag (Topkg_conf.pp_value c) key in
  Topkg_cmd.(v "-tag" % tag)

let ocb_bool_tag c key tag =
  Topkg_cmd.(on (Topkg_conf.value c key) @@ v "-tag" % tag)

let ocb_bool_tags c tags =
  let f (key, tag) = Topkg_cmd.(%%) (ocb_bool_tag c key tag) in
  List.fold_right f tags Topkg_cmd.empty

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
