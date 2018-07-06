(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Command *)

let cmd =
  Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Conf.tool "opam" `Host_os

(* Publish *)

let publish =
  let absent = Cmd.(v "opam-publish") in
  OS.Env.(value "TOPKG_OPAM_PUBLISH" cmd ~absent)

let ensure_publish () = OS.Cmd.must_exist publish >>| fun _ -> ()
let submit ?msg ~pkg_dir =
  let msg = match msg with
  | None -> Ok (Cmd.empty)
  | Some msg ->
      OS.File.tmp "topkg-opam-submit-msg-%s"
      >>= fun m -> OS.File.write m msg
      >>= fun () -> Ok Cmd.(v "--msg" % p m)
  in
  msg >>= fun msg -> OS.Cmd.run Cmd.(publish % "submit" %% msg % p pkg_dir)

(* Packages *)

let ocaml_base_packages = String.Set.of_list
    [ "base-bigarray"; "base-bytes"; "base-threads"; "base-unix"; ]

(* Files *)

module File = struct

  (* Try to compose with the OpamFile.OPAM API *)

  let id x = x
  let list f = fun v -> [f v]
  let field name field conv =
    name, fun acc o -> String.Map.add name (conv (field o)) acc

  let opt_field name field conv =
    name, fun acc o -> match field o with
    | None -> acc
    | Some v -> String.Map.add name (conv v) acc

  let deps_conv d =
    let add_pkg acc (n, _) = OpamPackage.Name.to_string n :: acc in
    OpamFormula.fold_left add_pkg [] d

  let fields = [
    opt_field "name" OpamFile.OPAM.name_opt (list OpamPackage.Name.to_string);
    opt_field "version" OpamFile.OPAM.version_opt
      (list OpamPackage.Version.to_string);
    field "opam-version" OpamFile.OPAM.opam_version
      (list OpamVersion.to_string);
    field "available" OpamFile.OPAM.available (list OpamFilter.to_string);
    field "maintainer" OpamFile.OPAM.maintainer id;
    field "homepage" OpamFile.OPAM.homepage id;
    field "authors" OpamFile.OPAM.author id;
    field "license" OpamFile.OPAM.license id;
    field "doc" OpamFile.OPAM.doc id;
    field "tags" OpamFile.OPAM.tags id;
    field "bug-reports" OpamFile.OPAM.bug_reports id;
    opt_field "dev-repo" OpamFile.OPAM.dev_repo (list OpamUrl.to_string);
    field "depends" OpamFile.OPAM.depends deps_conv;
    field "depopts" OpamFile.OPAM.depopts deps_conv;
  ]

  let field_names =
    let add acc (name, field) = String.Set.add name acc in
    List.fold_left add String.Set.empty fields

  let fields file =
    let parse file  =
      let file = OpamFilename.of_string (Fpath.to_string file) in
      let opam = OpamFile.OPAM.read (OpamFile.make file) in
      let known_fields =
        let add_field acc (_, field) = field acc opam in
        List.fold_left add_field String.Map.empty fields
      in
      (* FIXME add OpamFile.OPAM.extensions when supported *)
      known_fields
    in
    Logs.info (fun m -> m "Parsing opam file %a" Fpath.pp file);
    try Ok (parse file) with
    | exn ->
        (* Apparently in at least opam-lib 1.2.2, the error will be logged
             on stdout. *)
        R.error_msgf "%a: could not parse opam file" Fpath.pp file

  let deps ?(opts = true) fields =
    let deps = match String.Map.find "depends" fields with
    | None -> [] | Some deps -> deps
    in
    let dep_opts =
      if not opts then [] else
      match String.Map.find "depopts" fields with
      | None -> []  | Some deps -> deps
    in
    String.Set.of_list (List.rev_append dep_opts deps)
end

module Descr = struct
  type t = string * string

  let of_string s = match String.cuts "\n" s with
  | [] ->  R.error_msgf "Cannot extract opam descr."
  | synopsis :: descr -> Ok (synopsis, String.concat ~sep:"\n" descr)

  let to_string (synopsis, descr) = strf "%s\n%s" synopsis descr

  let of_readme ?flavour r =
    let parse_synopsis l =
      let error l = R.error_msgf "%S: can't extract opam synopsis" l in
      let ok s = Ok String.(Ascii.capitalize @@ String.Sub.to_string s) in
      let not_white c = not (Char.Ascii.is_white c) in
      let skip_non_white l = String.Sub.drop ~sat:not_white l in
      let skip_white l = String.Sub.drop ~sat:Char.Ascii.is_white l in
      let start =
        String.sub l |> skip_white |> skip_non_white |> skip_white
      in
      match String.Sub.head start with
      | None -> error l
      | Some c when Char.Ascii.is_letter c -> ok start
      | Some c -> (* Try to skip a separator. *)
          let start = start |> skip_non_white |> skip_white in
          match String.Sub.head start with
          | None -> error l
          | Some _ -> ok start
    in
    let drop_line l =
      String.is_prefix "Home page:" l ||
      String.is_prefix "Homepage:" l ||
      String.is_prefix "Contact:" l ||
      String.is_prefix "%%VERSION" l
    in
    let keep_line l = not (drop_line l) in
    match Topkg_care_text.head ?flavour r with
    | None -> R.error_msgf "Could not extract opam description."
    | Some (title, text) ->
        let sep = "\n" in
        let title = Topkg_care_text.header_title ?flavour title in
        parse_synopsis title
        >>= fun synopsis -> Ok (String.cuts ~sep text)
        >>= fun text -> Ok (List.filter keep_line text)
        >>= fun text -> Ok (synopsis, String.concat ~sep text)

  let of_readme_file file =
    let flavour = Topkg_care_text.flavour_of_fpath file in
    (OS.File.read file
     >>= fun text -> of_readme ?flavour text)
    |> R.reword_error_msg ~replace:true
      (fun m -> R.msgf "%a: %s" Fpath.pp file m)
end

module Url = struct
  let v ~uri ~checksum = strf "archive: \"%s\"\nchecksum: \"%s\"" uri checksum
  let with_distrib_file ~uri distrib_file =
    try
      let checksum = Digest.(to_hex @@ file (Fpath.to_string distrib_file)) in
      Ok (v ~uri ~checksum)
    with Failure msg | Sys_error msg -> R.error_msg msg
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
