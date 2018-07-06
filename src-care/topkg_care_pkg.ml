(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Misc *)

let path_set_of_strings ps =
  try
    let add_excl acc p = match Fpath.of_string p with
    | Error (`Msg msg) -> failwith msg
    | Ok p -> Fpath.Set.add p acc
    in
    Ok (List.fold_left add_excl Fpath.Set.empty ps)
  with
  Failure msg -> R.error_msg msg

let rec path_list_of_strings ps =
  let rec loop acc = function
  | [] -> Ok (List.rev acc)
  | p :: ps ->
      match Fpath.of_string p with
      | Error _ as e -> e
      | Ok p -> loop (p :: acc) ps
  in
  loop [] ps

let uri_sld uri = match Topkg_care_text.split_uri uri with
| None -> None
| Some (_, host, _) ->
    match List.rev (String.cuts ~sep:"." host) with
    | fst :: snd :: _ -> Some snd
    | _ -> None

let uri_append u s = match String.head ~rev:true u with
| None -> s
| Some '/' -> strf "%s%s" u s
| Some _ -> strf "%s/%s" u s

let chop_ext u = match String.cut ~rev:true ~sep:"." u with
| None -> u
| Some (u, _) -> u

let chop_git_prefix u = match String.cut ~sep:"git+" u with
| Some ("", uri) -> uri
| _ -> u

(* Package *)

type t =
  { name : string option;
    version : string option;
    delegate : Cmd.t option;
    build_dir : Fpath.t option;
    opam : Fpath.t option;
    opam_descr : Fpath.t option;
    opam_fields : (string list String.map, R.msg) result Lazy.t;
    readmes : Fpath.t list option;
    change_logs : Fpath.t list option;
    licenses : Fpath.t list option;
    distrib_uri : string option;
    distrib_file : Fpath.t option;
    publish_msg : string option;
    publish_artefacts : [ `Distrib | `Doc | `Alt of string ] list option;
    pkg_file : Fpath.t;
    pkg : (Topkg.Private.Pkg.t, R.msg) result Lazy.t; }

let empty p = { p with pkg = lazy (Ok (Topkg.Private.Pkg.empty)) }
let pkg p = Lazy.force p.pkg

let opam_fields p = Lazy.force p.opam_fields
let opam_field p f = opam_fields p >>| fun fields -> String.Map.find f fields
let opam_field_hd p f = opam_field p f >>| function
| None | Some [] -> None
| Some (v :: _) -> Some v

let opam_homepage_sld p = opam_field_hd p "homepage" >>| function
| None -> None
| Some uri -> match uri_sld uri with None -> None | Some sld -> Some (uri, sld)

let pkg_file p = p.pkg_file

let name p = match p.name with
| Some n -> Ok n
| None -> pkg p >>| Topkg.Private.Pkg.name

let version p = match p.version with
| Some v -> Ok v
| None -> Topkg.Vcs.get () >>= fun r -> Topkg.Vcs.describe ~dirty:false r

let delegate p =
  let not_found () =
    R.error_msg "No package delegate found. \
                 Try `topkg help delegate` for more information."
  in
  match p.delegate with
  | Some cmd -> Ok cmd
  | None ->
      pkg p >>| Topkg.Private.Pkg.delegate >>= function
      | Some cmd -> Ok (Cmd.of_list @@ Topkg.Cmd.to_list cmd)
      | None ->
          match OS.Env.(value "TOPKG_DELEGATE" (some cmd) ~absent:None) with
          | Some cmd -> Ok cmd
          | None ->
              opam_homepage_sld p >>= function
              | None -> not_found ()
              | Some (_, sld) ->
                  let exec = strf "%s-topkg-delegate" sld in
                  let cmd = Cmd.v exec in
                  OS.Cmd.exists cmd >>= function
                  | true -> Ok cmd
                  | false ->
                      if exec <> "github-topkg-delegate"
                      then not_found ()
                      else Ok (Cmd.v "toy-github-topkg-delegate")

let build_dir p = match p.build_dir with
| Some b -> Ok b
| None -> pkg p >>| Topkg.Private.Pkg.build_dir >>= Fpath.of_string

let readmes p = match p.readmes with
| Some f -> Ok f
| None -> pkg p >>| Topkg.Private.Pkg.readmes >>= path_list_of_strings

let readme p = readmes p >>= function
| [] -> R.error_msgf "No readme file specified in the package description"
| r :: _ -> Ok r

let opam p = match p.opam with
| Some f -> Ok f
| None ->
    name p
    >>= fun name -> pkg p
    >>| Topkg.Private.Pkg.opam ~name >>= Fpath.of_string

let opam_descr p =
  let descr_file_for_opam opam =
    if Fpath.has_ext ".opam" opam then Fpath.(rem_ext opam + ".descr") else
    Fpath.(parent opam / "descr")
  in
  let read f = OS.File.read f >>= fun c -> Topkg_care_opam.Descr.of_string c in
  match p.opam_descr with
  | Some f -> read f
  | None ->
      opam p
      >>= fun opam -> Ok (descr_file_for_opam opam)
      >>= fun descr_file -> OS.File.exists descr_file
      >>= function
      | true ->
          Logs.info (fun m -> m "Found opam descr file %a" Fpath.pp descr_file);
          read descr_file
      | false ->
          readme p
          >>= fun readme ->
          Logs.info
            (fun m -> m "Extracting opam descr from %a" Fpath.pp readme);
          Topkg_care_opam.Descr.of_readme_file readme

let change_logs p = match p.change_logs with
| Some f -> Ok f
| None -> pkg p >>| Topkg.Private.Pkg.change_logs >>= path_list_of_strings

let change_log p = change_logs p >>= function
| [] -> R.error_msgf "No change log specified in the package description."
| l :: _ -> Ok l

let licenses p = match p.licenses with
| Some f -> Ok f
| None -> pkg p >>| Topkg.Private.Pkg.licenses >>= path_list_of_strings

let distrib_uri ?(raw = false) p =
  let subst_uri p uri =
    uri
    >>= fun uri -> name p
    >>= fun name -> version p
    >>= fun version -> Ok (Topkg.String.drop_initial_v version)
    >>= fun version_num ->
    let defs = String.Map.(empty
                          |> add "NAME" name |> add "VERSION" version
                          |> add "VERSION_NUM" version_num)
    in
    Pat.of_string uri >>| fun pat -> Pat.format defs pat
  in
  let not_found () =
    R.error_msg "no distribution URI found, see topkg's API documentation."
  in
  let uri = match p.distrib_uri with
  | Some u -> Ok u
  | None ->
      pkg p
      >>= fun pkg -> match Topkg.Private.Pkg.distrib_uri pkg with
      | Some u -> Ok u
      | None ->
          opam_homepage_sld p >>= function
          | None -> not_found ()
          | Some (uri, sld) ->
              if sld <> "github"
              then (Ok (uri_append uri "releases/$(NAME)-$(VERSION_NUM).tbz"))
              else
              opam_field_hd p "dev-repo">>= function
              | None -> not_found ()
              | Some dev_repo ->
                  Ok (uri_append (chop_git_prefix (chop_ext dev_repo))
                      "releases/download/$(VERSION)/$(NAME)-$(VERSION_NUM).tbz")
  in
  if raw then uri else subst_uri p uri

let distrib_filename ?(opam = false) p =
  let sep = if opam then '.' else '-' in
  name p
  >>= fun name -> version p
  >>= fun version -> Ok (Topkg.String.drop_initial_v version)
  >>= fun version_num -> Fpath.of_string (strf "%s%c%s" name sep version_num)

let distrib_archive_path p =
  build_dir p
  >>= fun build_dir -> distrib_filename ~opam:false p
  >>| fun b -> Fpath.(build_dir // b + ".tbz")

let distrib_file p = match p.distrib_file with
| Some f -> Ok f
| None ->
    (distrib_archive_path p
     >>= fun f -> OS.File.must_exist f)
    |> R.reword_error_msg
      (fun _ -> R.msgf "Did you forget to call 'topkg distrib' ?")

let publish_msg p = match p.publish_msg with
| Some msg -> Ok msg
| None ->
    change_log p
    >>= fun change_log -> Topkg_care_text.change_log_file_last_entry change_log
    >>= fun (_, (h, txt)) -> Ok (strf "%s\n%s" h txt)

let publish_artefacts p = match p.publish_artefacts with
| Some arts -> Ok arts
| None -> pkg p >>| Topkg.Private.Pkg.publish_artefacts

let v
    ?name ?version ?delegate ?build_dir ?opam:opam_file ?opam_descr
    ?readme ?change_log ?license ?distrib_uri ?distrib_file ?publish_msg
    ?publish_artefacts
    pkg_file
  =
  let readmes = match readme with Some r -> Some [r] | None -> None in
  let change_logs = match change_log with Some c -> Some [c] | None -> None in
  let licenses = match license with Some l -> Some [l] | None -> None in
  let pkg = lazy (Topkg_care_ipc.ask ~pkg_file (Topkg.Private.Ipc.pkg ())) in
  let rec opam_fields = lazy (opam p >>= fun o -> Topkg_care_opam.File.fields o)
  and p =
    { name; version; delegate; build_dir; opam = opam_file; opam_descr;
      opam_fields; readmes; change_logs; licenses; distrib_uri; distrib_file;
      publish_msg; publish_artefacts; pkg_file; pkg }
  in
  p

(* Distrib *)

let distrib_prepare_ipc p ~dist_build_dir ~name ~version ~opam =
  let dist_build_dir = Fpath.to_string dist_build_dir in
  let opam = Fpath.to_string opam in
  R.join @@ Topkg_care_ipc.ask ~pkg_file:p.pkg_file
    (Topkg.Private.Ipc.distrib_prepare ~dist_build_dir ~name ~version ~opam)

let distrib_archive p ~keep_dir =
  Topkg_care_archive.ensure_bzip2 ()
  >>= fun () -> name p
  >>= fun name -> build_dir p
  >>= fun build_dir -> version p
  >>= fun version -> opam p
  >>= fun opam -> distrib_filename p
  >>= fun root -> Ok Fpath.(build_dir // root + ".build")
  >>= fun dist_build_dir -> OS.Dir.delete ~recurse:true dist_build_dir
  >>= fun () -> Topkg_vcs.get ()
  >>= fun repo -> Topkg_vcs.commit_id repo ~dirty:false
  >>= fun head -> Topkg_vcs.commit_ptime_s repo ~commit_ish:head
  >>= fun mtime -> Topkg_vcs.clone repo ~dir:(Fpath.to_string dist_build_dir)
  >>= fun () -> Topkg_vcs.get ~dir:(Fpath.to_string dist_build_dir) ()
  >>= fun clone -> Ok (Topkg_string.strf "topkg-dist-%s" head)
  >>= fun branch -> Topkg_vcs.checkout clone ~branch ~commit_ish:head
  >>= fun () -> distrib_prepare_ipc p ~dist_build_dir ~name ~version ~opam
  >>= fun exclude_paths -> path_set_of_strings exclude_paths
  >>= fun exclude_paths ->
  Topkg_care_archive.tar dist_build_dir ~exclude_paths ~root ~mtime
  >>= fun tar -> distrib_archive_path p
  >>= fun archive -> Topkg_care_archive.bzip2 tar ~dst:archive
  >>= fun () ->
  (if keep_dir then Ok () else OS.Dir.delete ~recurse:true dist_build_dir)
  >>= fun () -> Ok archive

(* Test & build *)

let run p ~dir cmd ~args ~out =
  let pkg_file = p.pkg_file in
  let cmd = Cmd.(v "ocaml" % p pkg_file % cmd %% args) in
  let run () = OS.Cmd.run_out cmd |> out in
  R.join @@ OS.Dir.with_current dir run ()

let test p ~dir ~args ~out = run p ~dir "test" ~args ~out
let build p ~dir ~args ~out = run p ~dir "build" ~args ~out
let clean p ~dir ~args ~out = run p ~dir "clean" ~args ~out

(* Lint *)

let pp_path = Topkg_care_text.Pp.path
let pp_status = Topkg_care_text.Pp.status

let lint_log msg = Logs.info (fun m -> m ~header:"LINT" "%a" Fmt.text msg); 0
let lint_disabled test =
  Logs.info (fun m -> m ~header:"LINT" "Package@ disabled@ %a." Fmt.text test);
  0

let lint_custom p =
  let report errs res =
    let status, msg, errs = match res with
    | Ok (`Msg msg) -> `Ok, msg, errs
    | Error (`Msg msg) -> `Fail, msg, errs + 1
    in
    Logs.app (fun m -> m "%a @[%a@]" pp_status status Fmt.text msg);
    errs
  in
  begin
    let pkg_file = p.pkg_file in
    pkg p >>= fun p -> match Topkg.Private.Pkg.lint_custom p with
    | None -> Ok (lint_disabled "custom linting")
    | _ ->
        Topkg_care_ipc.ask ~pkg_file (Topkg.Private.Ipc.lint_custom ())
        >>= function
        | None -> assert false
        | Some results -> Ok (List.fold_left report 0 results)
  end
  |> Logs.on_error_msg ~use:(fun () -> 1)

let lint_std_files p =
  let lint_exists file errs =
    let report exists =
      let status, errs = if exists then `Ok, errs else `Fail, errs + 1 in
      Logs.app (fun m ->
           m "%a @[File %a@ is@ present.@]" pp_status status pp_path file);
      errs
    in
    (OS.File.exists file >>= fun exists -> Ok (report exists))
    |> Logs.on_error_msg ~use:(fun () -> errs + 1)
  in
  begin
    pkg p >>= fun p -> match Topkg.Private.Pkg.lint_files p with
    | None -> Ok (lint_disabled "standard files linting")
    | Some files ->
        path_set_of_strings files
        >>= fun files -> Ok (Fpath.Set.fold lint_exists files 0)
  end
  |> Logs.on_error_msg ~use:(fun () -> 1)

let lint_file_with_cmd file_kind ~cmd file errs handle_exit =
  let run_linter cmd file ~exists =
    if not exists then Ok (`Fail (strf "%a: No such file" Fpath.pp file)) else
    OS.Cmd.(run_out ~err:err_run_out Cmd.(cmd % p file) |> out_string)
    >>| fun (out, status) -> handle_exit (snd status) out
  in
  begin
    OS.File.exists file
    >>= fun exists -> run_linter cmd file ~exists
    >>| function
    | `Ok ->
        Logs.app
          (fun m -> m "%a @[lint@ %s %a.@]"
              pp_status `Ok file_kind pp_path file);
        errs
    | `Fail msgs ->
        Logs.app
          (fun m -> m "%a @[<v>@[lint@ %s %a:@]@,@[%a messages:@]@,%a@]"
              pp_status `Fail file_kind pp_path file Cmd.pp cmd Fmt.lines msgs);
        errs + 1
  end
  |> Logs.on_error_msg ~use:(fun () -> errs + 1)

let lint_metas p =
  begin
    pkg p
    >>= fun p -> match Topkg.Private.Pkg.lint_metas p with
    | [] -> Ok (lint_log "No ocamlfind META file to lint")
    | metas ->
      let cmd = Cmd.(Topkg_care_ocamlfind.cmd % "lint") in
      let handle_exit status out = match status with
      | `Exited 0 -> `Ok
      | _ -> `Fail out
      in
      let lint errs (f, lint) =
        begin
          Fpath.of_string f >>| fun f ->
          if not lint
          then lint_log (strf "ocamlfind META lint disabled for %a" Fpath.pp f)
          else lint_file_with_cmd "META file" ~cmd f errs handle_exit
        end
        |> Logs.on_error_msg ~use:(fun () -> errs + 1)
      in
      Ok (List.fold_left lint 0 metas)
  end
  |> Logs.on_error_msg ~use:(fun () -> 1)

let lint_opams p =
  begin
    pkg p >>= fun p -> match Topkg.Private.Pkg.lint_opams p with
    | [] -> Ok (lint_log "No opam file to lint")
    | opams ->
        (* We first run opam lint with -s and if there's something beyond
           5 we rerun it without it for the error messages. It's ugly
           since 5 will still but opam lint's cli is broken. *)
        let cmd = Cmd.(Topkg_care_opam.cmd % "lint") in
        let handle_exit file status out = match status, out with
        | `Exited 0,
          ("" | "5" (* dirname version vs opam file version *)) -> `Ok
        | _ ->
            let err = OS.Cmd.err_run_out in
            match OS.Cmd.(run_out ~err Cmd.(cmd % p file) |> out_string) with
            | Ok (out, _) -> `Fail out
            | Error (`Msg out) -> `Fail out
        in
        let cmd = Cmd.(cmd % "-s") in
        let lint errs (f, lint, _) =
          begin
            Fpath.of_string f >>| fun f ->
            if not lint
            then lint_log (strf "opam file lint disabled for %a" Fpath.pp f)
            else lint_file_with_cmd "opam file" ~cmd f errs (handle_exit f)
          end
          |> Logs.on_error_msg ~use:(fun () -> errs + 1)
        in
        Ok (List.fold_left lint 0 opams)
  end
  |> Logs.on_error_msg ~use:(fun () -> 1)

let lint_deps_default_excludes =
  let exclude = ["ocamlfind"; "ocamlbuild"; "topkg"] in
  Topkg_care_ocamlfind.base_packages
  |> String.Set.union Topkg_care_opam.ocaml_base_packages
  |> String.Set.union (String.Set.of_list exclude)

let lint_opam_deps errs (opam, _, exclude) =
  let pp_deps_mismatches ppf (opam_only, tags_only) =
    let pp_miss present absent ppf id =
      Fmt.pf ppf "@[%a: %a present but %a absent@]"
        Fmt.(styled `Underline string) id
        Fmt.(styled `Green string) present
        Fmt.(styled `Red string) absent
    in
    let sep =
      if String.Set.(is_empty opam_only || is_empty tags_only)
      then Fmt.nop else Fmt.cut
    in
    Fmt.pf ppf "@[<v>%a%a%a@]"
      (String.Set.pp (pp_miss "opam" "_tags")) opam_only sep ()
      (String.Set.pp (pp_miss "_tags" "opam")) tags_only
  in
  let lint_deps ~exclude ~opam ~tags =
    begin
      Topkg_care_ocamlbuild.package_tags ~roots:true tags
      >>= fun tags -> Topkg_care_opam.File.fields opam
      >>| fun fields ->
      let exclude = String.Set.union exclude lint_deps_default_excludes in
      let keep id =
        not (String.Set.mem id exclude) &&
        not (String.is_prefix "conf-" id)
      in
      let opam_deps = Topkg_care_opam.File.deps ~opts:true fields in
      let opam = String.Set.filter keep opam_deps in
      let tags = String.Set.filter keep tags in
      let opam_only = String.Set.diff opam tags in
      let tags_only = String.Set.diff tags opam in
      if String.Set.is_empty opam_only && String.Set.is_empty tags_only
      then None
      else Some (opam_only, tags_only)
    end
    |> R.reword_error_msg ~replace:true
      (fun msg -> R.msgf "could not lint dependencies: %s" msg)
  in
  begin
    Fpath.of_string opam
    >>= fun opam -> match exclude with
    | None ->
        Ok (lint_disabled @@
            Fmt.(strf_like
                   stdout "opam dependency linting for %a" pp_path opam))
    | Some exclude ->
        let tags = Fpath.v "_tags" in
        let exclude = String.Set.of_list exclude in
        lint_deps ~exclude ~opam ~tags
        >>| function
        | None ->
            Logs.app (fun m -> m "%a @[opam file %a@ and %a dependency check."
                         pp_status `Ok pp_path opam pp_path tags);
            errs
        | Some miss ->
            Logs.app
              (fun m -> m "%a @[<v>@[File %a and %a dependency check:@]@,%a@]"
                  pp_status `Fail pp_path opam pp_path tags
                  pp_deps_mismatches miss);
            errs + 1
  end
  |> Logs.on_error_msg ~use:(fun () -> errs + 1)

let lint_deps p =
  begin
    pkg p >>= fun p -> match Topkg.Private.Pkg.lint_opams p with
    | [] -> Ok (lint_log "No opam file to dependency lint")
    | opams -> Ok (List.fold_left lint_opam_deps 0 opams)
  end
  |> Logs.on_error_msg ~use:(fun () -> 1)

type lint = [ `Custom | `Std_files | `Meta | `Opam | `Deps ]

let lints =
  [`Custom, lint_custom;
   `Std_files, lint_std_files;
   `Meta, lint_metas;
   `Opam, lint_opams;
   `Deps, lint_deps ]

let lint_all = List.map fst lints

let lint ?(ignore_pkg = false) p ~dir todo =
  let lint pkg =
    let do_lint acc (l, f) = acc + if List.mem l todo then f pkg else 0 in
    match List.fold_left do_lint 0 lints with
    | 0 ->
        Logs.app (fun m -> m "%a lint@ %a %a"
                     pp_status `Ok pp_path dir
                     (Fmt.styled_unit `Green "success") ()); 0
    | n ->
        Logs.app (fun m -> m "%a lint@ %a@ %a:@ %d@ errors."
                     pp_status `Fail pp_path dir
                     (Fmt.styled_unit `Red "failure") () n); 1
  in
  let p =
    if ignore_pkg then Ok (empty p) else
    pkg p >>| fun _ -> p (* verify the pkg_file exists *)
  in
  p >>= fun p -> OS.Dir.with_current dir lint p

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
