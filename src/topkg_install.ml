(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type move_scheme =
  { field :
      [ `Test of bool * Topkg_fpath.t option * Topkg_cmd.t
      | Topkg_opam.Install.field ];
    auto_bin : bool;
    force : bool;
    built : bool;
    exts : Topkg_fexts.t;
    src : string;
    dst : string;
    debugger_support : bool; (* This a bit hacky, only used by
                                OCamlbuild higher-level installs *) }

type t = move_scheme list

let nothing = []

let flatten ls = (* We don't care about order *)
  let rec push acc = function v :: vs -> push (v :: acc) vs | [] -> acc in
  let rec loop acc = function
  | l :: ls -> loop (push acc l) ls
  | [] -> acc
  in
  loop [] ls

let split_ext s = match Topkg_string.cut ~rev:true s ~sep:'.' with
| None -> s, `Ext ""
| Some (name, ext) -> name, `Ext (Topkg_string.strf ".%s" ext)

let bin_drop_exts native = if native then [] else Topkg_fexts.ext ".native"
let lib_drop_exts native native_dynlink =
  if native
  then (if native_dynlink then [] else Topkg_fexts.ext ".cmxs")
  else Topkg_fexts.(c_library @ exts [".cmx"; ".cmxa"; ".cmxs"])

let to_build ?header c os i =
  let bdir = Topkg_conf.build_dir c in
  let debugger_support = Topkg_conf.debugger_support c in
  let build_tests = Topkg_conf.build_tests c in
  let ocaml_conf = Topkg_conf.OCaml.v c os in
  let native = Topkg_conf.OCaml.native ocaml_conf in
  let native_dylink = Topkg_conf.OCaml.native_dynlink ocaml_conf in
  let ext_to_string = Topkg_fexts.ext_to_string ocaml_conf in
  let file_to_str (n, ext) = Topkg_string.strf "%s%s" n (ext_to_string ext) in
  let maybe_build = [ ".cmti"; ".cmt" ] in
  let bin_drops = List.map ext_to_string (bin_drop_exts native) in
  let lib_drops = List.map ext_to_string (lib_drop_exts native native_dylink) in
  let add acc m =
    if m.debugger_support && not debugger_support then acc else
    let mv (targets, moves, tests as acc) src dst =
      let src = file_to_str src in
      let drop = not m.force && match m.field with
      | `Bin -> List.exists (Filename.check_suffix src) bin_drops
      | `Lib -> List.exists (Filename.check_suffix src) lib_drops
      | _ -> false
      in
      if drop then (targets, moves, tests) else
      let dst = file_to_str dst in
      let maybe = List.exists (Filename.check_suffix src) maybe_build in
      let targets = if m.built && not maybe then src :: targets else targets in
      let src = if m.built then Topkg_string.strf "%s/%s" bdir src else src in
      match m.field with
      | `Test (run, dir, args) ->
          if not build_tests then acc else
          let test = Topkg_test.v src ~args ~run ~dir in
          (targets, moves, test :: tests)
      | #Topkg_opam.Install.field as field ->
          let move = (field, Topkg_opam.Install.move ~maybe src ~dst) in
          (targets, move :: moves, tests)
    in
    let src, dst =
      if not m.auto_bin then m.src, m.dst else
      ((if native then m.src ^ ".native" else m.src ^ ".byte"),
       m.dst ^ (ext_to_string `Exe))
    in
    if m.exts = [] then mv acc (split_ext src) (split_ext dst) else
    let expand acc ext = mv acc (src, ext) (dst, ext) in
    List.fold_left expand acc m.exts
  in
  let targets, moves, tests = List.fold_left add ([], [], []) (flatten i) in
  let tests = if build_tests then Some tests else None in
  targets, ((`Header header), moves), tests

(* Install fields *)

type field =
  ?force:bool -> ?built:bool -> ?cond:bool -> ?exts:Topkg_fexts.t ->
  ?dst:string -> string -> t

let _field field
    ?(debugger_support = false)
    ?(auto = true) ?(force = false) ?(built = true) ?(cond = true) ?(exts = [])
    ?dst src =
  if not cond then [] else
  let dst = match dst with
  | None -> Topkg_fpath.basename src
  | Some dst ->
      if Topkg_fpath.is_file_path dst then dst else
      dst ^ (Topkg_fpath.basename src)
  in
  [{ field; auto_bin = auto; force; built; exts; src; dst;
     debugger_support; }]

let field field =
  _field ~debugger_support:false ~auto:false field

let field_exec field ?auto ?force ?built ?cond ?exts ?dst src =
  _field field ~debugger_support:false
    ?auto ?force ?built ?cond ?exts ?dst src

let bin = field_exec `Bin
let doc = field `Doc
let etc = field `Etc
let lib = field `Lib
let lib_root = field `Lib_root
let libexec = field_exec `Libexec
let libexec_root = field_exec `Libexec_root
let man = field `Man
let misc = field `Misc
let sbin = field_exec `Sbin
let share = field `Share
let share_root = field `Share_root
let stublibs = field `Stublibs
let toplevel = field `Toplevel
let unknown name = field (`Unknown name)
let test ?(run = true) ?dir ?(args = Topkg_cmd.empty) =
  field_exec (`Test (run, dir, args))

(* OCamlbuild higher-level installs *)

let parse_mllib contents = (* list of module name and path (for dir/Mod) *)
  let lines = Topkg_string.cuts ~sep:'\n' contents in
  let add_mod acc l =
    let path = String.trim @@ match Topkg_string.cut ~sep:'#' l with
    | None -> l
    | Some (p, _ (* comment *)) -> p
    in
    if path = "" then acc else
    let mod_name = Topkg_string.capitalize @@ Topkg_fpath.basename path in
    (mod_name, path) :: acc
  in
  List.fold_left add_mod [] lines

let field_of_field field = (* hack, recover a field from a field function... *)
  match (List.hd (field "")).field with
  | `Test (run, dir, args) -> assert false
  | #Topkg_opam.Install.field as field -> field

let mllib
    ?(field = lib) ?(cond = true) ?(cma = true) ?(cmxa = true) ?(cmxs = true)
    ?api ?dst_dir mllib
  =
  if not cond then [] else
  let debugger_support_field =
    _field ~debugger_support:true ~auto:false (field_of_field field)
  in
  let lib_dir = Topkg_fpath.dirname mllib in
  let lib_base = Topkg_fpath.rem_ext mllib in
  let dst f = match dst_dir with
  | None -> None
  | Some dir -> Some (Topkg_fpath.append dir (Topkg_fpath.basename f))
  in
  let api mllib_content =
    let mod_names = List.map fst mllib_content in
    match api with
    | None -> mod_names (* all the .mllib modules if unspecified *)
    | Some api ->
        let in_mllib i = List.mem (Topkg_string.capitalize i) mod_names in
        let api, orphans = List.partition in_mllib api in
        let warn o =
          Topkg_log.warn (fun m -> m "mllib %s: unknown interface %s" mllib o)
        in
        List.iter warn orphans;
        api
  in
  let library =
    let add_if cond v vs = if cond then v :: vs else vs in
    let exts =
      add_if cma (`Ext ".cma") @@ add_if cmxa (`Ext ".cmxa") @@
      add_if cmxs (`Ext ".cmxs") @@ add_if (cmxa || cmxs) `Lib []
    in
    field ?dst:(dst lib_base) ~exts lib_base
  in
  let add_mods acc mllib_content =
    let api = api mllib_content in
    let add_mod acc (m, path) =
      let fname = Topkg_string.uncapitalize (Topkg_fpath.basename path) in
      let fpath = match Topkg_fpath.dirname path with
      | "." -> Topkg_fpath.append lib_dir fname
      | parent -> Topkg_fpath.(append lib_dir (append parent fname))
      in
      let dst = dst fname in
      let exts, debugger_support_exts = match List.mem m api with
      | true -> Topkg_fexts.api, Topkg_fexts.exts [".ml"; ".cmt"]
      | false -> Topkg_fexts.cmx, Topkg_fexts.exts [".ml"; ".cmi"; ".cmt"; ]
      in
      field ?dst ~exts fpath ::
      debugger_support_field ?dst ~exts:debugger_support_exts fpath ::
      acc
    in
    List.fold_left add_mod acc mllib_content
  in
  begin
    Topkg_os.File.read mllib
    >>= fun contents -> Ok (parse_mllib contents)
    >>= fun mllib_content -> Ok (flatten @@ add_mods [library] mllib_content)
  end
  |> Topkg_log.on_error_msg ~use:(fun () -> [])

let parse_clib contents =
  let lines = Topkg_string.cuts ~sep:'\n' contents in
  let add_obj_path acc l =
    let path = String.trim @@ match Topkg_string.cut ~sep:'#' l with
    | None -> l
    | Some (p, _ (* comment *)) -> p
    in
    if path = "" then acc else path :: acc
  in
  List.fold_left add_obj_path [] lines

let clib
    ?(dllfield = stublibs) ?(libfield = lib) ?(cond = true) ?lib_dst_dir clib
  =
  if not cond then [] else
  let debugger_support_field =
    _field ~debugger_support:true ~auto:false (field_of_field lib)
  in
  let lib_dir = Topkg_fpath.dirname clib in
  let lib_base =
    let base = Topkg_fpath.(basename @@ rem_ext clib) in
    if Topkg_string.is_prefix ~affix:"lib" base
    then Ok (Topkg_string.with_index_range ~first:3 base)
    else R.error_msgf "%s: OCamlbuild .clib file must start with 'lib'" clib
  in
  let lib_dst f = match lib_dst_dir with
  | None -> None
  | Some dir -> Some (Topkg_fpath.append dir (Topkg_fpath.basename f))
  in
  let add_debugger_support cobjs =
    let add_cobj acc path =
      let fname = Topkg_fpath.(rem_ext @@ basename path) in
      let fpath = match Topkg_fpath.dirname path with
      | "." -> Topkg_fpath.append lib_dir fname
      | parent -> Topkg_fpath.(append lib_dir (append parent fname))
      in
      let dst = lib_dst fname in
      debugger_support_field ?dst ~exts:(Topkg_fexts.ext ".c") fpath :: acc
    in
    List.fold_left add_cobj [] cobjs
  in
  begin
    lib_base
    >>= fun lib_base -> Topkg_os.File.read clib
    >>= fun contents ->
    let cobjs = parse_clib contents in
    let lib = Topkg_fpath.append lib_dir ("lib" ^ lib_base) in
    let lib = libfield ~exts:Topkg_fexts.c_library lib ?dst:(lib_dst lib) in
    let dll = Topkg_fpath.append lib_dir ("dll" ^ lib_base) in
    let dll = dllfield ~exts:Topkg_fexts.c_dll_library dll in
    Ok (flatten @@ lib :: dll :: add_debugger_support cobjs)
  end
  |> Topkg_log.on_error_msg ~use:(fun () -> [])

(* Dummy codec *)

let codec : t Topkg_codec.t = (* we don't care *)
  let fields = (fun _ -> ()), (fun () -> []) in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"install" fields unit)

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
