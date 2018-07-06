(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Configuration value converters *)

type 'a conv =
  { parse : (string -> 'a result);
    print : (Format.formatter -> 'a -> unit);
    docv : string; }

let conv ?(docv = "VALUE") parse print = { parse; print; docv }
let conv_parser conv = conv.parse
let conv_printer conv = conv.print
let conv_docv conv = conv.docv
let conv_with_docv conv ~docv = { conv with docv }

let bool =
  let parse s = try Ok (bool_of_string s) with
  | Invalid_argument _ -> R.error_msgf "%S: Can't parse boolean value" s
  in
  conv ~docv:"BOOL" parse Format.pp_print_bool

let int =
  let parse s = try Ok (int_of_string s) with
  | Failure _ -> R.error_msgf "%S: Can't parse integer value" s
  in
  conv ~docv:"INT" parse Format.pp_print_int

let string = conv ~docv:"STRING" (fun s -> Ok s) Format.pp_print_string
let fpath = conv_with_docv string "PATH"

let some ?(none = "") conv =
  let parse s = match conv.parse s with
  | Ok v -> Ok (Some v)
  | Error _ as e -> e
  in
  let print ppf = function
  | None -> Format.pp_print_string ppf none
  | Some v -> conv.print ppf v
  in
  { conv with parse; print }

(* Universal type, see http://mlton.org/UniversalType *)

type univ = exn
let univ (type s) () =
  let module M = struct exception E of s option end in
  (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

(* Configuration keys *)

let key_id =
  let count = ref (-1) in
  fun () -> incr count; !count

type 'a absent = Value of 'a | Discover of (unit -> 'a result)
type 'a key =
  { id : int;                                      (* unique id for the key. *)
    name : string;                                              (* key name. *)
    conv : 'a conv;                                  (* key value converter. *)
    absent : 'a absent;                             (* value if unspecified. *)
    env : string option;       (* environment variable to override [absent]. *)
    to_univ : 'a -> univ;                     (* convert to universal value. *)
    of_univ : univ -> 'a option;            (* convert from universal value. *)
    doc : string; }                            (* documentation for the key. *)

module Key = struct
  type t = V : 'a key -> t
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.id k1.id
end

module Kset = Set.Make (Key)
module Kmap = Map.Make (Key)

let key_index = ref Kset.empty

let cli_opts_of_key_index () =
  let add_key (Key.V k as key) acc = ("--" ^ k.name, key) :: acc in
  Kset.fold add_key !key_index []

let _key ?docv ?(doc = "Undocumented") ?env name conv absent =
  let id = key_id () in
  let to_univ, of_univ = univ () in
  let conv = match docv with
  | None -> conv
  | Some docv -> conv_with_docv conv docv
  in
  let key = { id; name; conv; absent; env; to_univ; of_univ; doc } in
  key_index := Kset.add (Key.V key) !key_index;
  key

let key ?docv ?doc ?env name conv ~absent =
  _key ?docv ?doc ?env name conv (Value absent)

let discovered_key ?docv ?doc ?env name conv ~absent =
  _key ?docv ?doc ?env name conv (Discover absent)

let key_absent_value k = (* WARNING raises Failure *)
  let absent_field k = match k.absent with
  | Value v -> v
  | Discover discover ->
      match discover () (* exciting... *) with
      | Ok v -> v
      | Error (`Msg m) -> failwith (Topkg_string.strf "key %s: %s" k.name m)
  in
  match k.env with
  | None -> absent_field k
  | Some var ->
      match Topkg_os.Env.var var with
      | None -> absent_field k
      | Some ev ->
          match conv_parser k.conv ev with
          | Ok v -> v
          | Error (`Msg m) ->
              failwith (Topkg_string.strf "key %s: env %s: %s" k.name var m)

let with_pkg ?(default = true) pkg =
  let doc = Topkg_string.strf "true if package %s is installed." pkg in
  key ("with-" ^ pkg) bool ~absent:default ~doc

(* Predefined keys *)

let pkg_name =
  let doc = "The name $(docv) of the package (and hence the opam install \
             file). If absent provided by the package description."
  in
  let absent () = assert false (* handled specially by [of_cli_args] *) in
  discovered_key "pkg-name" string ~absent ~doc ~docv:"NAME"

let build_dir =
  let doc = "Specifies the build directory $(docv)." in
  let absent () = assert false (* handled specially by [of_cli_args] *) in
  discovered_key "build-dir" string ~absent ~doc ~docv:"BUILD_DIR"

let vcs =
  let doc = "Specifies if the package directory is VCS managed." in
  let absent () = Topkg_vcs.find ~dir:"." () >>= function
  | None -> Ok false
  | Some _ -> Ok true
  in
  discovered_key "vcs" bool ~absent ~doc

let pinned =
  let doc = "Deprecated, use --dev-pkg. The semantics is the same."
  in
  key "pinned" bool ~absent:false ~doc

let dev_pkg =
  let doc = "Specifies that the build is a dev package build (e.g. in opam)." in
  key "dev-pkg" bool ~absent:false ~doc


let tests =
  let doc = "Specifies whether tests should be built. If absent depends \
             on the build context, true for development and false otherwise."
  in
  let absent () = Ok None in
  discovered_key "tests" (some bool) ~absent ~doc

let debug =
  let doc = "Debug build. Save debugging information in build artefacts. This \
             key should not be specified explicitly in your package build \
             instructions."
  in
  key "debug" bool ~env:"TOPKG_CONF_DEBUG" ~absent:true ~doc

let debugger_support =
  let doc = "Debugger support. Build and install build artefacts needed \
             by debuggers. This key should not be specified explicitly in \
             your package build instructions."
  in
  key "debugger-support" bool ~env:"TOPKG_CONF_DEBUGGER_SUPPORT" ~absent:false
    ~doc

let profile =
  let doc = "Profiling build. Include run-time profiling support in build \
             artefacts. This key should not be specified explicitly \
             in your package build instructions."
  in
  key "profile" bool ~env:"TOPKG_CONF_PROFILE" ~absent:false ~doc

let toolchain =
  let doc = "Specifies the ocamlfind toolchain." in
  key "toolchain" (some string) ~env:"TOPKG_CONF_TOOLCHAIN" ~absent:None ~doc

(* Key documentation *)

let pp_cli_opt ppf opt_name absent env doc docv =
  let prf = Format.fprintf in
  let pp_doc ppf doc =
    let subst = function "docv" -> docv | s -> Topkg_string.strf "$(%s)" s in
    let b = Buffer.create 244 in
    let doc =
      try Buffer.add_substitute b subst doc; Buffer.contents b
      with Not_found -> doc
    in
    Topkg_string.pp_text ppf doc
  in
  let pp_absent absent env ppf () = match absent, env with
  | "", None -> ()
  | "", Some var -> prf ppf "@ (or %s env)" var
  | absent, None -> prf ppf "@ (absent=%s)" absent
  | absent, Some var -> prf ppf "@ (absent=%s or %s env)" absent var
  in
  prf ppf "@[<v4>@[%s %s %a@]@,@[%a@]@]"
    opt_name docv (pp_absent absent env) () pp_doc doc

let pp_key ppf k =
  let absent = match k.absent with
  | Discover _ -> "discovered"
  | Value v -> Topkg_string.strf "@[<h>%a@]" (conv_printer k.conv) v
  in
  let opt_name = match k.name with
  | "pkg-name" -> "-n NAME, --pkg-name" (* a bit ugly to special case *)
  | n -> Topkg_string.strf "--%s" n
  in
  let docv = conv_docv k.conv in
  pp_cli_opt ppf opt_name absent k.env k.doc docv

let pp_keys_cli_opts ppf () =
  let pp_key is_first (Key.V k) =
    if is_first then () else Format.pp_print_cut ppf ();
    pp_key ppf k; false
  in
  let by_name (Key.V k) (Key.V k') = compare k.name k'.name in
  let keys = List.sort by_name (Kset.elements !key_index) in
  Format.fprintf ppf "@[<v>";
  ignore (List.fold_left pp_key true keys);
  Format.fprintf ppf "@]";
  ()

(* Configurations *)

type t = univ Kmap.t

let empty = Kmap.empty
let is_empty = Kmap.is_empty
let mem k c = Kmap.mem (Key.V k) c
let add k v c = Kmap.add (Key.V k) (k.to_univ v) c
let rem k c = Kmap.remove (Key.V k) c
let find k c = try k.of_univ (Kmap.find (Key.V k) c) with Not_found -> None
let value c k = match find k c with
| Some v -> v
| None ->
    invalid_arg
      (Topkg_string.strf "configuration key %s undefined, did you create
         a key after the call to Pkg.describe ?" (* dirty bastard *) k.name)

let pp_value c ppf k = k.conv.print ppf (value c k)

let dump ppf c =
  let dump_binding (Key.V k) v is_first =
    if is_first then () else Format.pp_print_cut ppf ();
    match k.of_univ v with
    | None -> assert false
    | Some v ->
        Format.fprintf ppf "%s: @[%a@]" k.name (conv_printer k.conv) v;
        false
  in
  Format.fprintf ppf "@[<v>";
  ignore (Kmap.fold dump_binding c true);
  Format.fprintf ppf "@]";
  ()

let of_cli_args ~pkg_name:name ~build_dir:bdir args =
  let cli_opts = cli_opts_of_key_index () in
  let cli_opts = ("-n", Key.V pkg_name) :: cli_opts in
  let strf = Topkg_string.strf in
  let rec parse_keys conf = function (* WARNING raises *)
  | key :: def :: defs ->
      begin match try Some (List.assoc key cli_opts) with Not_found -> None with
      | None -> failwith (Topkg_string.strf "key %s: Unknown key." key)
      | Some (Key.V k) ->
          if mem k conf
          then failwith (strf "key %s: Repeated definition." key) else
          match (conv_parser k.conv) def with
          | Ok v -> parse_keys (add k v conf) defs
          | Error (`Msg e) -> failwith (strf "key %s: %s." key e)
      end
  | [] -> conf
  | key :: [] -> failwith (strf "key %s: No value specified." key)
  in
  let add_if_absent (Key.V k) conf = (* WARNING raises *)
    if mem k conf then conf else
    add k (key_absent_value k) conf
  in
  let ensure_pkg_name_and_bdir conf =
    let conf = if mem pkg_name conf then conf else add pkg_name name conf in
    if mem build_dir conf then conf else add build_dir bdir conf
  in
  try
    let cli_conf = ensure_pkg_name_and_bdir (parse_keys empty args) in
    Ok (Kset.fold add_if_absent !key_index cli_conf)
  with
  | Failure e -> R.error_msg e

let pkg_name c = value c pkg_name
let build_dir c = value c build_dir
let toolchain c = value c toolchain
let vcs c = value c vcs
let pinned c = value c pinned
let dev_pkg c = value c dev_pkg

type build_context = [`Dev | `Distrib | `Pin ]
let build_context c =
  if not (vcs c) then `Distrib else
  if (pinned c || dev_pkg c) then `Pin else
  `Dev

let build_tests c = match value c tests with
| Some b -> b
| None ->
    match build_context c with
    | `Dev -> true
    | _ -> false

let debug c = value c debug
let debugger_support c = value c debugger_support
let profile c = value c profile

(* Tool lookup *)

type os = [ `Build_os | `Host_os ]

let os_to_string = function
| `Build_os -> "build-os"
| `Host_os -> "host-os"

let os_tool_env name os =
  let pre = match os with `Build_os -> "BUILD_OS_" | `Host_os -> "HOST_OS_" in
  pre ^ Topkg_string.uppercase name

let os_bin_dir_env = function
| `Build_os -> "BUILD_OS_BIN"
| `Host_os -> "HOST_OS_XBIN"

let os_suff_env = function
| `Build_os -> "BUILD_OS_SUFF"
| `Host_os -> "HOST_OS_SUFF"

let ocamlfindable ?conf name os = match name with
| "ocamlc" | "ocamlcp" | "ocamlmktop" | "ocamlopt" | "ocamldoc" | "ocamldep"
| "ocamlmklib" | "ocamlbrowser" as tool ->
  let toolchain =
    match conf with
    | None -> Topkg_cmd.empty
    | Some c ->
      match os, toolchain c with
      | `Host_os, Some toolchain -> Topkg_cmd.(v "-toolchain" % toolchain)
      | _ -> Topkg_cmd.empty
  in
  Some Topkg_cmd.(v "ocamlfind" %% toolchain % tool)
| _ -> None

let tool ?conf name os = match Topkg_os.Env.var (os_tool_env name os) with
| Some cmd -> Topkg_cmd.v cmd
| None ->
    match Topkg_os.Env.var (os_bin_dir_env os) with
    | Some path -> Topkg_cmd.v Topkg_fpath.(path // name)
    | None ->
        match Topkg_os.Env.var (os_suff_env os) with
        | Some suff -> Topkg_cmd.v (name ^ suff)
        | None ->
            match ocamlfindable ?conf name os with
            | Some cmd -> cmd
            | None -> Topkg_cmd.v name

let get_ncpus () =
  let on_fail () = 1 in
  let decode_int = Topkg_codec.(dec_result int) in
  if Sys.win32 then
    match Topkg_os.Env.var "NUMBER_OF_PROCESSORS" with
    | Some s -> decode_int s |>
                Topkg_log.on_error_msg ~level:Topkg_log.Debug ~use:on_fail
    | None -> on_fail ()
  else
    let run_tool name args ~on_fail =
      Topkg_log.on_error_msg ~level:Topkg_log.Debug ~use:on_fail @@
      Topkg_os.Cmd.(run_out Topkg_cmd.(tool name `Build_os %% of_list args) |>
                    out_string |> success >>= decode_int)
    in
    run_tool "getconf" ["_NPROCESSORS_ONLN"] ~on_fail:(fun () ->
        run_tool "sysctl" ["-n"; "hw.ncpu"] ~on_fail)

let jobs =
  let doc = "Allow to run $(docv) commands at once when building." in
  let absent () = Ok None in
  discovered_key "jobs" (some int) ~absent ~doc ~docv:"JOBS"

let default_jobs = 4
let jobs c = match value c jobs with
| Some n -> n
| None ->
    match build_context c with
    | `Dev -> get_ncpus ()
    | _ -> default_jobs

(* OCaml configuration, as communicated by ocamlc -config  *)

module OCaml = struct

  type conf = t

  (* Log strings *)

  let conf fmt = "OCaml %s conf: " ^^ fmt
  let conf_key fmt = conf ("key %s: " ^^ fmt)

  (* Configuration *)

  type t =
    { os : os; mutable conf : (string * string) list;
      (* Mutability is only used to add the value found by a discover
         procedure for keys that are not yet exposed in ocamlc -config.
         See http://caml.inria.fr/mantis/view.php?id=7172 *) }

  let empty os = { os; conf = [] }

  let read_config c os =
    let parse_line acc l = match Topkg_string.cut ~sep:':' l with
    | Some (k, v) -> (k, String.trim v) :: acc
    | None ->
        Topkg_log.warn (fun m ->
            m (conf "cannot parse line %S") (os_to_string os) l);
        acc
    in
    begin
      let ocamlc = tool ?conf:c "ocamlc" os in
      Topkg_os.Cmd.(run_out Topkg_cmd.(ocamlc % "-config") |> to_lines)
      >>= fun lines -> Ok (List.(rev (fold_left parse_line [] lines)))
      >>= fun conf -> Ok { os; conf }
    end
    |> R.reword_error_msg ~replace:true
      (fun msg -> R.msgf (conf " %s") (os_to_string os) msg)
    |> Topkg_log.on_error_msg ~level:Topkg_log.Warning ~use:(fun () -> empty os)

  let cache = Hashtbl.create 2
  let v c os =
    try Hashtbl.find cache (c, os)
    with Not_found ->
      let config = read_config (Some c) os in
      Hashtbl.add cache (c, os) config;
      config

  let add_discovery k v c = c.conf <- (k, v) :: c.conf
  let find k c = try Some (List.assoc k c.conf) with Not_found -> None
  let get ~absent k c = match find k c with
  | Some v -> v
  | None ->
      Topkg_log.warn (fun m ->
          m (conf_key "undefined, using %S") (os_to_string c.os) k absent);
      absent

  let get_string_with_discovery k c ~discover = match find k c with
  | Some v -> v
  | None -> let v = discover k c in add_discovery k v c; v

  let get_bool_with_discovery k c ~discover =
    let maybe_v = match find k c with
    | None -> None
    | Some v ->
        try Some (bool_of_string v) with
        | (* That good old joke... *) Invalid_argument _ ->
            Topkg_log.warn (fun m ->
              m (conf_key "could not parse boolean,@ trying to discover")
                (os_to_string c.os) k);
            None
    in
    match maybe_v with
    | Some v -> v
    | None -> let v = discover k c in add_discovery k (string_of_bool v) c; v

  let get_int_with_discovery k c ~discover =
    let maybe_v = match find k c with
    | None -> None
    | Some v ->
      try Some (int_of_string v) with
      | Failure _ ->
          Topkg_log.warn (fun m ->
              m (conf_key "could not parse integer,@ trying to discover")
                (os_to_string c.os) k);
          None
   in
   match maybe_v with
   | Some v -> v
   | None -> let v = discover k c in add_discovery k (string_of_int v) c; v

  let find_stdlib c = find "standard_library" c

  let get_bool_stdlib_file_exists_discovery k c ~file ~on_error =
    get_bool_with_discovery k c ~discover:begin fun k c ->
      match find_stdlib c with
      | None ->
          Topkg_log.warn (fun m ->
              m (conf_key
                   "undefined, stdlib dir not found for discovery@ using %B")
                (os_to_string c.os) k on_error);
          on_error
      | Some stdlib_dir ->
          match Topkg_os.File.exists (Topkg_fpath.(stdlib_dir // file c)) with
          | Ok exist -> exist
          | Error (`Msg e) ->
              Topkg_log.warn (fun m ->
                m (conf_key "undefined,@ discovery error: %s,@ using %B")
                  (os_to_string c.os) k e on_error);
              on_error
    end

  let version c = (* parses the specification described in Sys.ocaml_version *)
    let dumb_version = 0, 0, 0, None in
    let k = "version" in
    match find k c with
    | None ->
        Topkg_log.warn (fun m ->
          m (conf_key "missing, using 0.0.0") (os_to_string c.os) k);
        dumb_version
    | Some version ->
        match Topkg_string.parse_version version with
        | Some version -> version
        | None ->
            Topkg_log.warn (fun m ->
                m (conf_key "cannot parse from %S, using 0.0.0")
                  (os_to_string c.os) k version);
            dumb_version

  let ext_obj c = get ~absent:".o" "ext_obj" c
  let ext_asm c = get ~absent:".s" "ext_asm" c
  let ext_lib c = get ~absent:".a" "ext_lib" c
  let ext_dll c = get ~absent:".so" "ext_dll" c
  let ext_exe c =
    get_string_with_discovery "ext_exe" c ~discover:begin fun k c ->
      (* Not exposed until at least 4.03. The discover logic is based on
         the knowledge articulated in this message:
         http://lists.ocaml.org/pipermail/wg-windows/2015-July/000037.html *)
      let find_c_toolchain c = match find "ccomp_type" c, find "os_type" c with
      | None, _  | _, None -> None
      | Some ccomp_type, Some os_type ->
          match ccomp_type, os_type with
          | "msvc", _  -> Some `Win_msvc
          | "cc", "Win32" -> Some `Win_cc
          | _, _  -> Some `Other
      in
      match find_c_toolchain c with
      | Some (`Win_msvc | `Win_cc) -> ".exe"
      | Some `Other -> ""
      | None ->
          Topkg_log.warn (fun m ->
            m (conf_key "undefined and@ no C toolchain@ detected,@ using \"\"")
              (os_to_string c.os) k;);
          ""
    end

  let native c =
    let file c = "libasmrun" ^ (ext_lib c) in
    get_bool_stdlib_file_exists_discovery "native" c ~file ~on_error:false

  let native_dynlink c =
    let file _ = "dynlink.cmxa" in
    get_bool_stdlib_file_exists_discovery "natdynlink" c ~file ~on_error:false

  let word_size c =
    let sizeof_ptr_of_config_h file =
      let err l =
        R.error_msgf "could not parse SIZEOF_PTR from %S in %s" l file
      in
      let rec parse = function
      | [] -> R.error_msgf "could not find SIZEOF_PTR in %s" file
      | l :: ls ->
          let l = Topkg_string.trim l in
          let is_size_of_ptr = Topkg_string.is_prefix "#define SIZEOF_PTR" l in
          if not is_size_of_ptr then parse ls else
          match Topkg_string.cut ~rev:true ~sep:' ' l with
          | None -> err l
          | Some (_, size) ->
              try Ok (int_of_string size * 8) with Failure _ -> err l
      in
      Topkg_os.File.read file
      >>= fun conf -> Ok (Topkg_string.cuts ~sep:'\n' conf)
      >>= fun lines -> parse lines
    in
    get_int_with_discovery "word_size" c ~discover:begin fun k c ->
      let on_error = 64 in
      match find_stdlib c with
      | None ->
          Topkg_log.warn (fun m ->
              m (conf_key
                   "undefined, stdlib dir not found for discovery@ using %d")
                (os_to_string c.os) k on_error);
          on_error
      | Some stdlib_dir ->
          let config_h = "caml/config.h" in
          match
            Topkg_os.File.must_exist Topkg_fpath.(stdlib_dir // config_h)
            >>= fun conf -> sizeof_ptr_of_config_h conf
          with
          | Ok v -> v
          | Error (`Msg e) ->
              Topkg_log.warn (fun m ->
                m (conf_key "undefined,@ discovery error: %s,@ using %d")
                  (os_to_string c.os) k e on_error);
              on_error
    end

  let dump ppf c =
    let pp_elt ppf (k, v) = Format.fprintf ppf "(%S, %S)" k v in
    let rec loop = function
    | [] -> ()
    | v :: vs ->
        if vs = [] then (Format.fprintf ppf "@[%a@]" pp_elt v) else
        (Format.fprintf ppf "@[%a@];@ " pp_elt v; loop vs)
    in
    Format.fprintf ppf "@[<1>["; loop c.conf; Format.fprintf ppf "]@]"
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
