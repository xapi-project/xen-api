let config_mk = "config.mk"

(* Configure script *)
open Cmdliner

let dir name default docv doc =
  let doc = Printf.sprintf "Set the directory for installing %s" doc in
  Arg.(value & opt string default & info [name] ~docv ~doc)

let path name default docv doc =
  let doc = Printf.sprintf "Set the path for %s" doc in
  Arg.(value & opt string default & info [name] ~docv ~doc)

(* xapi *)

let coverage =
  let doc = "Enable coverage profiling" in
  Arg.(value & flag & info ["enable-coverage"] ~doc)

let disable_warn_error =
  let doc = "Disable -warn-error (default is enabled for development)" in
  Arg.(value & flag & info ["disable-warn-error"] ~doc)

let varpatchdir = dir "varpatchdir" "/var/patch" "VARPATCHDIR" "hotfixes"

let etcxendir =
  dir "etcxendir" "/etc/xensource" "ETCXENDIR" "configuration files"

let optdir = dir "optdir" "/opt/xensource" "OPTDIR" "system files"

let plugindir = dir "plugindir" "/etc/xapi.d/plugins" "PLUGINDIR" "xapi plugins"

let extensiondir =
  dir "extensiondir" "/etc/xapi.d/extensions" "PLUGINDIR" "XenAPI extensions"

let hooksdir = dir "hooksdir" "/etc/xapi.d" "HOOKSDIR" "hook scripts"

let inventory =
  path "inventory" "/etc/xensource-inventory" "INVENTORY" "the inventory file"

let xapiconf =
  dir "xapiconf" "/etc/xapi.conf" "XAPICONF" "xapi master config file"

let libexecdir =
  dir "libexecdir" "/opt/xensource/libexec" "LIBEXECDIR" "utility binaries"

let scriptsdir =
  dir "scriptsdir" "/etc/xensource/scripts" "SCRIPTSDIR" "utility scripts"

let sharedir = dir "sharedir" "/opt/xensource" "SHAREDIR" "shared binary files"

let webdir = dir "webdir" "/opt/xensource/www" "WEBDIR" "html files"

let cluster_stack_root =
  dir "cluster-stack-root" "/usr/libexec/xapi/cluster-stack"
    "CLUSTER_STACK_ROOT" "cluster stacks"

let udevdir = dir "udevdir" "/etc/udev" "UDEVDIR" "udev scripts"

let docdir = dir "docdir" "/usr/share/xapi/doc" "DOCDIR" "XenAPI documentation"

let sdkdir = dir "sdkdir" "/usr/share/xapi/sdk" "SDKDIR" "XenAPI SDK"

(* xenopsd *)

let bindir = dir "bindir" "/usr/bin" "BINDIR" "binaries"

let sbindir = dir "sbindir" "/usr/sbin" "SBINDIR" "superuser binaries"

let xenopsd_libexecdir =
  dir "xenopsd_libexecdir" "/usr/lib/xenopsd" "XENOPSD_LIBEXECDIR"
    "xenopsd helper executables"

let qemu_wrapper_dir =
  dir "qemu_wrapper_dir" "/usr/lib/xenopsd" "QEMU_WRAPPER_DIR"
    "xen helper executables"

let etcdir = dir "etcdir" "/etc" "ETCDIR" "configuration files"

let mandir = dir "mandir" "/usr/share/man" "MANDIR" "manpages"

let find_ocamlfind verbose name =
  let found =
    try
      let (_ : string) = Findlib.package_property [] name "requires" in
      true
    with
    | Not_found ->
        (* property within the package could not be found *)
        true
    | Findlib.No_such_package (_, _) ->
        false
  in
  if verbose then
    Printf.fprintf stderr "querying for ocamlfind package %s: %s" name
      (if found then "ok" else "missing") ;
  found

let expand start finish input output =
  let command =
    Printf.sprintf "cat %s | sed -r 's=%s=%s=g' > %s" input start finish output
  in
  if Sys.command command <> 0 then (
    Printf.fprintf stderr "Failed to expand %s -> %s in %s producing %s\n" start
      finish input output ;
    Printf.fprintf stderr "Command-line was:\n%s\n%!" command ;
    exit 1
  )

let output_file filename lines =
  let oc = open_out filename in
  List.iter (fun line -> Printf.fprintf oc "%s\n" line) lines ;
  close_out oc

let find_xentoollog verbose =
  let c_program =
    ["int main(int argc, const char *argv){"; "  return 0;"; "}"]
  in
  let c_file = Filename.temp_file "configure" ".c" in
  let exe_file = c_file ^ ".exe" in
  output_file c_file c_program ;
  let found =
    Sys.command
      (Printf.sprintf "cc -Werror %s -lxentoollog -o %s %s" c_file exe_file
         (if verbose then "" else "2>/dev/null")
      )
    = 0
  in
  if Sys.file_exists c_file then Sys.remove c_file ;
  if Sys.file_exists exe_file then Sys.remove exe_file ;
  Printf.printf "Looking for xentoollog: %s\n"
    (if found then "found" else "missing") ;
  output_file "ocaml/xenopsd/xentoollog_flags"
    (if found then ["-L/lib64"; "-lxentoollog"] else []) ;
  found

(* general *)
let yumplugindir =
  dir "yumplugindir" "/usr/lib/yum-plugins" "YUMPLUGINDIR" "YUM plugins"

let yumpluginconfdir =
  dir "yumpluginconfdir" "/etc/yum/pluginconf.d" "YUMPLUGINCONFDIR"
    "YUM plugins conf dir"

let info =
  let doc = "Configures a package" in
  Term.info "configure" ~version:"0.1" ~doc

let output_file filename lines =
  let oc = open_out filename in
  let lines = List.map (fun line -> line ^ "\n") lines in
  List.iter (output_string oc) lines ;
  close_out oc

let yesno_of_bool = function true -> "YES" | false -> "NO"

let able_of_bool = function true -> "enable" | false -> "disable"

let configure coverage disable_warn_error varpatchdir etcxendir optdir plugindir
    extensiondir hooksdir inventory xapiconf libexecdir scriptsdir sharedir
    webdir cluster_stack_root udevdir docdir sdkdir bindir sbindir
    xenopsd_libexecdir qemu_wrapper_dir etcdir mandir yumplugindir
    yumpluginconfdir =
  let xenctrl = find_ocamlfind false "xenctrl" in
  let xentoollog = find_xentoollog false in
  (* Write config.mk *)
  let vars =
    [
      ("BISECT_ENABLE", yesno_of_bool coverage)
    ; ("DISABLE_WARN_ERROR", string_of_bool disable_warn_error)
    ; ("VARPATCHDIR", varpatchdir)
    ; ("ETCXENDIR", etcxendir)
    ; ("OPTDIR", optdir)
    ; ("PLUGINDIR", plugindir)
    ; ("EXTENSIONDIR", extensiondir)
    ; ("HOOKSDIR", hooksdir)
    ; ("INVENTORY", inventory)
    ; ("XAPICONF", xapiconf)
    ; ("LIBEXECDIR", libexecdir)
    ; ("SCRIPTSDIR", scriptsdir)
    ; ("SHAREDIR", sharedir)
    ; ("WEBDIR", webdir)
    ; ("CLUSTER_STACK_ROOT", cluster_stack_root)
    ; ("UDEVDIR", udevdir)
    ; ("DOCDIR", docdir)
    ; ("SDKDIR", sdkdir)
    ; ("BINDIR", bindir)
    ; ("SBINDIR", sbindir)
    ; ("XENOPSD_LIBEXECDIR", xenopsd_libexecdir)
    ; ("QEMU_WRAPPER_DIR", qemu_wrapper_dir)
    ; ("ETCDIR", etcdir)
    ; ("MANDIR", mandir)
    ; ("ENABLE_XEN", Printf.sprintf "--%s-xen" (able_of_bool xenctrl))
    ; ( "ENABLE_XENTOOLLOG"
      , Printf.sprintf "--%s-xentoollog" (able_of_bool xentoollog)
      )
    ; ("YUMPLUGINDIR", yumplugindir)
    ; ("YUMPLUGINCONFDIR", yumpluginconfdir)
    ]
  in
  let lines = List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) vars in
  let export =
    Printf.sprintf "export %s" (vars |> List.map fst |> String.concat " ")
  in
  let header =
    [
      "# Warning - this file is autogenerated by the configure script"
    ; "# Do not edit"
    ]
  in
  Printf.printf "Configuring with the following parameters\n" ;
  Printf.printf "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n" ;
  List.iter
    (fun (k, v) -> Printf.printf "%20s = %s\n" (String.lowercase_ascii k) v)
    vars ;
  output_file config_mk (header @ lines @ [export]) ;
  (* Expand @LIBEXEC@ in udev rules *)
  expand "@LIBEXEC@" xenopsd_libexecdir "ocaml/xenopsd/scripts/vif.in"
    "ocaml/xenopsd/scripts/vif" ;
  expand "@LIBEXEC@" xenopsd_libexecdir
    "ocaml/xenopsd/scripts/xen-backend.rules.in"
    "ocaml/xenopsd/scripts/xen-backend.rules"

let configure_t =
  Term.(
    pure configure
    $ coverage
    $ disable_warn_error
    $ varpatchdir
    $ etcxendir
    $ optdir
    $ plugindir
    $ extensiondir
    $ hooksdir
    $ inventory
    $ xapiconf
    $ libexecdir
    $ scriptsdir
    $ sharedir
    $ webdir
    $ cluster_stack_root
    $ udevdir
    $ docdir
    $ sdkdir
    $ bindir
    $ sbindir
    $ xenopsd_libexecdir
    $ qemu_wrapper_dir
    $ etcdir
    $ mandir
    $ yumplugindir
    $ yumpluginconfdir
  )

let () =
  match Term.eval (configure_t, info) with `Error _ -> exit 1 | _ -> exit 0
