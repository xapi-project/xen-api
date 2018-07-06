(*$ #use "src/cinaps_helpers" $*)
let usage_msg =
  Printf.sprintf "Usage: %s <input-ast> [-to-ocaml40x <output-ast>]"
    Sys.argv.(0)

let conversions = ref []
let input = ref ""

let () =
  let add v name = conversions := (v, name) :: !conversions in
  let set_input name =
    if !input = "" then input := name
    else
      raise (Arg.Bad (Printf.sprintf
                        "You can pass only one input filename (got %S and %S)"
                        !input name))
  in
  let arg_spec = [
    (*$ foreach_version (fun suffix version ->
          printf "(\"-to-ocaml%s\", Arg.String (add Migrate_parsetree.OCaml_%s),\n" suffix suffix;
          printf "\"<filename> Produce an ast valid for OCaml %s in <filename>\");\n" version;
        )
    *)
    ("-to-ocaml402", Arg.String (add Migrate_parsetree.OCaml_402),
     "<filename> Produce an ast valid for OCaml 4.02 in <filename>");
    ("-to-ocaml403", Arg.String (add Migrate_parsetree.OCaml_403),
     "<filename> Produce an ast valid for OCaml 4.03 in <filename>");
    ("-to-ocaml404", Arg.String (add Migrate_parsetree.OCaml_404),
     "<filename> Produce an ast valid for OCaml 4.04 in <filename>");
    ("-to-ocaml405", Arg.String (add Migrate_parsetree.OCaml_405),
     "<filename> Produce an ast valid for OCaml 4.05 in <filename>");
    ("-to-ocaml406", Arg.String (add Migrate_parsetree.OCaml_406),
     "<filename> Produce an ast valid for OCaml 4.06 in <filename>");
    ("-to-ocaml407", Arg.String (add Migrate_parsetree.OCaml_407),
     "<filename> Produce an ast valid for OCaml 4.07 in <filename>");
    (*$*)
  ] in
  Arg.parse arg_spec set_input usage_msg;
  if !input = "" then (
    Arg.usage arg_spec usage_msg;
    exit 1
  );
  let (src_filename, ast) =
    let ic = open_in_bin !input in
    match Migrate_parsetree.from_channel ic with
    | exception (Migrate_parsetree.Unknown_magic_number number) ->
      Printf.eprintf "Input file has unknown magic number: %s\n" number;
      close_in ic;
      exit 1
    | exception exn -> close_in ic; raise exn
    | ast -> close_in ic; ast
  in
  Printf.printf "Ast of %S for OCaml %s\n"
    src_filename (Migrate_parsetree.string_of_ocaml_version
                    (Migrate_parsetree.ast_version ast));
  List.iter (fun (version, dst_filename) ->
      match
        let ast' = Migrate_parsetree.migrate_to_version ast version in
        let oc = open_out_bin dst_filename in
        Migrate_parsetree.to_channel oc src_filename ast';
        close_out_noerr oc
      with
      | () ->
        Printf.printf "Successfully converted %S to OCaml %s in %S\n"
          !input (Migrate_parsetree.string_of_ocaml_version version) dst_filename
      | exception exn ->
        Printf.eprintf "Failed to convert %S to OCaml %s in %S:\n%s%!\n"
          !input (Migrate_parsetree.string_of_ocaml_version version) dst_filename
          (Printexc.to_string exn)
    ) (List.rev !conversions)
