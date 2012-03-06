open Pervasiveext

let with_file filename f =
  let oc = open_out filename in
  finally (fun () -> f oc) (fun () -> close_out oc)

let print_file_to oc =
  let output_line oc txt = output_string oc txt; output_string oc "\n" in
  Unixext.file_lines_iter (output_line oc)

let _ =
(*
  print_string (to_dbus_xml smapiv2);
  print_string "";
  print_string "\n";
  print_string "";
  print_string (to_json smapiv2);
  print_string "\n";
  print_string "";
  to_rpclight smapiv2;
  print_string "";
*)
  let resolve_refs_in_api api =
    let idents, api = Types.lift_type_decls api in
    Types.dump_ident_mappings idents;
    idents, (Types.resolve_references idents api) in

  with_file "doc/smapiv2.html"
      (fun oc ->
	print_file_to oc ("doc/header.html");
	let idents, api = resolve_refs_in_api Smapiv2.api in
	output_string oc (Types.to_html idents api);
	print_file_to oc ("doc/footer.html")
      );
  with_file "doc/xenops.html"
    (fun oc ->
	print_file_to oc ("doc/header.html");
	let idents, api = resolve_refs_in_api Xenops.api in
	output_string oc (Types.to_html idents api);
	print_file_to oc ("doc/footer.html")
    )


