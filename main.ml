let print_file_to oc = Unixext.file_lines_iter (output_string oc)

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
  let oc = open_out "doc/smapiv2.html" in
  print_file_to oc ("doc/header.html");
  output_string oc (Types.to_html Smapiv2.api);
  print_file_to oc ("doc/footer.html");
  close_out oc

