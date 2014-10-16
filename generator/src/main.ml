
let _ =
  let generate_html = ref false in
  Arg.parse [
    "-html", Arg.Set generate_html, "Output HTML docs";
  ] (fun x -> Printf.fprintf stderr "Unknown argument: %s\n%!" x; exit 1)
  "Generate OCaml/Python/HTML documentation";

  let open Types in
  let open Files in
  let apis = [
    Plugin.api;
    Control.api;
    Data.api;
  ] in
  (* Prepend the debug_info argument *)
  let apis = List.map Types.prepend_dbg apis in

  if !generate_html
  then Www.write apis;

  List.iter
    (fun api ->
       with_output_file (Printf.sprintf "python/%s.py" api.Interfaces.name)
         (fun oc ->
            let idents, api = resolve_refs_in_api api in
            output_string oc (Python.of_interfaces idents api |> Python.string_of_ts)
         )
    ) apis;

  List.iter
    (fun api ->
       with_output_file (Printf.sprintf "ocaml/lib/%s.ml" api.Interfaces.name)
         (fun oc ->
            let idents, api = resolve_refs_in_api api in
            Ocaml.write_examples (Printf.sprintf "ocaml/examples/%s" api.Interfaces.name) idents api;
            output_string oc (Ocaml.of_interfaces idents api |> Ocaml.string_of_ts)
         )
    ) apis
