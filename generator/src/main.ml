
let _ =
  let gen_lib = ref false in
  let gen_examples = ref false in
  let gen_python = ref false in
  Arg.parse [
    "-lib",  Arg.Set gen_lib, "Output ocaml library files";
    "-examples", Arg.Set gen_examples, "Output ocaml examples";
    "-python",   Arg.Set gen_python, "Outputpython files";
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

  if !gen_python then
    List.iter
      (fun api ->
         with_output_file (Printf.sprintf "xapi/storage/api/%s.py" api.Interfaces.name)
           (fun oc ->
              let idents, api = resolve_refs_in_api api in
              output_string oc (Python.of_interfaces idents api |> Python.string_of_ts)
           )
      ) apis;

  if !gen_lib then
    List.iter
      (fun api ->
         with_output_file (Printf.sprintf "%s.ml" api.Interfaces.name)
           (fun oc ->
              let idents, api = resolve_refs_in_api api in
              output_string oc (Ocaml.of_interfaces idents api |> Ocaml.string_of_ts)
           )
      ) apis;

  if !gen_examples then
    List.iter
      (fun api ->
         let idents, api = resolve_refs_in_api api in
         Ocaml.write_examples (Printf.sprintf "%s" api.Interfaces.name) idents api
      ) apis
