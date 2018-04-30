open Cmdliner

let gen_markdown path =
  let open Rpc.Types in
  let open Xapi_storage in
  let open Files in

  List.iter
    (fun api ->
       with_output_file (Printf.sprintf "%s/%s.md" path api.Codegen.Interfaces.name)
         (fun oc ->
            let p = Markdowngen.to_string api in
            output_string oc p)) Apis.apis;
  `Ok ()

let gen_python path =
  let open Rpc.Types in
  let open Xapi_storage in
  let open Files in

  List.iter
    (fun api ->
       with_output_file (Printf.sprintf "%s/%s.py" path api.Codegen.Interfaces.name)
         (fun oc ->
            let p = Pythongen.of_interfaces api |> Pythongen.string_of_ts in
            output_string oc p
         )
    ) Apis.apis;
  `Ok ()

let gen_python_cmd =
  let doc = "Generate the python library files" in
  let path =
    let doc = "Generate the files in the path specified" in
    Arg.(value & opt string ("./xapi/storage/api/v4") & info ["p";"path"] ~doc ~docv:"PATH")
  in
  Term.(ret (const gen_python $ path)),
  Term.info "gen_python" ~doc ~exits:Term.default_exits

let gen_markdown_cmd =
  let doc = "Generate documentation files in markdown format" in
  let path =
    let doc = "Generate the files in the path specified" in
    Arg.(value & opt string (".") & info ["p";"path"] ~doc ~docv:"PATH")
  in
  Term.(ret (const gen_markdown $ path)),
  Term.info "gen_markdown" ~doc ~exits:Term.default_exits

let default_cmd =
  let doc = "SMAPI code/documentation generation tool" in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Term.info "main" ~doc ~exits:Term.default_exits

let _ =
  Term.(exit @@ eval_choice default_cmd [gen_python_cmd; gen_markdown_cmd])
