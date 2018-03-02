
let _ =
  let open Types in
  let open Files in

  List.iter
    (fun api ->
       with_output_file (Printf.sprintf "../../../../python/xapi/storage/api/v4/%s.py" api.Codegen.Interfaces.name)
         (fun oc ->
            let p = Pythongen.of_interfaces api |> Pythongen.string_of_ts in
            output_string oc p
         )
    ) Apis.apis
