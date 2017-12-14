
let config_ml= "lib/xcp_inventory_config.ml"

(* Configure script *)
open Cmdliner

let default_inventory =
  let doc = "Set the default filename for the inventory" in
  Arg.(value & opt string "/etc/xcp/inventory" & info ["default_inventory"] ~docv:"DEFAULT_INVENTORY" ~doc)

let info =
  let doc = "Configures a package" in
  Term.info "configure" ~version:"0.1" ~doc

let output_file filename lines =
  let oc = open_out filename in
  let lines = List.map (fun line -> line ^ "\n") lines in
  List.iter (output_string oc) lines;
  close_out oc

let configure inventory =
  let lines =
    [
      "default_inventory", inventory;
    ] in
  print_endline "Configuring with:";
  lines |> List.map (fun (x,y) -> Printf.sprintf "\t%s=%s" x y) |> List.iter print_endline;
  lines |> List.map (fun (x,y) -> Printf.sprintf "let %s=\"%s\"" x y) |> output_file config_ml

let configure_t = Term.(pure configure $ default_inventory)

let () =
  match
    Term.eval (configure_t, info)
  with
  | `Error _ -> exit 1
  | _ -> exit 0
