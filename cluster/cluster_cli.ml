(* Cluster CLI *)

open Cluster_interface

module Cmds = LocalAPI(Cmdlinergen.Gen ())

let version_str description =
  let maj,min,mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

let default_cmd =
  let doc = String.concat "" [
    "A CLI for the cluster API. This tool is not intended to be used as an ";
    "end user tool"] in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "cluster_cli" ~version:(version_str Cmds.description) ~doc

let cli () =
  let rpc = Cluster_client.rpc Cluster_client.json_url in
  Cmdliner.Term.eval_choice default_cmd (List.map (fun t -> t rpc) (Cmds.implementation ()))

let _ = cli ()
