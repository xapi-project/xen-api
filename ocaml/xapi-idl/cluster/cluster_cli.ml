(* Cluster CLI *)

open Cluster_interface

module Cmds = LocalAPI (Cmdlinergen.Gen ())

let doc =
  String.concat ""
    [
      "A CLI for the cluster API. This tool is not intended to be used as "
    ; "an end user tool"
    ]

let cmdline_gen () =
  List.map
    (fun t -> t (Cluster_client.rpc_internal Cluster_client.json_url))
    (Cmds.implementation ())

let cli =
  Xcp_service.cli ~name:"cluster_cli" ~doc ~version:Cmds.description.version
    ~cmdline_gen

let () = Xcp_service.eval_cmdline cli
