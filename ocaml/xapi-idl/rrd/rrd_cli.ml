(* Rrd CLI *)

module Cmds = Rrd_interface.RPC_API (Cmdlinergen.Gen ())

let doc =
  String.concat ""
    [
      "A CLI for the Db monitoring API. This allows scripting of the Rrd "
    ; "daemon for testing and debugging. This tool is not intended to be "
    ; "used as an end user tool"
    ]

let cmdline_gen () =
  List.map (fun t -> t Rrd_client.rpc) (Cmds.implementation ())

let cli =
  Xcp_service.cli ~name:"rrd_cli" ~doc ~version:Cmds.description.version
    ~cmdline_gen

let () = Xcp_service.eval_cmdline cli
