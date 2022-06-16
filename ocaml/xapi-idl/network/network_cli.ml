(* Network CLI *)

open Network_interface

module Cmds = Interface_API (Cmdlinergen.Gen ())

let doc =
  String.concat ""
    [
      "A CLI for the network API. This allows scripting of the xcp-networkd "
    ; "daemon for testing and debugging. This tool is not intended to be used "
    ; "as an end user tool"
    ]

let cmdline_gen () =
  List.map (fun t -> t Network_client.rpc) (Cmds.implementation ())

let cli =
  Xcp_service.cli ~name:"network_cli" ~doc ~version:Cmds.description.version
    ~cmdline_gen

let () = Xcp_service.eval_cmdline cli
