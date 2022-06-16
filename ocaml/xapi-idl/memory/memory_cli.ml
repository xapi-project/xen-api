(* Memory CLI *)

open Memory_interface

module Cmds = API (Cmdlinergen.Gen ())

let doc =
  String.concat ""
    [
      "A CLI for the memory API. This allows scripting of the squeeze "
    ; "daemon for testing and debugging. This tool is not intended to be "
    ; "used as an end user tool"
    ]

let cmdline_gen () =
  List.map (fun t -> t Memory_client.rpc) (Cmds.implementation ())

let cli =
  Xcp_service.cli ~name:"memory_cli" ~doc ~version:Cmds.description.version
    ~cmdline_gen

let () = Xcp_service.eval_cmdline cli
