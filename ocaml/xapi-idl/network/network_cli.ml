(* Network CLI *)

open Network_interface

module Cmds = Interface_API (Cmdlinergen.Gen ())

let version_str description =
  let maj, min, mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

let doc =
  String.concat ""
    [
      "A CLI for the network API. This allows scripting of the xcp-networkd "
    ; "daemon for testing and debugging. This tool is not intended to be used "
    ; "as an end user tool"
    ]

open! Cmdliner

let cmds =
  List.map
    (fun t ->
      let term, inf = t Network_client.rpc in
      Cmd.v inf term
    )
    (Cmds.implementation ())

let cli () =
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    Cmd.info "network_cli" ~version:(version_str Cmds.description) ~doc
  in
  let cmd = Cmd.group ~default info cmds in
  Cmd.eval_value cmd

let () = match cli () with Ok (`Ok f) -> f () | _ -> ()
