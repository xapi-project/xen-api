(* Licensing CLI *)

module Cmds = V6_interface.RPC_API (Cmdlinergen.Gen ())

let version_str description =
  let maj, min, mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

open! Cmdliner

let cmds =
  List.map
    (fun t ->
      let t, i = t V6_client.rpc in
      Cmd.v i t
    )
    (Cmds.implementation ())

let cli () =
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  let info =
    let doc =
      String.concat ""
        [
          "A CLI for the V6d API. This allows scripting of the licensing daemon "
        ; "for testing and debugging. This tool is not intended to be used as "
        ; "an end user tool"
        ]
    in
    Cmd.info "licensing_cli" ~version:(version_str Cmds.description) ~doc
  in
  let cmd = Cmd.group ~default info cmds in
  Cmd.eval_value cmd

let () = match cli () with Ok (`Ok f) -> f () | _ -> ()
