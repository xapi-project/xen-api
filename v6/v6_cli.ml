(* Licensing CLI *)

module Cmds =V6_interface.RPC_API(Cmdlinergen.Gen ())

let version_str description =
  let maj,min,mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

let default_cmd =
  let doc = String.concat "" [
    "A CLI for the V6d API. This allows scripting of the licensing daemon ";
    "for testing and debugging. This tool is not intended to be used as an ";
    "end user tool"] in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "licensing_cli" ~version:(version_str Cmds.description) ~doc

let cli () =
  let rpc = V6_client.rpc in
  Cmdliner.Term.eval_choice default_cmd (List.map (fun t -> t rpc) (Cmds.implementation ()))

let _ = cli ()
