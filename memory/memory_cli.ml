(* Memory CLI *)

open Memory_interface

module Cmds = API(Cmdlinergen.Gen ())

let version_str description =
  let maj,min,mic = description.Idl.Interface.version in
  Printf.sprintf "%d.%d.%d" maj min mic

let default_cmd =
  let doc = String.concat "" [
    "A CLI for the memory API. This allows scripting of the squeeze daemon ";
    "for testing and debugging. This tool is not intended to be used as an ";
    "end user tool"] in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "memory_cli" ~version:(version_str Cmds.description) ~doc

let cli () =
  let rpc = Memory_client.rpc in
  match Cmdliner.Term.eval_choice default_cmd (List.map (fun t -> t rpc) (Cmds.implementation ())) with
  | `Ok f -> f ()
  | _ -> ()

let _ = cli ()
