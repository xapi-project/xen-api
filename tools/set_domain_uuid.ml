(* Set the uuid of the specified domain *)

(* Intended use case is to set dom0's uuid *)

let is_uuid_valid uuid =
  match Uuidm.of_string uuid with
  | None -> false
  | Some _ -> true

let set domain uuid =
  if not (is_uuid_valid uuid) then begin
    `Error (false, "Invalid uuid");
  end else begin
    let xc = Xenctrl.interface_open () in
    try
      Xenctrl.domain_sethandle xc domain uuid;
      `Ok ()
    with e ->
      `Error (false, Printf.sprintf "Caught exception while setting uuid: %s" (Printexc.to_string e))
  end

open Cmdliner

let info =
  let doc = "Utility to set a domain's uuid" in
  let man = [] in
  Term.info "set_domain_uuid" ~version:"1.0" ~doc ~man

let uuid =
  let doc = "Uuid of the domain" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"UUID" ~doc)

let domid =
  let doc = "Id of the domain" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"DOMID" ~doc)

let cmd =
  Term.(ret (pure set $ domid $ uuid))

let () =
  match Term.eval (cmd, info) with
  | `Error _ -> exit 1
  | _ -> exit 0

