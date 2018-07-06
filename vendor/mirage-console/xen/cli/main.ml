(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let project_url = "http://github.com/mirage/mirage-console"

open Lwt.Infix
module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
open Result

let backend_name =
  let sanitise x =
    let x' = String.length x in
    let y = Bytes.create x' in
    for i = 0 to x' - 1 do
      Bytes.set y i
        (match x.[i] with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> x.[i]
         | _ -> '_')
    done;
    y in
  sanitise (Filename.basename Sys.argv.(0))

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
  }
  (** options common to all subcommands *)

  let make verbose debug = { verbose; debug }
end

let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let read_one client k =
  Client.immediate client (fun xs ->
      Lwt.catch
        (fun () -> Client.read xs k >|= fun v -> Ok v)
        (fun _ -> Lwt.return (Error ("failed to read: " ^ k)))
    )

let exists client k =
  read_one client k >|= function
  | Error _ -> false
  | Ok _    -> true

let find_vm client vm =
  (* First interpret as a domain ID, then UUID, then name *)
  let domainpath x = "/local/domain/" ^ x in
  exists client (domainpath vm) >>= fun e ->
  if e
  then Lwt.return (Some (int_of_string vm))
  else begin
    Client.immediate client
      (fun xs -> Client.directory xs "/local/domain") >>= fun valid_domids ->
    Lwt_list.map_s (fun d ->
        read_one client ("/local/domain/" ^ d ^ "/vm") >|= function
        | Ok path -> Some (Filename.basename path)
        | Error _ -> None
      ) valid_domids
    >>= fun valid_uuids ->
    Lwt_list.map_s (fun d ->
        read_one client ("/local/domain/" ^ d ^ "/name") >|= function
        | Ok path -> Some path
        | Error _ -> None
      ) valid_domids
    >|= fun valid_names ->
    let uuids_to_domids = List.combine valid_uuids valid_domids in
    let names_to_domids = List.combine valid_names valid_domids in
    if List.mem_assoc (Some vm) uuids_to_domids
    then Some (int_of_string (List.assoc (Some vm) uuids_to_domids))
    else if List.mem_assoc (Some vm) names_to_domids
    then Some (int_of_string (List.assoc (Some vm) names_to_domids))
    else None
  end

let find_free_console client vm =
  Client.immediate client (fun xs ->
      Lwt.catch (fun () ->
          Client.directory xs
            (Printf.sprintf "/local/domain/%d/device/console" vm)
        ) (function Xs_protocol.Enoent _ -> Lwt.return [] | e -> Lwt.fail e)
    )
  >|= fun used ->
  used
  |> List.map (fun x -> try Some (int_of_string x) with _ -> None)
  |> List.fold_left
    (fun acc this -> match this with Some x -> x :: acc | None -> acc) []
  |> List.fold_left max 0 (* console 0 is handled specially *)
  |> (fun x -> x + 1)

let connect' (vm: string) (name: string option) =
  Client.make () >>= fun client ->
  (* Figure out where the device is going to go: *)
  (find_vm client vm >>= function
  | Some vm -> Lwt.return vm
  | None    -> Lwt.fail (Failure (Printf.sprintf "Failed to find VM %s" vm)))
  >>= fun vm ->
  Printf.fprintf stderr "Operating on VM domain id: %d\n%!" vm;
  find_free_console client vm >>= fun devid ->
  Printf.fprintf stderr "Creating device/console/%d\n" devid;
  (match name with
   | None ->
     Printf.fprintf stderr "Device has no 'name', default rules will apply and \
                            it will become /dev/hvcX\n%!"
   | Some name ->
     Printf.fprintf stderr "Device has 'name = %s', look for a device in \
                            /dev/xenconsole/%s\n%!" name name);
  let device = vm, devid in

  let module M = Conback.Make(Unix_activations)(Client)(Console_unix) in

  (* If we're asked to shutdown cleanly, first initiate a hot-unplug.
     If we're asked again, be more aggressive. *)
  let already_asked = ref false in
  let shutdown_signal _ =
    if not(!already_asked) then begin
      already_asked := true;
      Printf.fprintf stderr "Received signal, requesting hot-unplug.\n%!";
      let (_: unit Lwt.t) = M.request_close backend_name device in
      ()
    end else begin
      Printf.fprintf stderr "Received signal again, tearing down the backend.\n%!";
      let (_: unit Lwt.t) = M.force_close device in
      ()
    end in
  List.iter
    (fun signal ->
      let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal signal shutdown_signal in
      ()
    ) [ Sys.sigint; Sys.sigterm ];

  M.create ?name backend_name device >>= fun () ->
  M.run "" backend_name device >>= fun stats ->
  Printf.fprintf stderr "# console stats\n";
  Printf.fprintf stderr "Total read:     %d\n" stats.Conback.total_read;
  Printf.fprintf stderr "Total written:  %d\n" stats.Conback.total_write;
  M.destroy backend_name device

let plug' ?(backend="0") (vm: string) (name: string option) =
  Client.make () >>= fun client ->
  (* Figure out where the device is going to go: *)
  (find_vm client vm >>= function
    | Some vm -> Lwt.return vm
    | None    -> Lwt.fail_with (Printf.sprintf "Failed to find VM %s" vm))
  >>= fun vm ->
  (find_vm client backend >>= function
    | Some vm -> Lwt.return vm
    | None    -> Lwt.fail_with (Printf.sprintf "Failed to find VM %s" backend))
  >>= fun backend ->
  Printf.fprintf stderr "Backend VM domain id: %d\n" backend;
  Printf.fprintf stderr "Frontend VM domain id: %d\n%!" vm;
  find_free_console client vm >>= fun devid ->
  Printf.fprintf stderr "Creating device/console/%d\n" devid;
  (match name with
   | None ->
     Printf.fprintf stderr "Device has no 'name', default rules will apply and \
                            it will become /dev/hvcX\n%!"
    | Some name -> Printf.fprintf stderr "Device has 'name = %s', look for a \
                                          device in /dev/xenconsole/%s\n%!"
                     name name);
  let device = vm, devid in
  let module M = Conback.Make(Unix_activations)(Client)(Console_unix) in
  M.create ?name ~backend_domid:backend "console" device >|= fun () ->
  Printf.fprintf stderr "Device connection initiated\n%!"

let connect (_common: Common.t) (vm: string) (name: string option) =
  match vm with
  | "" ->
    `Error(true, "I don't know which VM to operate on. Please supply a VM name \
                  or uuid.")
  | vm -> Lwt_main.run (connect' vm name); `Ok ()

let plug (_common: Common.t) (backend: string option) (vm: string)
    (name: string option) =
  match vm with
  | "" ->
    `Error(true, "I don't know which VM to operate on. Please supply a VM \
                  name or uuid.")
  | vm -> Lwt_main.run (plug' ?backend vm name); `Ok ()

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
 `S _common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  Term.(pure Common.make $ debug $ verb)

let console_name =
  let doc = "Name for this console (visible to the VM)" in
  Arg.(value & opt (some string) None & info [ "name" ]~doc)

let connect_command =
  let doc = "Connect a console to a specific VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Connect a console to a specific VM.";
    `P "This command creates a virtual console in the specified VM and implements it locally. Reads and writes to the local terminal will be proxied to the remote console and vice-versa.";
  ] in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect to." in
    Arg.(required & pos 0 (some string) None & info [ ] ~docv:"VM-name-or-uuid" ~doc) in
  Term.(ret (pure connect $ common_options_t $ vm $ console_name )),
  Term.info "connect" ~sdocs:_common_options ~doc ~man

let plug_command =
  let doc = "Plug a console frontend into a backend." in
  let man = [
    `S "DESCRIPTION";
    `P "";
  ] in
  let backend =
    let doc = "The domain, UUID or name of the backend VM." in
    Arg.(value & opt (some string) None & info [ "backend" ] ~doc) in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect to." in
    Arg.(required & pos 0 (some string) None & info [ ] ~docv:"VM-name-or-uuid" ~doc) in
  Term.(ret (pure plug $ common_options_t $ backend $ vm $ console_name )),
  Term.info "plug" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate virtual console devices on Xen virtual machines" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "mirage-console" ~version:"0.1" ~sdocs:_common_options ~doc ~man

let cmds = [ connect_command; plug_command ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
