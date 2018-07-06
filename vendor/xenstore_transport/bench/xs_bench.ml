(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_protocol
module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
open Client

let ( |> ) a b = b a

(* So we can run against a real xenstore, place all nodes in a subtree *)
let prefix = "/bench"

let getdomainpath domid client =
  immediate client (fun xs -> getdomainpath xs domid)
  >>= fun dom_path ->
  return (prefix ^ dom_path)

let readdir d client =
  Lwt.catch (fun () ->
      immediate client (fun xs -> directory xs d)
    ) (function
      | Xs_protocol.Enoent _ ->
        return []
      | e -> Lwt.fail e
    )

let read_opt path xs =
  Lwt.catch (fun () ->
      read xs path
      >>= fun x ->
      Lwt.return (Some x)
    ) (function
      | Xs_protocol.Enoent _ ->
        return None
      | e -> Lwt.fail e
    )

let exists path xs = read_opt path xs >|= (fun x -> x <> None)

module Device = struct

  type kind = Vif | Vbd | Tap | Pci | Vfs | Vfb | Vkbd

  let kind_of_string = function
    | "vif" -> Some Vif | "vbd" -> Some Vbd | "tap" -> Some Tap
    | "pci" -> Some Pci | "vfs" -> Some Vfs | "vfb" -> Some Vfb
    | "vkbd" -> Some Vkbd
    | x -> None

  let string_of_kind = function
    | Vif -> "vif" | Vbd -> "vbd" | Tap -> "tap" | Pci -> "pci" | Vfs -> "vfs" | Vfb -> "vfb" | Vkbd -> "vkbd"

  type devid = int
  (** Represents one end of a device *)
  type endpoint = { domid: int; kind: kind; devid: int }

  (** Represent a device as a pair of endpoints *)
  type device = {
    frontend: endpoint;
    backend: endpoint
  }

  let parse_int i =
    try
      Some (int_of_string i)
    with _ -> None

  let rec split ?limit:(limit=(-1)) c s =
    let i = try String.index s c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
      [ s ]
    else
      let a = String.sub s 0 i
      and b = String.sub s (i + 1) (String.length s - i - 1) in
      a :: (split ~limit: nlimit c b)

  let parse_backend_link x =
    match split '/' x with
    | [ ""; "local"; "domain"; domid; "backend"; kind; _; devid ] ->
      begin
        match parse_int domid, kind_of_string kind, parse_int devid with
        | Some domid, Some kind, Some devid ->
          Some { domid = domid; kind = kind; devid = devid }
        | _, _, _ -> None
      end
    | _ -> None

  let to_list xs = List.fold_left (fun acc x -> match x with
      | Some x -> x :: acc
      | None -> acc
    ) [] xs

  let list_kinds dir client =
    readdir dir client >|= List.map kind_of_string >|= to_list

  (* NB: we only read data from the frontend directory. Therefore this gives
     the "frontend's point of view". *)
  let list_frontends domid client =
    getdomainpath domid client
    >>= fun dom_path ->
    let frontend_dir = dom_path ^ "/device" in
    list_kinds frontend_dir client
    >>= fun kinds ->

    Lwt_list.map_s
      (fun k ->
         let dir = Printf.sprintf "%s/%s" frontend_dir (string_of_kind k) in
         readdir dir client >|= List.map parse_int >|= to_list
         >>= fun devids ->
         Lwt_list.map_s
           (fun devid ->
              (* domain [domid] believes it has a frontend for
                 					   device [devid] *)
              let frontend = { domid = domid; kind = k; devid = devid } in
              Lwt.catch (fun () ->
                  immediate client
                    (fun xs ->
                       read xs (Printf.sprintf "%s/%d/backend" dir devid)
                       >>= fun x ->
                       match parse_backend_link x with
                       | Some b -> return (Some { backend = b; frontend = frontend })
                       | None -> return None
                    )
                ) (fun _ -> return None)
           ) devids >|= to_list
      ) kinds
    >>= fun ll ->
    return (List.concat ll)

  (** Location of the backend in xenstore *)
  let backend_path_of_device (x: device) client =
    getdomainpath x.backend.domid client
    >>= fun dom_path ->
    return (Printf.sprintf "%s/backend/%s/%u/%d"
              dom_path
              (string_of_kind x.backend.kind)
              x.frontend.domid x.backend.devid)

  (** Location of the backend error path *)
  let backend_error_path_of_device (x: device) client =
    getdomainpath x.backend.domid client
    >>= fun dom_path ->
    return (Printf.sprintf "%s/error/backend/%s/%d"
              dom_path
              (string_of_kind x.backend.kind)
              x.frontend.domid)

  (** Location of the frontend in xenstore *)
  let frontend_path_of_device (x: device) client =
    getdomainpath x.backend.domid client
    >>= fun dom_path ->
    return (Printf.sprintf "%s/device/%s/%d"
              dom_path
              (string_of_kind x.frontend.kind)
              x.frontend.devid)

  (** Location of the frontend error node *)
  let frontend_error_path_of_device (x: device) client =
    getdomainpath x.frontend.domid client
    >>= fun dom_path ->
    return (Printf.sprintf "%s/error/device/%s/%d/error"
              dom_path
              (string_of_kind x.frontend.kind)
              x.frontend.devid)

  let hard_shutdown_request (x: device) client =
    backend_path_of_device x client
    >>= fun backend_path ->
    frontend_path_of_device x client
    >>= fun frontend_path ->
    let online_path = backend_path ^ "/online" in
    immediate client
      (fun xs ->
         write xs online_path "0"
         >>= fun () ->
         rm xs frontend_path
      )

  (* We store some transient data elsewhere in xenstore to avoid it getting
     deleted by accident when a domain shuts down. We should always zap this
     tree on boot. *)
  let private_path = prefix ^ "/xapi"

  (* The private data path is only used by xapi and ignored by frontend and backend *)
  let get_private_path domid = Printf.sprintf "%s/%d" private_path domid

  let get_private_data_path_of_device (x: device) =
    Printf.sprintf "%s/private/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

  (* Path in xenstore where we stuff our transient hotplug-related stuff *)
  let get_hotplug_path (x: device) =
    Printf.sprintf "%s/hotplug/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

  let get_private_data_path_of_device (x: device) =
    Printf.sprintf "%s/private/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

  let rm_device_state (x: device) client =
    immediate client
      (fun xs ->
         frontend_path_of_device x client
         >>= fun fe ->
         backend_path_of_device x client
         >>= fun be ->
         backend_error_path_of_device x client
         >>= fun ber ->
         frontend_error_path_of_device x client
         >>= fun fer ->
         Lwt_list.iter_s (rm xs) [ fe; be; ber; Filename.dirname fer ]
      )

  let hard_shutdown device client =
    hard_shutdown_request device client
    >>= fun () ->
    rm_device_state device client

  let add device client =
    let backend_list = []
    and frontend_list = []
    and private_list = [] in

    frontend_path_of_device device client
    >>= fun frontend_path ->
    backend_path_of_device device client
    >>= fun backend_path ->
    let hotplug_path = get_hotplug_path device in
    let private_data_path = get_private_data_path_of_device device in
    transaction client
      (fun xs ->
         exists (Printf.sprintf "/local/domain/%d/vm" device.backend.domid) xs
         >>= fun _ ->
         exists frontend_path xs
         >>= fun _ ->
         Lwt.catch (fun () -> rm xs frontend_path) (fun _ -> return ())
         >>= fun () ->
         Lwt.catch (fun () -> rm xs backend_path) (fun _ -> return ())
         >>= fun () ->

         (* CA-16259: don't clear the 'hotplug_path' because this is where we
            			   record our own use of /dev/loop devices. Clearing this causes us to leak
            			   one per PV .iso *)
         mkdir xs frontend_path
         >>= fun () ->
         setperms xs frontend_path (Xs_protocol.ACL.({owner = device.frontend.domid; other = NONE; acl = [ device.backend.domid, READ ]}))
         >>= fun () ->
         mkdir xs backend_path
         >>= fun () ->
         setperms xs backend_path (Xs_protocol.ACL.({owner = device.backend.domid; other = NONE; acl = [ device.frontend.domid, READ ]}))
         >>= fun () ->
         mkdir xs hotplug_path
         >>= fun () ->
         setperms xs hotplug_path (Xs_protocol.ACL.({owner = device.backend.domid; other = NONE; acl = []}))
         >>= fun () ->
         Lwt_list.iter_s (fun (x, y) -> write xs (frontend_path ^ "/" ^ x) y)
           (("backend", backend_path) :: frontend_list)
         >>= fun () ->
         Lwt_list.iter_s (fun (x, y) -> write xs (backend_path ^ "/" ^ x) y)
           (("frontend", frontend_path) :: backend_list)
         >>= fun () ->
         mkdir xs private_data_path
         >>= fun () ->
         setperms xs private_data_path (Xs_protocol.ACL.({owner = device.backend.domid; other = NONE; acl = []}))
         >>= fun () ->
         Lwt_list.iter_s (fun (x, y) -> write xs (private_data_path ^ "/" ^ x) y)
           (("backend-kind", string_of_kind device.backend.kind) ::
            ("backend-id", string_of_int device.backend.domid) :: private_list)

      )
end

module Domain = struct

  let make domid client =
    (* create /local/domain/<domid> *)
    (* create 3 VBDs, 1 VIF (workaround transaction problem?) *)
    getdomainpath domid client
    >>= fun dom_path ->
    let uuid = Printf.sprintf "uuid-%d" domid in
    let name = "name" in
    let vm_path = prefix ^ "/vm/" ^ uuid in
    let vss_path = prefix ^ "/vss/" ^ uuid in
    let xsdata = [
      "xsdata", "xsdata"
    ] in
    let platformdata = [
      "platformdata", "platformdata"
    ] in
    let bios_strings = [
      "bios_strings", "bios_strings"
    ] in
    let roperm = Xs_protocol.ACL.({owner = 0; other = NONE; acl = [ domid, READ ]}) in
    let rwperm = Xs_protocol.ACL.({owner = domid; other = NONE; acl = []}) in
    transaction client
      (fun xs ->
         (* Clear any existing rubbish in xenstored *)
         Lwt.catch (fun () -> rm xs dom_path) (fun _ -> return ())
         >>= fun () ->
         mkdir xs dom_path
         >>= fun () ->
         setperms xs dom_path roperm
         >>= fun () ->
         (* The /vm path needs to be shared over a localhost migrate *)
         immediate client (exists vm_path)
         >>= fun vm_exists ->
         ( if not vm_exists then begin
               mkdir xs vm_path
               >>= fun () ->
               setperms xs vm_path roperm
               >>= fun () ->
               write xs (vm_path ^ "/uuid") uuid
               >>= fun () ->
               write xs (vm_path ^ "/name") name
             end else return () )
         >>= fun () ->
         write xs (Printf.sprintf "%s/domains/%d" vm_path domid) dom_path
         >>= fun () ->

         mkdir xs vss_path
         >>= fun () ->
         setperms xs vss_path rwperm
         >>= fun () ->

         write xs (dom_path ^ "/vm") vm_path
         >>= fun () ->
         write xs (dom_path ^ "/vss") vss_path
         >>= fun () ->
         write xs (dom_path ^ "/name") name
         >>= fun () ->

         (* create cpu and memory directory with read only perms *)
         Lwt_list.iter_s (fun dir ->
             let ent = Printf.sprintf "%s/%s" dom_path dir in
             mkdir xs ent
             >>= fun () ->
             setperms xs ent roperm
           ) [ "cpu"; "memory" ]
         >>= fun () ->
         (* create read/write nodes for the guest to use *)
         Lwt_list.iter_s (fun dir ->
             let ent = Printf.sprintf "%s/%s" dom_path dir in
             mkdir xs ent
             >>= fun () ->
             setperms xs ent rwperm
           ) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages"; "vm-data" ]
      )
    >>= fun () ->
    immediate client
      (fun xs ->

         Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ "/" ^ x) y) xsdata
         >>= fun () ->

         Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ "/platform/" ^ x) y) platformdata
         >>= fun () ->
         Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ "/bios-strings/" ^ x) y) bios_strings
         >>= fun () ->

         (* If a toolstack sees a domain which it should own in this state then the
            			   domain is not completely setup and should be shutdown. *)
         write xs (dom_path ^ "/action-request") "poweroff"
         >>= fun () ->

         write xs (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1"
         >>= fun () ->

         (* CA-30811: let the linux guest agent easily determine if this is a fresh domain even if
            			   the domid hasn't changed (consider cross-host migrate) *)
         write xs (dom_path ^ "/unique-domain-id") uuid
      )

  let control_shutdown domid client =
    getdomainpath domid client >|= (fun x -> x ^ "/control/shutdown")

  let string_of_shutdown_reason _ = "halt"

  let get_uuid domid = Printf.sprintf "uuid-%d" domid

  (** Request a shutdown, return without waiting for acknowledgement *)
  let shutdown domid req client =
    let reason = string_of_shutdown_reason req in
    control_shutdown domid client
    >>= fun path ->
    getdomainpath domid client
    >>= fun dom_path ->
    transaction client
      (fun xs ->
         (* Fail if the directory has been deleted *)
         immediate client (exists dom_path)
         >>= fun domain_exists ->
         if domain_exists then begin
           write xs path reason
           >>= fun () ->
           return true
         end else return false
      )

  let destroy domid client =
    getdomainpath domid client
    >>= fun dom_path ->
    (* These are the devices with a frontend in [domid] and a well-formed backend
       	   in some other domain *)
    Device.list_frontends domid client
    >>= fun all_devices ->

    (* Forcibly shutdown every backend *)
    Lwt_list.iter_s
      (fun device ->
         Device.hard_shutdown device client
      ) all_devices
    >>= fun () ->
    (* Remove our reference to the /vm/<uuid> directory *)
    immediate client (read_opt (dom_path ^ "/vm"))
    >>= fun vm_path ->
    immediate client (read_opt (dom_path ^ "/vss"))
    >>= fun vss_path ->
    begin match vm_path with
      | Some vm_path ->
        immediate client
          (fun xs ->
             rm xs (vm_path ^ "/domains/" ^ (string_of_int domid))
             >>= fun () ->
             readdir (vm_path ^ "/domains") client
             >>= fun domains ->
             if List.filter (fun x -> x <> "") domains = [] then begin
               rm xs vm_path
               >>= fun () ->
               begin match vss_path with
                 | Some vss_path -> rm xs vss_path
                 | None -> return ()
               end
             end else return ()
          )
      | None -> return ()
    end
    >>= fun () ->
    immediate client (fun xs -> rm xs dom_path)
    >>= fun () ->
    getdomainpath 0 client >|= (fun x -> x ^ "/backend")
    >>= fun backend_path ->
    readdir backend_path client
    >>= fun all_backend_types ->
    Lwt_list.iter_s
      (fun ty ->
         immediate client (fun xs -> rm xs (Printf.sprintf "%s/%s/%d" backend_path ty domid))
      ) all_backend_types
end

let vm_shutdown domid client =
  Domain.shutdown domid () client
  >>= fun _ ->
  Domain.destroy domid client

let vm_start domid client =
  let vbd devid = {
    Device.frontend = { Device.domid = domid; kind = Device.Vbd; devid = 0 };
    backend = { Device.domid = 0; kind = Device.Vbd; devid = 0 }
  } in
  Domain.make domid client
  >>= fun () ->
  Lwt_list.iter_s (fun d -> Device.add d client) [ vbd 0; vbd 1; vbd 2 ]

let vm_cycle domid client =
  vm_start domid client
  >>= fun () ->
  vm_shutdown domid client

let rec between start finish =
  if start > finish
  then []
  else start :: (between (start + 1) finish)

let sequential n client : unit Lwt.t =
  Lwt_list.iter_s
    (fun domid ->
       vm_cycle domid client
    ) (between 0 n)

let parallel n client =
  Lwt_list.iter_p
    (fun domid ->
       vm_cycle domid client
    ) (between 0 n)

let query m n client =
  Lwt_list.iter_s
    (fun domid ->
       vm_start domid client
    ) (between 0 n)
  >>= fun () ->
  let rec loop i =
    if i = m then Lwt.return_unit else begin
      Lwt_list.iter_p
        (fun domid ->
           immediate client (fun xs -> read xs (Printf.sprintf "%s/local/domain/%d/name" prefix domid) >>= fun _ -> Lwt.return_unit)
        ) (between 0 n)
      >>= fun () ->
      loop (i + 1)
    end in
  loop 0
  >>= fun () ->
  Lwt_list.iter_s
    (fun domid ->
       vm_shutdown domid client
    ) (between 0 n)


let time f =
  let start = Unix.gettimeofday () in
  f ()
  >>= fun () ->
  return (Unix.gettimeofday () -. start)

let usage () =
  let bin x = Sys.argv.(0) ^ x in
  let lines = [
    bin " : a xenstore benchmark tool";
    "";
    "Usage:";
    bin " [-path /var/run/xenstored/socket] [-n number of vms]";
  ] in
  List.iter (fun x -> Printf.fprintf stderr "%s\n" x) lines

let main () =
  let verbose = ref false in
  let args = Sys.argv |> Array.to_list |> List.tl in
  (* Look for "-h" or "-v" arguments *)
  if List.mem "-h" args then begin
    usage ();
    return ();
  end else begin
    verbose := List.mem "-v" args;
    let args = List.filter (fun x -> x <> "-v") args in
    (* Extract any -path X argument *)
    let extract args key =
      let result = ref None in
      let args =
        List.fold_left (fun (acc, foundit) x ->
            if foundit then (result := Some x; (acc, false))
            else if x = key then (acc, true)
            else (x :: acc, false)
          ) ([], false) args |> fst |> List.rev in
      !result, args in
    let path, args = extract args "-path" in
    begin match path with
      | Some path -> Xs_transport.xenstored_socket := path
      | None -> ()
    end;
    let n, args = extract args "-n" in
    let n = match n with
      | None -> 300
      | Some n -> int_of_string n in

    make ()
    >>= fun client ->

    time (fun () -> sequential n client)
    >>= fun t ->
    Lwt_io.write Lwt_io.stdout (Printf.sprintf "%d sequential starts and shutdowns: %.02f\n" n t)
    >>= fun () ->
    time (fun () -> parallel n client)
    >>= fun t ->
    Lwt_io.write Lwt_io.stdout (Printf.sprintf "%d parallel starts and shutdowns: %.02f\n" n t)
    >>= fun () ->
    time (fun () -> query 1000 n client)
    >>= fun t ->
    Lwt_io.write Lwt_io.stdout (Printf.sprintf "%d read queries per %d VMs: %.02f\n" 1000 n t)
  end

let _ =
  Lwt_main.run (main ())
