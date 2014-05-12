(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Client
open Xstringext
open Utils

let get_fc_sr rpc session =
  let srs = Client.SR.get_all_records rpc session in
  let hba_srs = List.filter (fun (sr,sr_rec) -> sr_rec.API.sR_type = "lvmohba") srs in
  match hba_srs with
  | [] -> raise (Multipathrt_exceptions.Test_error "couldn't find an LVMoHBA SR")
  | (sr,sr_rec)::_ -> sr

(* Note: could also get this from the PBD's other-config:SCSIid field *)
let get_fc_scsiid rpc session sr =
  let sm_config = Client.SR.get_sm_config rpc session sr in
  if not(List.mem_assoc "devserial" sm_config) then raise (Multipathrt_exceptions.Test_error "couldn't find devserial key in sm-config");
  let devserial = List.assoc "devserial" sm_config in
  (* strip the leading 'scsi-' *)
  if String.startswith "scsi-" devserial then String.sub_to_end devserial 5
  else raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expected string '%s' to begin 'scsi-'" devserial))

let get_num_paths rpc session sr scsiid =
  let pbds = Client.SR.get_PBDs rpc session sr in
  match pbds with
  | [] -> raise (Multipathrt_exceptions.Test_error "can't find any PBDs for the SR")
  | pbd::_ ->
      let other_config = Client.PBD.get_other_config rpc session pbd in
      let key = Printf.sprintf "mpath-%s" scsiid in
      let value = List.assoc key other_config in
      Scanf.sscanf value "[%d, %d]" (fun cur_paths max_paths -> max_paths)

(* Note: this function only works for QLogic HBAs because it uses 'scli'. Emulex HBAs won't work. *)
let get_hba_state ~rpc ~session ~host =
  (* Find out the current HBA state *)
  let hba_state = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"get_hba_status" ~args:[] in
  let wwpns = String.split ',' hba_state in
  let wwpns = List.map (fun str ->
    let fields = String.split ':' str in
    match fields with
    | [wwpn; "Online"] -> (wwpn, `online)
    | [wwpn; "Offline"] -> (wwpn, `offline)
    | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Couldn't parse HBA state. Expected '<wwpn>:<status>', got '%s'" str))
  ) wwpns in
  List.iter (fun (wwpn, status) -> debug "HBA status: WWPN %s is %s" wwpn (if status = `online then "online" else "offline")) wwpns;
  wwpns

let modify_fc_paths_manual rpc session host remove =
  let rec request_user_edit () =
    (* Ask the user to remove the path *)
    let verb = if remove then "remove" else "reinstate" in
    Printf.printf "Please %s all but one of the paths by logging into the FibreChannel switch, then press Enter\n> %!" verb;
    Scanf.scanf "%s\n" (fun _ -> ());
  
(* Don't do this because invoking 'scli -t' waits while the failover happens!
    (* Check that a path has been removed *)
    debug "Checking path change...";
    let new_hba_state = get_hba_state rpc session host in
    if check_paths new_hba_state then
      debug "Manual path change was successful"
    else begin
      if remove then debug "Did not find an offline path. Try again!" else debug "At least one path is still offline. Try again!";
      request_user_edit ()
    end
*)
  in

(* Commented out because this assumes Qlogic because it uses 'scli'
  (* Get initial HBA state *)
  let initial_hba_state = get_hba_state rpc session host in

  (* Assert that all the paths must be online initially *)
  assert_initial_state_okay initial_hba_state;
*)

  (* Ask the user to remove a path, until one finally goes offline *)
  request_user_edit ()


