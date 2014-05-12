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
open Xstringext
open Client

let get_netapp_sr rpc session =
  let srs = Client.SR.get_all_records rpc session in
  let netapp_srs = List.filter (fun (sr,srr) -> srr.API.sR_type = "netapp") srs in
  match netapp_srs with
  | [] -> raise (Multipathrt_exceptions.Test_error "couldn't find a NetApp SR")
  | (sr,srr)::_ -> sr

(* Each sm-config key is a SCSIid, associated with the UUID of the VDI. *)
let get_netapp_vdi_scsiid rpc session sr vdi =
  let vdi_uuid = Client.VDI.get_uuid ~rpc ~session_id:session ~self:vdi in
  let sm_config = Client.SR.get_sm_config ~rpc ~session_id:session ~self:sr in
  let sm_config = List.filter (fun (sm_config_scsiid, sm_config_vdi_uuid) -> sm_config_vdi_uuid = vdi_uuid) sm_config in
  match sm_config with
  | [] -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "did not find VDI with uuid '%s' in SR's sm-config" vdi_uuid))
  | (str,_)::_ ->
      (* strip the leading 'scsi-' *)
      if String.startswith "scsi-" str then String.sub_to_end str 5
      else raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expected string '%s' to begin 'scsi-'" str))

(* Returns a list of IP addresses *)
let get_paths_to_netapp_sr rpc session sr host =
  let pbds = Client.SR.get_PBDs rpc session sr in
  let pbds = List.map (fun pbd -> (pbd, Client.PBD.get_record rpc session pbd)) pbds in
  (* Find the PBD which corresponds to this host *)
  let pbds = List.filter (fun (pbd, pbdr) -> pbdr.API.pBD_host = host) pbds in
  match pbds with
  | [] -> raise (Multipathrt_exceptions.Test_error "couldn't find an appropriate PBD for the NetApp SR")
  | (pbd,pbdr) :: _ ->
      (* Get the 'multihomelist' from device-config, e.g. 'multihomelist: 10.80.225.95:3260,10.80.226.246:3260' *)
      let device_config = pbdr.API.pBD_device_config in
      if not(List.mem_assoc "multihomelist" device_config) then raise (Multipathrt_exceptions.Test_error "no 'multihomelist' in PBD's device-config");
      let multihomelist_str = List.assoc "multihomelist" device_config in
      let multihomelist = String.split ',' multihomelist_str in
      List.map (fun ipport ->
        match String.split ':' ipport with
        | [ip;"3260"] -> ip
        | _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "expected '<ip>:3260', found '%s' in multihomelist" ipport))
      ) multihomelist
