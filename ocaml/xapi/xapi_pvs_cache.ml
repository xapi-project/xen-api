(*
 * Copyright (C) 2016 Citrix Systems Inc.
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
open Stdext
open Listext
open Threadext

exception No_cache_sr_available
exception No_cache_vdi_present

module VDI = struct
  (* TODO: make this configurable. *)
  let size = Int64.of_int (20 * 1024 * 1024 * 1024)

  let find_all ~__context ~sr =
    let open Db_filter_types in
    Db.VDI.get_refs_where ~__context ~expr:(And
                                              (Eq (Field "SR", Literal (Ref.string_of sr)),
                                               Eq (Field "type", Literal "pvs_cache")))

  let find_one ~__context ~sr =
    match find_all ~__context ~sr with
    | [] -> None
    | vdi :: _ -> Some vdi

  let create ~__context ~sr =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.VDI.create ~rpc ~session_id
          ~name_label:"PVS cache VDI"
          ~name_description:"PVS cache VDI"
          ~sR:sr
          ~virtual_size:size
          ~_type:`pvs_cache
          ~sharable:false
          ~read_only:false
          ~other_config:[]
          ~xenstore_data:[]
          ~sm_config:[]
          ~tags:[])
end

let check_cache_availability ~__context ~host ~site =
  match
    List.filter
      (fun sr -> Helpers.host_has_pbd_for_sr ~__context ~host ~sr)
      (Db.PVS_site.get_cache_storage ~__context ~self:site)
  with
  | [] -> None
  | srs -> begin
      let sorted_srs =
        Helpers.sort_by_schwarzian
          (fun sr -> Db.SR.get_uuid ~__context ~self:sr) srs
      in
      match
        List.filter_map
          (fun sr ->
             VDI.find_one ~__context ~sr
             |> Opt.map (fun vdi -> sr, vdi))
          sorted_srs
      with
      | [] -> Some (List.hd sorted_srs, None)
      | (sr, vdi) :: _ -> Some (sr, Some vdi)
    end

let cache_m = Mutex.create ()

let find_or_create_cache_vdi ~__context ~host ~site =
  Mutex.execute cache_m (fun () ->
      match check_cache_availability ~__context ~host ~site with
      | None -> raise No_cache_sr_available
      | Some (sr, None) -> sr, VDI.create ~__context ~sr
      | Some (sr, Some vdi) -> sr, vdi)

let find_cache_vdi ~__context ~sr =
  match VDI.find_one ~__context ~sr with
  | None -> raise No_cache_vdi_present
  | Some vdi -> vdi

let on_sr_remove ~__context ~sr =
  match VDI.find_all ~__context ~sr with
  | [] -> ()
  | vdis ->
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         List.iter
           (fun vdi -> Client.VDI.destroy ~rpc ~session_id ~self:vdi)
           vdis)
