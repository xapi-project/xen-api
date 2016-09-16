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

module E = Api_errors

let api_error msg xs = raise (E.Server_error (msg, xs))
let str ref = Ref.string_of ref

let create_vdi ~__context ~sR ~size =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.VDI.create ~rpc ~session_id
        ~name_label:"PVS cache VDI"
        ~name_description:"PVS cache VDI"
        ~sR
        ~virtual_size:size
        ~_type:`pvs_cache
        ~sharable:false
        ~read_only:false
        ~other_config:[]
        ~xenstore_data:[]
        ~sm_config:[]
        ~tags:[]
    )

let destroy_vdi ~__context ~self =
  let vdi = Db.PVS_cache_storage.get_VDI ~__context ~self in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       Client.Client.VDI.destroy ~rpc ~session_id ~self:vdi
    )

let assert_not_already_present ~__context site host =
  let caches = Db.PVS_site.get_cache_storage ~__context ~self:site in
  let hosts = List.map (fun pcs -> Db.PVS_cache_storage.get_host ~__context ~self:pcs) caches in
  if List.mem host hosts then
    api_error E.pvs_cache_storage_already_present [str site; str host]

let create ~__context ~host ~sR ~site ~size =
  assert_not_already_present ~__context site host;
  let cache_storage = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  let vDI = create_vdi ~__context ~sR ~size in
  Db.PVS_cache_storage.create ~__context ~ref:cache_storage ~uuid ~host ~sR ~site ~vDI ~size;
  cache_storage

let assert_not_in_use ~__context self =
  let site = Db.PVS_cache_storage.get_site ~__context ~self in
  let host = Db.PVS_cache_storage.get_host ~__context ~self in
  let in_use =
    Pvs_proxy_control.get_running_proxies ~__context ~site
    |> List.map (fun self -> Db.PVS_proxy.get_VIF ~__context ~self)
    |> List.map (fun self -> Db.VIF.get_VM ~__context ~self)
    |> List.exists (fun self -> Db.VM.get_resident_on ~__context ~self = host)
  in
  if in_use then
    api_error E.pvs_cache_storage_is_in_use [str self]

let destroy ~__context ~self =
  assert_not_in_use ~__context self;
  let site = Db.PVS_cache_storage.get_site ~__context ~self in
  Pvs_proxy_control.remove_site_on_localhost ~__context ~site;
  destroy_vdi ~__context ~self;
  Db.PVS_cache_storage.destroy ~__context ~self
