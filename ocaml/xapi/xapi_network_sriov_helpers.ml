(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

open Network
open Db_filter_types
module D = Debug.Make(struct let name="xapi" end)
open D

let is_device_underneath_same_type ~__context pif1 pif2 =
  let get_device_info pif =
    let pci = Db.PIF.get_PCI ~__context ~self:pif in
    Db.PCI.get_vendor_id ~__context ~self:pci, Db.PCI.get_device_id ~__context ~self:pci
  in
  (get_device_info pif1) = (get_device_info pif2)

(* SRIOV PIF can only join the network which is empty or all of the existing PIFs of it are SRIOV PIFS *)
let assert_sriov_pif_compatible_with_network ~__context ~pif ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | logical_pif :: _ ->
    begin
      match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_pif with
      | [] -> raise (Api_errors.(Server_error (network_is_not_sriov_compatible, [Ref.string_of network])))
      | sriov :: _ ->
        let existing_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
        if not (is_device_underneath_same_type ~__context pif existing_pif) then
          raise (Api_errors.(Server_error (network_has_incompatible_sriov_pifs, [Ref.string_of pif; Ref.string_of network])))
    end
