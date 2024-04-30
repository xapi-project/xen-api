(*
 * Copyright (c) 2024 Cloud Software Group
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

module D = Debug.Make (struct let name = "xapi_vm_group_helpers" end)

open D

(* Check the breach state of a group.
   When there are no VMs or only one VM in the group, it is not considered a
   breach.
   when there are two or more VMs and all of them are on the same host, it is
   considered a breach, and the specific host is returned.
*)
let check_breach_on_vm_anti_affinity_rules ~__context ~group =
  Db.VM_group.get_VMs ~__context ~self:group
  |> List.filter_map (fun vm ->
         let vm_rec = Db.VM.get_record ~__context ~self:vm in
         match (vm_rec.API.vM_power_state, vm_rec.API.vM_resident_on) with
         | `Running, h when h <> Ref.null ->
             Some h
         | _ ->
             None
     )
  |> function
  | [] | [_] ->
      None
  | h :: remaining ->
      if List.exists (fun h' -> h' <> h) remaining then
        None
      else
        Some h

let report_anti_affinity_alert ~__context ~group ~host =
  let group_uuid = Db.VM_group.get_uuid ~__context ~self:group in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let body =
    String.concat ""
      [
        "<body><message>Breach on VM anti-affinity rules</message><VM_group>"
      ; group_uuid
      ; "</VM_group><host>"
      ; host_uuid
      ; "</host></body>"
      ]
  in
  let obj_uuid =
    Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context)
  in
  Xapi_alert.add
    ~msg:Api_messages.all_running_vms_in_anti_affinity_grp_on_single_host
    ~cls:`Pool ~obj_uuid ~body

let get_anti_affinity_alerts ~__context =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Message.get_all_records ~rpc ~session_id
  )
  |> List.filter (fun (_, record) ->
         record.API.message_name
         = fst Api_messages.all_running_vms_in_anti_affinity_grp_on_single_host
     )

let alert_matched ~__context ~label_name ~id alert =
  let alert_rec = snd alert in
  match Xml.parse_string alert_rec.API.message_body with
  | Xml.Element ("body", _, children) -> (
      let filtered =
        List.filter_map
          (function
            | Xml.Element (name, _, [Xml.PCData v]) when name = label_name ->
                Some v
            | _ ->
                None
            )
          children
      in
      match filtered with [uuid] when uuid = id -> true | _ -> false
    )
  | _ ->
      let msg = "Invalid message body of VM group alert" in
      error "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))
  | exception e ->
      let msg = Printf.sprintf "%s" (ExnHelper.string_of_exn e) in
      error "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))

let filter_alerts_with_group ~__context ~group ~alerts =
  let group_uuid = Db.VM_group.get_uuid ~__context ~self:group in
  List.filter
    (alert_matched ~__context ~label_name:"VM_group" ~id:group_uuid)
    alerts

let filter_alerts_with_host ~__context ~host ~alerts =
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  List.filter (alert_matched ~__context ~label_name:"host" ~id:host_uuid) alerts

(* If it is a breach and no alerts exist, generate one,
   If it is not a breach and alerts exist, dismiss the existing alert *)
let update_vm_anti_affinity_alert_for_group ~__context ~group ~alerts =
  let breach_on_host =
    check_breach_on_vm_anti_affinity_rules ~__context ~group
  in
  debug "[Anti-affinity] existing alerts of group (UUID: %s) is: %d"
    (Db.VM_group.get_uuid ~__context ~self:group)
    (List.length alerts) ;
  match (alerts, breach_on_host) with
  | [], Some host ->
      report_anti_affinity_alert ~__context ~group ~host
  | alerts, None ->
      List.iter
        (fun (ref, _) ->
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Client.Message.destroy ~rpc ~session_id ~self:ref
          )
        )
        alerts
  | alerts, Some host when filter_alerts_with_host ~__context ~host ~alerts = []
    ->
      List.iter
        (fun (ref, _) ->
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Client.Message.destroy ~rpc ~session_id ~self:ref
          )
        )
        alerts ;
      report_anti_affinity_alert ~__context ~group ~host
  | _, _ ->
      ()

let maybe_update_vm_anti_affinity_alert_for_vm ~__context ~vm =
  if Pool_features.is_enabled ~__context Features.VM_group then
    try
      Db.VM.get_groups ~__context ~self:vm
      |> List.filter (fun g ->
             Db.VM_group.get_placement ~__context ~self:g = `anti_affinity
         )
      |> function
      | [] ->
          ()
      | group :: _ ->
          let alerts = get_anti_affinity_alerts ~__context in
          let alerts_of_group =
            filter_alerts_with_group ~__context ~group ~alerts
          in
          update_vm_anti_affinity_alert_for_group ~__context ~group
            ~alerts:alerts_of_group
    with e -> error "%s" (Printexc.to_string e)
  else
    debug "VM group feature is disabled, alert will not be updated"

let remove_vm_anti_affinity_alert_for_group ~__context ~group ~alerts =
  debug "[Anti-affinity] remove alert for group:%s"
    (Db.VM_group.get_uuid ~__context ~self:group) ;
  List.iter
    (fun (ref, _) ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.Message.destroy ~rpc ~session_id ~self:ref
      )
    )
    alerts

let update_alert ~__context ~groups ~action =
  try
    let alerts = get_anti_affinity_alerts ~__context in
    groups
    |> List.filter (fun g ->
           Db.VM_group.get_placement ~__context ~self:g = `anti_affinity
       )
    |> List.iter (fun group ->
           let alerts_of_group =
             filter_alerts_with_group ~__context ~group ~alerts
           in
           action ~__context ~group ~alerts:alerts_of_group
       )
  with e -> error "%s" (Printexc.to_string e)

let update_vm_anti_affinity_alert ~__context ~groups =
  if Pool_features.is_enabled ~__context Features.VM_group then
    update_alert ~__context ~groups
      ~action:update_vm_anti_affinity_alert_for_group
  else
    debug "VM group feature is disabled, alert will not be updated"

let remove_vm_anti_affinity_alert ~__context ~groups =
  update_alert ~__context ~groups
    ~action:remove_vm_anti_affinity_alert_for_group

let maybe_update_alerts_on_feature_change ~__context ~old_restrictions
    ~new_restrictions =
  try
    let is_enabled restrictions =
      List.mem Features.VM_group (Features.of_assoc_list restrictions)
    in
    let groups = Db.VM_group.get_all ~__context in
    match (is_enabled old_restrictions, is_enabled new_restrictions) with
    | false, true ->
        update_vm_anti_affinity_alert ~__context ~groups
    | true, false ->
        remove_vm_anti_affinity_alert ~__context ~groups
    | _, _ ->
        ()
  with e -> error "%s" (Printexc.to_string e)
