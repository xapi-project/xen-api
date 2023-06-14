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

module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

let seconds_per_day = 3600. *. 24.

let days_until_expiry epoch expiry =
  (expiry /. seconds_per_day) -. (epoch /. seconds_per_day)

let all_messages rpc session_id =
  XenAPI.Message.get_all_records ~rpc ~session_id

let related_messages msg_obj_uuid related_message_name_list all_msgs =
  List.filter
    (fun (_ref, record) ->
      let related name = List.mem name related_message_name_list in
      record.API.message_obj_uuid = msg_obj_uuid
      && related record.API.message_name
    )
    all_msgs

let message_body msg expiry =
  Printf.sprintf "<body><message>%s</message><date>%s</date></body>" msg
    (Date.to_string expiry)

let expired_message obj = Printf.sprintf "The %s has expired." obj

let expiring_message obj = Printf.sprintf "The %s is expiring soon." obj

let generate_alert now expiry expired_message_id expiring_conditions
    alert_obj_description =
  let remaining_days =
    days_until_expiry (Date.to_float now) (Date.to_float expiry)
  in
  let sorted_alert_conditions =
    List.sort
      (fun (remaining_days_a, _) (remaining_days_b, _) ->
        remaining_days_a - remaining_days_b
      )
      ((0, expired_message_id) :: expiring_conditions)
  in
  let matched_condition =
    List.find_opt
      (fun (days, _) -> remaining_days <= float_of_int days)
      sorted_alert_conditions
  in
  match matched_condition with
  | None ->
      None
  | Some (days, expired_message_id) when days = 0 ->
      let expired = expired_message alert_obj_description in
      Some (message_body expired expiry, expired_message_id)
  | Some (_, expiring_message_id) ->
      let expiring = expiring_message alert_obj_description in
      Some (message_body expiring expiry, expiring_message_id)

let messages_require_update related_msgs alert =
  let msg_body, (msg_name, msg_prio) = alert in
  let is_outdated (_ref, record) =
    record.API.message_body <> msg_body
    || record.API.message_name <> msg_name
    || record.API.message_priority <> msg_prio
  in
  let outdated, current = List.partition is_outdated related_msgs in
  (outdated, current = [])

let update_message_internal expired_message_id expiring_conditions msg_obj_uuid
    alert all_msgs =
  let expiring_msg_name_list =
    List.map (fun (_, (msg_name, _)) -> msg_name) expiring_conditions
  in
  let expired_expiring_msg_name_list =
    fst expired_message_id :: expiring_msg_name_list
  in
  let related_msgs =
    related_messages msg_obj_uuid expired_expiring_msg_name_list all_msgs
  in
  let outdated_msgs, create_new_msg =
    messages_require_update related_msgs alert
  in
  (outdated_msgs, create_new_msg)

let update_message rpc session_id expired_message_id expiring_conditions msg_cls
    msg_obj_uuid alert all_msgs =
  let outdated_msgs, create_new_msg =
    update_message_internal expired_message_id expiring_conditions msg_obj_uuid
      alert all_msgs
  in
  List.iter
    (fun (self, _) -> XenAPI.Message.destroy ~rpc ~session_id ~self)
    outdated_msgs ;
  if create_new_msg then
    let body, (name, priority) = alert in
    let (_ : [> `message] API.Ref.t) =
      XenAPI.Message.create ~rpc ~session_id ~name ~priority ~cls:msg_cls
        ~obj_uuid:msg_obj_uuid ~body
    in
    ()

let update ~rpc ~session_id ~alert_obj_description ~expired_message_id
    ~expiring_conditions ~expiry ~msg_cls ~msg_obj_uuid =
  let now = Date.of_float (Unix.time ()) in
  let alert =
    generate_alert now expiry expired_message_id expiring_conditions
      alert_obj_description
  in
  match alert with
  | Some alert ->
      all_messages rpc session_id
      |> update_message rpc session_id expired_message_id expiring_conditions
           msg_cls msg_obj_uuid alert
  | None ->
      ()
