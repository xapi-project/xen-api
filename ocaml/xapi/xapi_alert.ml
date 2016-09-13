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
module D = Debug.Make(struct let name="xapi_alert" end)
open D

open Client

module Alert = struct
  type t = { name: string;
             priority: int64;
             cls: API.cls;
             obj_uuid: string;
             body: string }
  let process (x: t) =
    Server_helpers.exec_with_new_task "Sending an HA alert" ~task_in_database:false
      (fun __context ->
         Helpers.call_api_functions ~__context
           (fun rpc session_id ->
              try
                let (_: 'a Ref.t) = Client.Message.create rpc session_id x.name x.priority x.cls x.obj_uuid x.body in ()
              with e ->
                warn "Exception creating message: %s" (ExnHelper.string_of_exn e)
           )
      )
end

(** Function which pushes Alerts onto the queue for background processing *)
let alert_queue_push = (Thread_queue.make ~name:"API messages" ~max_q_length:100 Alert.process).Thread_queue.push_fn

(** Function which guarantees not to block and creates the message on a 'best-effort' basis *)
let add ~msg:(name, priority) ~cls ~obj_uuid ~body =
  let sent =
    if Pool_role.is_master () then begin
      Server_helpers.exec_with_new_task "Sending an alert" ~task_in_database:false
        (fun __context ->
           let (_: 'a Ref.t) = Xapi_message.create ~__context ~name ~priority ~cls ~obj_uuid ~body in true
        )
    end else alert_queue_push name { Alert.name = name; priority = priority; cls = cls; obj_uuid = obj_uuid; body = body } in
  if not sent then warn "Failed to send alert %s %s" name obj_uuid



(** Repeated calls to this function call 'on_edge_fn' on every value transition *)
let edge_trigger on_edge_fn =
  let old_value = ref None in
  fun x ->
    begin
      match !old_value with
      | None -> ()
      | Some ov -> if ov <> x then on_edge_fn ov x
    end;
    old_value := Some x

(*
let int_trigger = edge_trigger (fun _ _ -> add ~name:"foo"  ~priority:1L ~cls:`Pool ~obj_uuid:"" ~body:"")
let string_trigger = edge_trigger (fun _ _ -> add ~name:"foo" ~priority:1L ~cls:`Pool ~obj_uuid:"" ~body:"")

let _ =
  int_trigger 0;
  int_trigger 1;
  string_trigger "";
  string_trigger ""
*)
