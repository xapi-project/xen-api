module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

let seconds_per_day = 3600. *. 24.

let days_until_expiry epoch expiry =
  let days = (expiry /. seconds_per_day) -. (epoch /. seconds_per_day) in
  match Float.sign_bit days with
  | true ->
      -1
  | false ->
      Float.(to_int (ceil days))

let execute rpc session_id existing_messages get_cls_and_uuid (obj, alert) =
  (* CA-342551: messages need to be deleted if the pending alert regard the
     same host and has newer, updated information.
     If the pending alert has the same metadata as the existing host message
     it must not be emmited and the existing message must be retained, this
     prevents changing the message UUID.
     In the case there are alerts regarding the host but no alert is pending
     they are not destroyed since no alert is automatically dismissed. *)
  match alert with
  | Some (body, (alert, priority)) -> (
    try
      let cls, obj_uuid = get_cls_and_uuid ~rpc ~session_id obj in
      let messages_in_host =
        List.filter
          (fun (_, record) -> record.API.message_obj_uuid = obj_uuid)
          existing_messages
      in
      let is_outdated (_ref, record) =
        record.API.message_body <> body
        || record.API.message_name <> alert
        || record.API.message_priority <> priority
      in
      let outdated, current = List.partition is_outdated messages_in_host in

      List.iter
        (fun (self, _) -> XenAPI.Message.destroy ~rpc ~session_id ~self)
        outdated ;
      if current = [] then
        let (_ : [> `message] API.Ref.t) =
          XenAPI.Message.create ~rpc ~session_id ~name:alert ~priority ~cls
            ~obj_uuid ~body
        in
        ()
    with Api_errors.(Server_error (_handle_invalid, _)) ->
      (* this happens when the host reference is invalid *)
      ()
  )
  | None ->
      ()

let generate_alert get_expiry get_attrs_body_and_name epoch attributes =
  let expiry = get_expiry attributes in
  let days = days_until_expiry epoch (Date.to_float expiry) in
  let body msg =
    Printf.sprintf "<body><message>%s</message><date>%s</date></body>" msg
      (Date.to_string expiry)
  in
  get_attrs_body_and_name body days attributes

module type AlertSource = sig
  type t

  val get_expiry : t -> Date.t

  val get_attrs_body_and_name :
    (string -> string) -> int -> t -> t * (string * (string * int64)) option

  val filter_possible_messages : [`message] Ref.t * API.message_t -> bool

  val get_item : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> t list

  val get_cls_and_uuid :
       rpc:(Rpc.call -> Rpc.response)
    -> session_id:[< `session] Ref.t
    -> t
    -> [> `Certificate
       | `Host
       | `PVS_proxy
       | `Pool
       | `SR
       | `VDI
       | `VM
       | `VMPP
       | `VMSS ]
       * string
end

module type Alerter = sig
  val alert : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> unit
end

module Make (Source : AlertSource) : Alerter = struct
  let generate_alert =
    generate_alert Source.get_expiry Source.get_attrs_body_and_name

  let alert rpc session_id =
    let now = Unix.time () in
    let previous_messages =
      XenAPI.Message.get_all_records ~rpc ~session_id
      |> List.filter Source.filter_possible_messages
    in
    let send_alert_maybe attributes =
      generate_alert now attributes
      |> execute rpc session_id previous_messages Source.get_cls_and_uuid
    in
    Source.get_item rpc session_id |> List.iter send_alert_maybe
end
