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

let get_certificate_attributes rpc session =
  XenAPI.Certificate.get_all_records rpc session
  |> List.map (fun (_, certificate) ->
         ( certificate.API.certificate_host
         , certificate.API.certificate_not_after ))

let generate_alert epoch (host, expiry) =
  let days = days_until_expiry epoch (Date.to_float expiry) in
  if days > 30 then
    (host, None)
  else
    let expiring_message = "The TLS server certificate is expiring soon." in
    let expired_message = "The TLS server certificate has expired." in
    let body msg =
      Printf.sprintf "<body><message>%s</message><date>%s</date></body>" msg
        (Date.to_string expiry)
    in
    let message, alert =
      if days < 0 then
        (body expired_message, Api_messages.host_server_certificate_expired)
      else if days < 8 then
        (body expiring_message, Api_messages.host_server_certificate_expiring_07)
      else if days < 15 then
        (body expiring_message, Api_messages.host_server_certificate_expiring_14)
      else
        (body expiring_message, Api_messages.host_server_certificate_expiring_30)
    in
    (host, Some (message, alert))

let execute rpc session previous_messages (host, alert) =
  let host_uuid = XenAPI.Host.get_uuid rpc session host in
  let obsolete_messages =
    previous_messages
    |> List.filter_map (fun (ref, record) ->
           if record.API.message_obj_uuid = host_uuid then
             Some ref
           else
             None)
  in
  List.iter
    (fun self -> XenAPI.Message.destroy rpc session self)
    obsolete_messages ;
  match alert with
  | None ->
      ()
  | Some (message, (alert, priority)) ->
      ignore
        (XenAPI.Message.create rpc session alert priority `Host host_uuid
           message)

let alert rpc session =
  let now = Unix.time () in
  (* Message starting with [host_server_certificate_\{expiring,expired\}] may
     need to be refreshed, gather them just once *)
  let previous_messages =
    XenAPI.Message.get_all_records rpc session
    |> List.filter (fun (ref, record) ->
           let expiring_or_expired name =
             let expiring = Api_messages.host_server_certificate_expiring in
             let expired = fst Api_messages.host_server_certificate_expired in
             let open Astring.String in
             is_prefix ~affix:expiring name || is_prefix ~affix:expired name
           in
           expiring_or_expired record.API.message_name)
  in
  let send_alert_maybe attributes =
    attributes |> generate_alert now |> execute rpc session previous_messages
  in
  get_certificate_attributes rpc session |> List.iter send_alert_maybe
