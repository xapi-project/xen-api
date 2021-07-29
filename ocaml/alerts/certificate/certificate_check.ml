module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

type cert =
  | CA of API.ref_pool * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

let seconds_per_day = 3600. *. 24.

let internal_error fmt =
  fmt
  |> Printf.kprintf @@ fun msg ->
     raise Api_errors.(Server_error (internal_error, [msg]))

let days_until_expiry epoch expiry =
  let days = (expiry /. seconds_per_day) -. (epoch /. seconds_per_day) in
  match Float.sign_bit days with
  | true ->
      -1
  | false ->
      Float.(to_int (ceil days))

let get_certificate_attributes rpc session =
  XenAPI.Certificate.get_all_records rpc session
  |> List.map @@ fun (_, certificate) ->
         match certificate.API.certificate_type with
         | `host ->
         Host
               ( certificate.API.certificate_host
           , certificate.API.certificate_not_after
           )
     | `host_internal ->
         Internal
           ( certificate.API.certificate_host
           , certificate.API.certificate_not_after
           )
     | `ca ->
         let pool =
           match XenAPI.Pool.get_all rpc session with
           | p :: _ ->
               p
           | [] ->
               internal_error "can't indentify pool at %s" __LOC__
         in
         CA (pool, certificate.API.certificate_not_after)

let expired_message = function
  | Host _ ->
      "The TLS server certificate has expired."
  | Internal _ ->
      "The internal TLS server certificate has expired."
  | CA _ ->
      "The CA pool certificate has expired"

let expiring_message = function
  | Host _ ->
      "The TLS server certificate is expiring soon."
  | Internal _ ->
      "The internal TLS server certificate is expiring soon."
  | CA _ ->
      "The CA pool certificate is expiring soon"

let generate_alert epoch cert =
  let expiry =
    match cert with Host (_, exp) | Internal (_, exp) | CA (_, exp) -> exp
  in
  let days = days_until_expiry epoch (Date.to_float expiry) in
  let expiring = expiring_message cert in
  let expired = expired_message cert in
    let body msg =
      Printf.sprintf "<body><message>%s</message><date>%s</date></body>" msg
        (Date.to_string expiry)
    in
  match (days, cert) with
  | days, _ when days > 30 ->
      (cert, None)
  | days, Host _ when days < 0 ->
      (cert, Some (body expired, Api_messages.host_server_certificate_expired))
  | days, Host _ when days < 8 ->
      ( cert
      , Some (body expiring, Api_messages.host_server_certificate_expiring_07)
      )
  | days, Host _ when days < 15 ->
      ( cert
      , Some (body expiring, Api_messages.host_server_certificate_expiring_14)
      )
  | _, Host _ ->
      ( cert
      , Some (body expiring, Api_messages.host_server_certificate_expiring_30)
      )
  | days, CA _ when days < 0 ->
      (cert, Some (body expired, Api_messages.pool_ca_certificate_expired))
  | days, CA _ when days < 8 ->
      (cert, Some (body expiring, Api_messages.pool_ca_certificate_expiring_07))
  | days, CA _ when days < 15 ->
      (cert, Some (body expiring, Api_messages.pool_ca_certificate_expiring_14))
  | _, CA _ ->
      (cert, Some (body expiring, Api_messages.pool_ca_certificate_expiring_30))
  | days, Internal _ when days < 0 ->
      (cert, Some (body expired, Api_messages.host_internal_certificate_expired))
  | days, Internal _ when days < 8 ->
      ( cert
      , Some (body expiring, Api_messages.host_internal_certificate_expiring_07)
      )
  | days, Internal _ when days < 15 ->
      ( cert
      , Some (body expiring, Api_messages.host_internal_certificate_expiring_14)
      )
  | _, Internal _ ->
      ( cert
      , Some (body expiring, Api_messages.host_internal_certificate_expiring_30)
      )

let execute rpc session existing_messages (cert, alert) =
  (* CA-342551: messages need to be deleted if the pending alert regard the
     same host and has newer, updated information.
     If the pending alert has the same metadata as the existing host message
     it must not be emmited and the existing message must be retained, this
     prevents changing the message UUID.
     In the case there are alerts regarding the host but no alert is pending
     they are not destroyed since no alert is automatically dismissed. *)
  match alert with
  | Some (message, (alert, priority)) -> (
    try
      let cls, uuid =
        match cert with
        | Host (host, _) | Internal (host, _) ->
            (`Host, XenAPI.Host.get_uuid rpc session host)
        | CA (pool, _) ->
            (`Pool, XenAPI.Pool.get_uuid rpc session pool)
      in
      let messages_in_host =
        List.filter
          (fun (_, record) -> record.API.message_obj_uuid = uuid)
          existing_messages
      in
      let is_outdated (ref, record) =
        record.API.message_body <> message
        || record.API.message_name <> alert
        || record.API.message_priority <> priority
      in
      let outdated, current = List.partition is_outdated messages_in_host in
      List.iter
        (fun (self, _) -> XenAPI.Message.destroy rpc session self)
        outdated ;
      if current = [] then
        let (_ : [> `message] API.Ref.t) =
          XenAPI.Message.create rpc session alert priority cls uuid message
        in
        ()
    with Api_errors.(Server_error (handle_invalid, _)) ->
      (* this happens when the host reference is invalid *)
      ()
  )
  | None ->
      ()

let alert rpc session =
  let now = Unix.time () in
  (* Message starting with
     [\{host_server_certificate,pool_ca_certificate\}_\{expiring,expired\}]
     may need to be refreshed, gather them just once *)
  let previous_messages =
    XenAPI.Message.get_all_records rpc session
    |> List.filter (fun (ref, record) ->
           let expiring_or_expired name =
             let matching affix = Astring.String.is_prefix ~affix name in
             matching Api_messages.host_server_certificate_expiring
             || matching (fst Api_messages.host_server_certificate_expired)
             || matching Api_messages.pool_ca_certificate_expiring
             || matching (fst Api_messages.pool_ca_certificate_expired)
             || matching Api_messages.host_internal_certificate_expiring
             || matching (fst Api_messages.host_internal_certificate_expired)
           in
           expiring_or_expired record.API.message_name
       )
  in
  let send_alert_maybe attributes =
    attributes |> generate_alert now |> execute rpc session previous_messages
  in
  get_certificate_attributes rpc session |> List.iter send_alert_maybe
