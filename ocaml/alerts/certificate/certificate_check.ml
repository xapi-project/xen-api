module XenAPI = Client.Client

type cert =
  | CA of API.ref_Certificate * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

let get_certificates rpc session_id =
  XenAPI.Certificate.get_all_records ~rpc ~session_id
  |> List.map @@ fun (cert_ref, certificate) ->
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
         CA (cert_ref, certificate.API.certificate_not_after)

let certificate_description = function
  | Host _ ->
      "TLS server certificate"
  | Internal _ ->
      "internal TLS server certificate"
  | CA _ ->
      "CA pool certificate"

let expired_message_id = function
  | Host _ ->
      Api_messages.host_server_certificate_expired
  | Internal _ ->
      Api_messages.host_internal_certificate_expired
  | CA _ ->
      Api_messages.pool_ca_certificate_expired

let expiring_conditions = function
  | Host _ ->
      [
        (7, Api_messages.host_server_certificate_expiring_07)
      ; (14, Api_messages.host_server_certificate_expiring_14)
      ; (30, Api_messages.host_server_certificate_expiring_30)
      ]
  | Internal _ ->
      [
        (7, Api_messages.host_internal_certificate_expiring_07)
      ; (14, Api_messages.host_internal_certificate_expiring_14)
      ; (30, Api_messages.host_internal_certificate_expiring_30)
      ]
  | CA _ ->
      [
        (7, Api_messages.pool_ca_certificate_expiring_07)
      ; (14, Api_messages.pool_ca_certificate_expiring_14)
      ; (30, Api_messages.pool_ca_certificate_expiring_30)
      ]

let alert_message_cls = function
  | Host _ ->
      `Host
  | Internal _ ->
      `Host
  | CA _ ->
      `Certificate

let alert_message_obj_uuid rpc session_id cert =
  match cert with
  | Host (host, _) | Internal (host, _) ->
      XenAPI.Host.get_uuid ~rpc ~session_id ~self:host
  | CA (cert, _) ->
      XenAPI.Certificate.get_uuid ~rpc ~session_id ~self:cert

let alert_for_certificate rpc session_id cert =
  let alert_obj_description = certificate_description cert in
  let expired_message_id = expired_message_id cert in
  let expiring_conditions = expiring_conditions cert in
  let expiry =
    match cert with Host (_, exp) | Internal (_, exp) | CA (_, exp) -> exp
  in
  let msg_cls = alert_message_cls cert in
  let msg_obj_uuid = alert_message_obj_uuid rpc session_id cert in
  Expiry_alert.update ~rpc ~session_id ~alert_obj_description
    ~expired_message_id ~expiring_conditions ~expiry ~msg_cls ~msg_obj_uuid

let alert rpc session_id =
  get_certificates rpc session_id
  |> List.iter (alert_for_certificate rpc session_id)
