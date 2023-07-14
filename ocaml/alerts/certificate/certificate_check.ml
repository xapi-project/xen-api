module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

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
      "The TLS server certificate"
  | Internal _ ->
      "The internal TLS server certificate"
  | CA _ ->
      "The CA pool certificate"

let alert_conditions = function
  | Host _ ->
      [
        (0, Api_messages.host_server_certificate_expired)
      ; (7, Api_messages.host_server_certificate_expiring_07)
      ; (14, Api_messages.host_server_certificate_expiring_14)
      ; (30, Api_messages.host_server_certificate_expiring_30)
      ]
  | Internal _ ->
      [
        (0, Api_messages.host_internal_certificate_expired)
      ; (7, Api_messages.host_internal_certificate_expiring_07)
      ; (14, Api_messages.host_internal_certificate_expiring_14)
      ; (30, Api_messages.host_internal_certificate_expiring_30)
      ]
  | CA _ ->
      [
        (0, Api_messages.pool_ca_certificate_expired)
      ; (7, Api_messages.pool_ca_certificate_expiring_07)
      ; (14, Api_messages.pool_ca_certificate_expiring_14)
      ; (30, Api_messages.pool_ca_certificate_expiring_30)
      ]

let alert_message_cls_and_obj_uuid rpc session_id cert =
  match cert with
  | Host (host, _) | Internal (host, _) ->
      (`Host, XenAPI.Host.get_uuid ~rpc ~session_id ~self:host)
  | CA (cert, _) ->
      (`Certificate, XenAPI.Certificate.get_uuid ~rpc ~session_id ~self:cert)

let get_expiry = function
  | Host (_, exp) | Internal (_, exp) | CA (_, exp) ->
      exp

let alert rpc session_id =
  get_certificates rpc session_id
  |> List.map (fun cert ->
         let cls, obj_uuid =
           alert_message_cls_and_obj_uuid rpc session_id cert
         in
         let obj_description = certificate_description cert in
         let alert_conditions = alert_conditions cert in
         let expiry = get_expiry cert in
         Expiry_alert.{cls; obj_uuid; obj_description; alert_conditions; expiry}
     )
  |> Expiry_alert.alert ~rpc ~session_id

let maybe_generate_alert = Expiry_alert.maybe_generate_alert
