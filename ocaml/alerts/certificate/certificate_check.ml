type cert =
  | CA of API.ref_Certificate * API.datetime
  | Host of API.ref_host * API.datetime
  | Internal of API.ref_host * API.datetime

module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

let get_certificate_attributes rpc session_id =
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

let get_expiry = function
  | Host (_, exp) | Internal (_, exp) | CA (_, exp) ->
      exp

let get_attrs_body_and_name body days cert =
  let expiring = expiring_message cert in
  let expired = expired_message cert in
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

let generate_alert = Lib.generate_alert get_expiry get_attrs_body_and_name

module AlertSource = struct
  type t = cert

  let get_expiry = get_expiry

  let get_attrs_body_and_name = get_attrs_body_and_name

  let filter_possible_messages (_ref, record) =
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

  let get_item = get_certificate_attributes

  let get_cls_and_uuid ~rpc ~session_id cert =
    match cert with
    | Host (host, _) | Internal (host, _) ->
        (`Host, XenAPI.Host.get_uuid ~rpc ~session_id ~self:host)
    | CA (cert, _) ->
        (`Certificate, XenAPI.Certificate.get_uuid ~rpc ~session_id ~self:cert)
end

include Lib.Make (AlertSource)
