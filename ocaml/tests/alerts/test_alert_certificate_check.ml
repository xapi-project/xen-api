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

open Certificate_check

let date_of = Xapi_stdext_date.Date.of_string

let check_time = Xapi_stdext_date.Date.to_float (date_of "20200201T02:00:00Z")

let good_samples =
  [
    "20210202T02:00:00Z" (* +1 year*)
  ; "20200302T02:00:01Z" (* +30 days, +1 second *)
  ]

let expiring_samples =
  [
    ("20200302T02:00:00Z", 30, Api_messages.host_server_certificate_expiring_30)
  ; ("20200301T02:00:00Z", 29, Api_messages.host_server_certificate_expiring_30)
  ; ("20200215T02:00:00Z", 14, Api_messages.host_server_certificate_expiring_14)
  ; ("20200208T02:00:00Z", 7, Api_messages.host_server_certificate_expiring_07)
  ; ("20200201T02:00:00Z", 0, Api_messages.host_server_certificate_expiring_07)
  ]

let expired_samples = ["20200102T02:00:00Z"; "20200201T01:59:59Z"]

let format_good datestring = (("host", date_of datestring), ("host", None))

let _format (datestring, ppf, alert) =
  let fmt = Scanf.format_from_string ppf "%s" in
  ( ("host", date_of datestring)
  , ("host", Some (Printf.sprintf fmt datestring, alert))
  )

let format_expiring (datestring, days, alert) =
  let ppf =
    "<body><message>The TLS server certificate is expiring \
     soon.</message><date>%s</date></body>"
  in
  _format (datestring, ppf, alert)

let format_expired datestring =
  let ppf =
    "<body><message>The TLS server certificate has \
     expired.</message><date>%s</date></body>"
  in
  _format (datestring, ppf, Api_messages.host_server_certificate_expired)

let certificate_samples =
  List.concat
    [
      List.map format_good good_samples
    ; List.map format_expiring expiring_samples
    ; List.map format_expired expired_samples
    ]

let gen check_time (host, datetime) =
  let cert = Host (Ref.null, datetime) in
  match generate_alert check_time cert with
  | CA _, x ->
      ("pool", x)
  | Host _, x ->
      ("host", x)
  | Internal _, x ->
      ("internal", x)

let test_alerts (server_certificates, expected) () =
  let result = gen check_time server_certificates in
  Alcotest.(check @@ pair string @@ option @@ pair string @@ pair string int64)
    "certificate_expiry" expected result

let test =
  List.mapi
    (fun i spec ->
      ( Printf.sprintf "Test certificate checks as run daily #%d" i
      , `Quick
      , test_alerts spec
      )
    )
    certificate_samples
