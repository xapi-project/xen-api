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

module D = Debug.Make (struct let name = __MODULE__ end)

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module Uds = struct
  (* {{{1 *)

  exception Parse_error of string

  let with_channel_aux fd func =
    let ic, oc = (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd) in
    finally (fun () -> func ic oc) (fun () -> Unix.close fd)

  let with_socket address func =
    let fd = Xapi_stdext_unix.Unixext.open_connection_fd address 80 in
    with_channel_aux fd func

  let with_unix_channels filename func =
    let fd = Xapi_stdext_unix.Unixext.open_connection_unix_fd filename in
    with_channel_aux fd func

  let http_response_code d =
    match String.split_on_char ' ' d with
    | _ :: code :: _ ->
        int_of_string code
    | _ ->
        raise (Parse_error "Failed to parse HTTP reponse code")

  let rec read_header ic acc =
    let line = input_line ic in
    if line = "\r" then
      List.rev (line :: acc)
    else
      read_header ic (line :: acc)

  let rec read_body ic acc =
    try
      let line = input_line ic in
      read_body ic (line :: acc)
    with End_of_file -> List.rev acc

  let http_command ?(file_socket = true) filename cmd =
    let with_channel =
      if file_socket then with_unix_channels else with_socket
    in
    with_channel filename (fun ic oc ->
        Printf.fprintf oc "%s" cmd ;
        flush oc ;
        let result_line = input_line ic in
        let response_code = http_response_code result_line in
        let header = read_header ic [] in
        let body = read_body ic [] in
        (response_code, result_line, header, body)
    )
end

module Cookies = struct
  (* {{{1 *)
  (* Cookies used to be accepted when delimited by '&'. Now they're also
     accepted in the RFC-compliant way with ';' as delimiters. This test
     verifies that both styles are parsed correctly *)

  let get_config_files_cmd =
    Printf.sprintf "GET /sync_config_files/2 HTTP/1.0\r\nCookie: %s\r\n\r\n"

  let req_with_cookies secret ~sep =
    get_config_files_cmd
      (Printf.sprintf "k1=v1%spool_secret=%s%sk3=v3" sep secret sep)

  let read_pool_secret () =
    try
      Unix.access Rrdd_libs.Constants.pool_secret_path [Unix.F_OK] ;
      Xapi_stdext_unix.Unixext.string_of_file
        Rrdd_libs.Constants.pool_secret_path
    with _ -> failwith "Unable to read the pool secret."

  let send_http_aux ~cmd f =
    let response_code, result_line, header, body =
      Uds.http_command "localhost" cmd ~file_socket:false
    in
    f (response_code, result_line, header, body)

  let check_response_body_contains ~expected ~cmd =
    let check_body (_, _, _, body) =
      match body with
      | first_line :: _ ->
          D.warn "expected = [%s]; received = [%s]" expected first_line ;
          Astring.String.is_infix ~affix:first_line expected
      | _ ->
          false
    in
    send_http_aux ~cmd check_body

  let check_response_code ~expected ~cmd =
    let check_code (code, _, _, _) =
      D.warn "expected = [%d]; received = [%d]" expected code ;
      expected = code
    in
    send_http_aux ~cmd check_code

  let test_cookies_old_style_valid () =
    Alcotest.(check bool)
      "Cookies should be parsed properly to extract valid pool secret" true
      (check_response_body_contains ~expected:"password"
         ~cmd:(req_with_cookies (read_pool_secret ()) ~sep:"&")
      )

  let test_cookies_old_style_invalid () =
    Alcotest.(check bool)
      "Invalid pool_secret in cookies should be rejected" true
      (check_response_code ~expected:401 (* Unauthorised *)
         ~cmd:(req_with_cookies "whatever" ~sep:"&")
      )

  let test_cookies_new_style_valid () =
    Alcotest.(check bool)
      "Cookies should be parsed properly to extract valid pool secret" true
      (check_response_body_contains ~expected:"password"
         ~cmd:(req_with_cookies (read_pool_secret ()) ~sep:";")
      )

  let test_cookies_new_style_invalid () =
    Alcotest.(check bool)
      "Invalid pool_secret in cookies should be rejected" true
      (check_response_code ~expected:401 (* Unauthorised *)
         ~cmd:(req_with_cookies "whatever" ~sep:";")
      )

  let tests =
    [
      ("test_cookies_old_style_valid", `Quick, test_cookies_old_style_valid)
    ; ("test_cookies_old_style_invalid", `Quick, test_cookies_old_style_invalid)
    ; ("test_cookies_new_style_valid", `Quick, test_cookies_new_style_valid)
    ; ("test_cookies_new_style_invalid", `Quick, test_cookies_new_style_invalid)
    ]
end

module Secret_Auth_fails = struct
  (* {{{1 *)

  let invalid_pool_secret =
    Http.Request.make ~version:"1.0" ~user_agent:"quicktest" Http.Get
      "/sync_config_files/2"
    |> Helpers.with_cookie (SecretString.of_string "whatever")

  let invalid_basicauth =
    Http.Request.make ~version:"1.0" ~user_agent:"quicktest"
      ~headers:[("Authorization", "Basic cm9vdDpiYXI=")] (* root:bar *)
      Http.Get "/rss"

  (** Tests that invalid pool secrets are rejected. *)
  let test_auth_fails_invalid_pool_secret () =
    Qt.Test.assert_raises_match
      (function Http_client.Http_error _ -> true | _ -> false)
      (fun () -> Qt.http invalid_pool_secret (fun _ -> ()))

  (** Tests that invalid basic authentication fails. *)
  let test_auth_fails_invalid_basicauth () =
    Qt.Test.assert_raises_match
      (function
        | Http_client.Http_error _ ->
            true
        | Http_client.Http_request_rejected _ ->
            true
        | _ ->
            false
        )
      (fun () -> Qt.http invalid_basicauth (fun _ -> ()))

  let tests =
    [
      ( "test_auth_failes_invalid_pool_secret"
      , `Quick
      , test_auth_fails_invalid_pool_secret
      )
    ; ( "test_auth_failes_invalid_basicauth"
      , `Quick
      , test_auth_fails_invalid_basicauth
      )
    ]
end

module HTML_Escaping = struct
  (* {{{1 *)

  let non_resource_cmd = "GET /foo<>'\"& HTTP/1.0\r\n\r\n"

  let non_resource_exp = "&lt;&gt;&apos;&quot;&amp;"

  let bad_resource_cmd = "GET /%foo<>'\"& HTTP/1.0\r\n\r\n"

  let bad_resource_exp = "&lt;&gt;&apos;&quot;&amp;"

  let bad_command_cmd = "FOO<>'\"& /foo HTTP/1.0\r\n\r\n"

  let bad_command_exp = "&lt;&gt;&apos;\\&quot;&amp;"

  let html_escaping expected cmd =
    let check_result = Astring.String.is_infix ~affix:expected in
    let _, _, _, body = Uds.http_command Xapi_globs.unix_domain_socket cmd in
    match body with
    | first_line :: _ ->
        Printf.printf "expected = [%s]; received = [%s]\n%!" expected first_line ;
        check_result first_line
    | _ ->
        false

  let test_html_escaping_non_resource () =
    Alcotest.(check bool)
      "The data returned when asking for a non-existing resource should be \
       properly escaped."
      true
      (html_escaping non_resource_exp non_resource_cmd)

  let test_html_escaping_bad_resource () =
    Alcotest.(check bool)
      "The data returned when asking for a badly named resource should be \
       properly escaped."
      true
      (html_escaping bad_resource_exp bad_resource_cmd)

  let tests =
    [
      ( "test_html_escaping_non_resource"
      , `Quick
      , test_html_escaping_non_resource
      )
    ; ( "test_html_escaping_bad_resource"
      , `Quick
      , test_html_escaping_bad_resource
      )
    ]
end

(* Test suite and definition of test function {{{1 *)
let tests = Secret_Auth_fails.tests @ Cookies.tests @ HTML_Escaping.tests
