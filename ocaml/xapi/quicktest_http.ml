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
open Stdext
open Xstringext
open Threadext
open Pervasiveext
open Client
open Quicktest_common

open Quicktest_ocamltest
open Ocamltest

module Uds = struct (* {{{1 *)

  module D = Debug.Make(struct let name = "quicktest_http:Uds" end)
  open D

  exception Parse_error of string

  let with_unix_channels filename func =
    let fd = Unixext.open_connection_unix_fd filename in
    let ic, oc = (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd) in
    finally (fun () -> func ic oc) (fun () -> Unix.close fd)

  let http_response_code d =
    match Xstringext.String.split ' ' d with
    | _ :: code :: _ -> int_of_string code
    | _ -> raise (Parse_error "Failed to parse HTTP reponse code")

  let rec read_header ic acc =
    let line = input_line ic in
    if line = "\r"
    then List.rev (line :: acc)
    else read_header ic (line :: acc)

  let rec read_body ic acc =
    try
      let line = input_line ic in
      read_body ic (line :: acc)
    with
      End_of_file -> List.rev acc

  let http_command filename cmd = with_unix_channels filename (fun ic oc ->
      Printf.fprintf oc "%s" cmd;
      flush oc;
      let result_line = input_line ic in
      let response_code =  http_response_code result_line in
      let header = read_header ic [] in
      let body = read_body ic [] in
      (response_code, result_line, header, body))

end

module Secret_Auth_fails = struct (* {{{1 *)

  let http request f =
    let open Xmlrpc_client in
    let transport =
      if !using_unix_domain_socket
      then Unix Xapi_globs.unix_domain_socket
      else SSL(SSL.make ~use_fork_exec_helper:false (), !host, 443) in
    with_transport transport (with_http request f)

  let invalid_pool_secret =
    Http.Request.make ~version:"1.0" ~cookie:["pool_secret", "whatever"]
      ~user_agent:"quicktest"
      Http.Get "/sync_config_files"

  let invalid_basicauth =
    Http.Request.make ~version:"1.0"
      ~user_agent:"quicktest"
      ~headers:["Authorization", "Basic cm9vdDpiYXI="] (* root:bar *)
      Http.Get "/rss"

  let test_invalid_pool_secret = make_test_case "invalid_pool_secret"
      "Tests that invalid pool secrets are rejected."
      begin fun () ->
        assert_raises_match
          (function Http_client.Http_error _ -> true | _ -> false)
          (fun () -> http invalid_pool_secret (fun _ -> ()))
      end

  let test_invalid_basicauth = make_test_case "invalid_basicauth"
      "Tests that invalid basic authentication fails."
      begin fun () ->
        assert_raises_match
          (function Http_client.Http_error _ -> true | Http_client.Http_request_rejected _ -> true | _ -> false)
          (fun () -> http invalid_basicauth (fun _ -> ()))
      end

  let tests = make_module_test_suite "Secret_Auth"
      [ test_invalid_pool_secret
      ; test_invalid_basicauth
      ]
end

module HTML_Escaping = struct (* {{{1 *)

  module D = Debug.Make(struct let name = "quicktest_http:HTML_Escaping" end)
  open D

  let non_resource_cmd = "GET /foo<>'\"& HTTP/1.0\r\n\r\n"
  let non_resource_exp = "&lt;&gt;&apos;&quot;&amp;"
  let bad_resource_cmd = "GET /%foo<>'\"& HTTP/1.0\r\n\r\n"
  let bad_resource_exp = "&lt;&gt;&apos;&quot;&amp;"
  let bad_command_cmd = "FOO<>'\"& /foo HTTP/1.0\r\n\r\n"
  let bad_command_exp = "&lt;&gt;&apos;\\&quot;&amp;"

  let html_escaping expected cmd =
    let check_result b = String.has_substr b expected in
    let _, _, _, body = Uds.http_command Xapi_globs.unix_domain_socket cmd in
    Printf.printf "expected = [%s]; received = [%s]\n%!" expected (List.hd body);
    check_result (List.hd body)

  let test_html_escaping_non_resource = make_test_case "html_escaping_non_resouce"
      "Tests that the data returned when asking for a non-existing resource is properly escaped."
      (fun () -> assert_true (html_escaping non_resource_exp non_resource_cmd))

  let test_html_escaping_bad_resource = make_test_case "html_escaping_bad_resouce"
      "Tests that the data returned when asking for a badly named resource is properly escaped."
      (fun () -> assert_true (html_escaping bad_resource_exp bad_resource_cmd))

  let tests = make_module_test_suite "HTML_Escaping"
      [ test_html_escaping_non_resource
      ; test_html_escaping_bad_resource
      ]
end

(* Test suite and definition of test function {{{1 *)
let tests = make_module_test_suite "Rejects"
    [ Secret_Auth_fails.tests
    ; HTML_Escaping.tests
    ]

let run_from_within_quicktest () = run_from_within_quicktest tests
