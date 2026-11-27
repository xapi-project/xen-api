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

module Accept = struct
  module Accept = Http.Accept

  let accept =
    Alcotest.testable (Fmt.of_to_string Accept.to_string) Accept.equal

  let test_accept_simple () =
    let data = "application/json" in
    let expected = [Accept.{ty= Some ("application", Some "json"); q= 1000}] in
    let actual = Accept.of_string data in
    Alcotest.(check @@ list accept) data expected actual

  let test_invalid () =
    let data = "text/html, image/gif, image/jpeg, ; q=.2, */; q=.2" in
    let expected = Accept.Parse_failure " " in
    let actual () =
      let _ = Accept.of_string data in
      ()
    in
    Alcotest.check_raises "Raises Parse failure" expected actual

  let test_accept_complex () =
    let data =
      "application/xml;q=0.9,text/html,application/xhtml+xml,*/*;q=0.8"
    in
    let expected = ["text/html"] in
    let content_types = Accept.of_string data in
    let actual = Accept.preferred ~from:["text/html"] content_types in
    Alcotest.(check @@ list string) data expected actual ;

    let expected = ["foo/bar"] in
    let actual = Accept.preferred ~from:["foo/bar"] content_types in
    Alcotest.(check @@ list string) data expected actual

  let preferred_tests =
    let data =
      [
        ("Empty ~from", "application/json,text/html", [], [])
      ; ("Empty list", "", ["text/xml"], [])
      ; ("No matches", "application/json,text/html", ["text/xml"], [])
      ; ( "Matches a single element"
        , "application/json,text/*"
        , ["text/xml"]
        , ["text/xml"]
        )
      ; ( "Detects correctly a low-priority matching rule"
        , "text/xml,*/*;q=0.8,text/html"
        , ["application/json"]
        , ["application/json"]
        )
      ; ( "Not all elements match"
        , "text/html,application/json"
        , ["text/xml"; "text/html"]
        , ["text/html"]
        )
      ; ( "Multiple selection"
        , "text/*"
        , ["text/xml"; "text/html"]
        , ["text/xml"; "text/html"]
        )
      ]
    in
    let test (name, data, from, expected) =
      let test () =
        let content_types = Accept.of_string data in
        let actual = Accept.preferred ~from content_types in
        Alcotest.(check @@ list string) data expected actual
      in
      ("Preferred: " ^ name, `Quick, test)
    in
    List.map test data

  let tests =
    List.concat
      [
        [
          ("Simple", `Quick, test_accept_simple)
        ; ("Complex", `Quick, test_accept_complex)
        ; ("Invalid", `Quick, test_invalid)
        ]
      ; preferred_tests
      ]
end

module Radix = struct
  let test_strings =
    [
      "/import_vdi"
    ; "/import_raw_vdi"
    ; "/export"
    ; "/export_metadata"
    ; "/import"
    ; "/import_metadata"
    ; "/migrate"
    ; "/console"
    ; "/host_backup"
    ; "/host_restore"
    ; "/host_logs_download"
    ; "/pool_patch_upload"
    ; "/oem_patch_stream"
    ; "/pool_patch_download"
    ; "/sync_config_files"
    ; "/pool/xmldbdump"
    ; "http"
    ; "/vncsnapshot"
    ; "/system-status"
    ; "/remote_db_access"
    ; "/remote_db_access_v2"
    ; "/remote_stats"
    ; "/json"
    ; "/cli"
    ; "/vm_rrd"
    ; "/rrd"
    ; "/host_rrd"
    ; "/rrd_updates"
    ; "/blob"
    ; "/remotecmd"
    ; "/rss"
    ; "/wlb_report"
    ; "/wlb_diagnostics"
    ; "/audit_log"
    ; "/"
    ]

  let a_radix_tree =
    let open Radix_tree in
    List.fold_left (fun t x -> insert x x t) empty test_strings

  let test_radix_tree1 _ =
    let open Radix_tree in
    let t = a_radix_tree in
    (* Check that each string can be found in the structure and maps to
       	   the right key *)
    List.iter
      (fun x ->
        if longest_prefix x t <> Some x then failwith (Printf.sprintf "x = %s" x)
      )
      test_strings

  let test_radix_tree2 _ =
    let open Radix_tree in
    let t = a_radix_tree in
    let all = fold (fun k _ acc -> k :: acc) [] t in
    if List.length all <> List.length test_strings then failwith "fold"

  let tests = [("1", `Quick, test_radix_tree1); ("2", `Quick, test_radix_tree2)]
end

module URL = struct
  let url =
    Alcotest.testable (Fmt.of_to_string Http.Url.to_string) Http.Url.equal

  let urls_data =
    let open Http.Url in
    [
      ( "file:/var/xapi/storage"
      , (File {path= "/var/xapi/storage"}, {path= "/"; query_params= []})
      )
    ; ( "http://root:foo@localhost"
      , ( Http
            {
              auth= Some (Basic ("root", "foo"))
            ; ssl= false
            ; host= "localhost"
            ; port= None
            }
        , {path= "/"; query_params= []}
        )
      )
    ; ( "https://google.com/gmail"
      , ( Http {auth= None; ssl= true; host= "google.com"; port= None}
        , {path= "/gmail"; query_params= []}
        )
      )
    ; ( "https://xapi.xen.org/services/SM"
      , ( Http {auth= None; ssl= true; host= "xapi.xen.org"; port= None}
        , {path= "/services/SM"; query_params= []}
        )
      )
    ; ( "https://root:foo@xapi.xen.org:1234/services/SM"
      , ( Http
            {
              auth= Some (Basic ("root", "foo"))
            ; ssl= true
            ; host= "xapi.xen.org"
            ; port= Some 1234
            }
        , {path= "/services/SM"; query_params= []}
        )
      )
    ; ( "https://xapi.xen.org/services/SM?foo=bar"
      , ( Http {auth= None; ssl= true; host= "xapi.xen.org"; port= None}
        , {path= "/services/SM"; query_params= [("foo", "bar")]}
        )
      )
    ]

  let url_tests =
    let test_url inp expected () =
      Alcotest.check url inp expected (Http.Url.of_string inp)
    in
    List.map (fun (i, e) -> (i, `Quick, test_url i e)) urls_data

  let modify_uri_test () =
    let data = "https://xapi.xen.org/services/SM?foo=bar" in
    let expected = "https://xapi.xen.org/services/SM/data?foo=bar" in
    let u = Http.Url.of_string data in
    let u' = Http.Url.set_path u (Http.Url.get_path u ^ "/data") in
    let actual = Http.Url.to_string u' in
    Alcotest.(check string) data expected actual

  let tests = ("Modify path", `Quick, modify_uri_test) :: url_tests
end

module Header = struct
  let with_fd input f =
    let read_fd, write_fd = Unix.pipe () in
    let (_ : int) =
      Unix.write write_fd (Bytes.of_string input) 0 (String.length input)
    in
    Fun.protect
      ~finally:(fun () -> Unix.close read_fd ; Unix.close write_fd)
      (fun () -> f read_fd)

  let cross xs ys zs =
    xs
    |> List.fold_left
         (fun acc x ->
           ys
           |> List.fold_left
                (fun acc y ->
                  zs |> List.fold_left (fun acc z -> (x, y, z) :: acc) acc
                )
                acc
         )
         []

  let test_read_http_request_header () =
    let proxy_str =
      "TCP6 ::ffff:10.71.152.135 ::ffff:10.71.152.134 53772 443"
    in
    let header1 =
      "POST / HTTP/1.0\r\n\
       content-length: 253\r\n\
       user-agent: xen-api-libs/1.0\r\n\
       connection: keep-alive\r\n\
       \r\n"
    in
    let header2 =
      "GET \
       /rrd_updates?session_id=OpaqueRef%3A26930e89-5c3c-4f80-a578-8a5344281532&start=1601481300&cf=AVERAGE&interval=5&host=true \
       HTTP/1.0\r\n\
       Host: 10.71.152.134\r\n\
       \r\n"
    in
    let mk_header_string ~frame ~proxy ~header =
      let b = Buffer.create 1024 in
      if proxy then Buffer.add_string b (Printf.sprintf "PROXY %s\r\n" proxy_str) ;
      if frame then Buffer.add_string b (Http.make_frame_header header) ;
      Buffer.add_string b header ;
      Buffer.to_bytes b |> Bytes.to_string
    in
    let test_cases = cross [true; false] [true; false] [header1; header2] in
    assert (List.length test_cases = 8) ;
    test_cases
    |> List.iter (fun (frame, proxy, header) ->
        with_fd (mk_header_string ~frame ~proxy ~header) (fun fd ->
            let actual_frame, actual_header, actual_proxy =
              Http.read_http_request_header ~read_timeout:None
                ~total_timeout:None ~max_length:None fd
            in
            assert (actual_frame = frame) ;
            assert (actual_header = header) ;
            assert (
              actual_proxy
              =
              if proxy then
                Some proxy_str
              else
                None
            )
        )
    )

  let with_socket ?(slowly = false) input f =
    let reader, writer = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.setsockopt_float reader Unix.SO_RCVTIMEO 5. ;
    let write () =
      if slowly then
        String.iter
          (fun c ->
            ignore (Unix.write_substring writer (String.make 1 c) 0 1) ;
            Thread.yield ()
          )
          input
      else
        ignore (Unix.write_substring writer input 0 (String.length input))
    in
    let writer_thread = Thread.create write () in
    Fun.protect
      ~finally:(fun () ->
        Thread.join writer_thread ; Unix.close reader ; Unix.close writer
      )
      (fun () -> f reader)

  let rec read_exactly fd n =
    if n = 0 then
      ""
    else
      let buf = Bytes.create n in
      let r = Unix.read fd buf 0 n in
      if r = 0 then failwith "unexpected end of file" ;
      Bytes.sub_string buf 0 r ^ read_exactly fd (n - r)

  let body = String.init 256 (fun i -> Char.chr (32 + (i mod 90)))

  let check_rest fd expected =
    Alcotest.(check string)
      "the bytes after the header are left unread" expected
      (read_exactly fd (String.length expected))

  let request_header =
    "POST / HTTP/1.1\r\n\
     content-length: 256\r\n\
     user-agent: xen-api-libs/1.0\r\n\
     connection: keep-alive\r\n\
     \r\n"

  let read_request fd =
    Http.read_http_request_header ~read_timeout:None ~total_timeout:None
      ~max_length:(Some 16384) fd

  let check_request (frame, header, proxy) ~frame:f ~proxy:p =
    Alcotest.(check bool) "frame" f frame ;
    Alcotest.(check string) "header" request_header header ;
    Alcotest.(check (option string)) "proxy" p proxy

  let test_request_body ~slowly () =
    with_socket ~slowly (request_header ^ body) @@ fun fd ->
    check_request (read_request fd) ~frame:false ~proxy:None ;
    check_rest fd body

  let test_pipelined_requests () =
    let one = request_header ^ body in
    with_socket (one ^ one) @@ fun fd ->
    check_request (read_request fd) ~frame:false ~proxy:None ;
    check_rest fd body ;
    check_request (read_request fd) ~frame:false ~proxy:None ;
    check_rest fd body

  let test_framed_and_proxied_request () =
    let proxy = "TCP4 10.0.0.1 10.0.0.2 53772 443" in
    let input =
      Printf.sprintf "PROXY %s\r\n%s%s%s" proxy
        (Http.make_frame_header request_header)
        request_header body
    in
    with_socket input @@ fun fd ->
    check_request (read_request fd) ~frame:true ~proxy:(Some proxy) ;
    check_rest fd body

  let test_header_length_header () =
    let prefix = "GET / HTTP/1.1\r\nhdr:" and suffix = "\r\nx: y\r\n\r\n" in
    let length = String.length prefix + 5 + String.length suffix in
    let header = Printf.sprintf "%s%05d%s" prefix length suffix in
    with_socket (header ^ body) @@ fun fd ->
    let _, actual, _ = read_request fd in
    Alcotest.(check string) "header" header actual ;
    check_rest fd body

  let test_request_too_large () =
    let header =
      "GET / HTTP/1.1\r\nx-padding: " ^ String.make 2048 'x' ^ "\r\n\r\n"
    in
    with_socket header @@ fun fd ->
    match
      Http.read_http_request_header ~read_timeout:None ~total_timeout:None
        ~max_length:(Some 1024) fd
    with
    | exception Http.Too_large ->
        ()
    | _ ->
        Alcotest.fail "expected Too_large"

  let test_request_stalled () =
    let partial = "GET / HTTP/1.1\r\nuser-agent: stalled\r\n" in
    with_socket partial @@ fun fd ->
    match
      Http.read_http_request_header ~read_timeout:(Some 0.2)
        ~total_timeout:(Some 1.) ~max_length:(Some 16384) fd
    with
    | exception Unix.Unix_error (Unix.EAGAIN, "read", _) ->
        ()
    | exception Http.Timeout ->
        ()
    | _ ->
        Alcotest.fail "expected a timeout"

  let response_header ?(padding = 0) () =
    let padding =
      List.init padding (fun i ->
          Printf.sprintf "x-padding-%03d: %s\r\n" i (String.make 100 'p')
      )
    in
    String.concat ""
      (["HTTP/1.1 200 OK\r\n"; "content-length: 256\r\n"; "x-a: b\r\n"]
      @ padding
      @ ["\r\n"]
      )

  let check_response ?(padding = 0) response =
    match response with
    | None ->
        Alcotest.fail "no response"
    | Some r ->
        Alcotest.(check string) "code" "200" r.Http.Response.code ;
        Alcotest.(check (option int64))
          "content-length" (Some 256L) r.Http.Response.content_length ;
        Alcotest.(check (option string))
          "x-a" (Some "b")
          (List.assoc_opt "x-a" r.Http.Response.additional_headers) ;
        Alcotest.(check int)
          "number of headers" (padding + 1)
          (List.length r.Http.Response.additional_headers)

  let test_response ~use_fastpath ~padding ~slowly () =
    with_socket ~slowly (response_header ~padding () ^ body) @@ fun fd ->
    check_response ~padding (Http_client.response_of_fd ~use_fastpath fd) ;
    check_rest fd body

  let test_response_pipe () =
    with_fd (response_header ~padding:20 () ^ body) @@ fun fd ->
    check_response ~padding:20 (Http_client.response_of_fd fd) ;
    check_rest fd body

  let tests =
    [
      ("read http header", `Quick, test_read_http_request_header)
    ; ("request, then body", `Quick, test_request_body ~slowly:false)
    ; ("request sent slowly, then body", `Quick, test_request_body ~slowly:true)
    ; ("pipelined requests", `Quick, test_pipelined_requests)
    ; ("framed, proxied request", `Quick, test_framed_and_proxied_request)
    ; ("header length header", `Quick, test_header_length_header)
    ; ("request header too large", `Quick, test_request_too_large)
    ; ("stalled request times out", `Quick, test_request_stalled)
    ; ( "response, then body"
      , `Quick
      , test_response ~use_fastpath:false ~padding:0 ~slowly:false
      )
    ; ( "response sent slowly, then body"
      , `Quick
      , test_response ~use_fastpath:false ~padding:0 ~slowly:true
      )
    ; ( "large response, then body"
      , `Quick
      , test_response ~use_fastpath:false ~padding:60 ~slowly:false
      )
    ; ( "response (fastpath), then body"
      , `Quick
      , test_response ~use_fastpath:true ~padding:0 ~slowly:false
      )
    ; ( "response (fastpath) sent slowly, then body"
      , `Quick
      , test_response ~use_fastpath:true ~padding:0 ~slowly:true
      )
    ; ("response over a pipe, then body", `Quick, test_response_pipe)
    ]
end

let () =
  Alcotest.run "Test HTTP library"
    [
      ("Accept", Accept.tests)
    ; ("Radix tree", Radix.tests)
    ; ("URL", URL.tests)
    ; ("Header", Header.tests)
    ]
