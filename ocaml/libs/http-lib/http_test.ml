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
        if longest_prefix x t <> Some x then
          failwith (Printf.sprintf "x = %s" x)
      )
      test_strings

  let test_radix_tree2 _ =
    let open Radix_tree in
    let t = a_radix_tree in
    let all = fold (fun k _ acc -> k :: acc) [] t in
    if List.length all <> List.length test_strings then
      failwith "fold"

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
               assert (actual_proxy = if proxy then Some proxy_str else None)
           )
       )

  let tests = [("read http header", `Quick, test_read_http_request_header)]
end

let () =
  Alcotest.run "Test HTTP library"
    [
      ("Accept", Accept.tests)
    ; ("Radix tree", Radix.tests)
    ; ("URL", URL.tests)
    ; ("Header", Header.tests)
    ]
