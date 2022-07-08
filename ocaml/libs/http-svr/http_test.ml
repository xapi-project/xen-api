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

open OUnit2
open Http

let test_accept_simple _ =
  let t = Accept.t_of_string "application/json" in
  assert_equal ~msg:"ty"
    ~printer:(Option.value ~default:"None")
    t.Accept.ty (Some "application") ;
  assert_equal ~msg:"subty"
    ~printer:(Option.value ~default:"None")
    t.Accept.subty (Some "json") ;
  assert (Accept.matches ("application", "json") t)

let test_accept_complex _ =
  let ts =
    Accept.ts_of_string
      "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  in
  let m = Accept.preferred_match ("text", "html") ts in
  assert ((Option.get m).Accept.ty = Some "text") ;
  let m = Accept.preferred_match ("foo", "bar") ts in
  assert ((Option.get m).Accept.ty = None)

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

let make_radix_tree () =
  let open Radix_tree in
  List.fold_left (fun t x -> insert x x t) empty test_strings

let test_radix_tree1 _ =
  let open Radix_tree in
  let t = make_radix_tree () in
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
  let t = make_radix_tree () in
  let all = fold (fun k _ acc -> k :: acc) [] t in
  if List.length all <> List.length test_strings then
    failwith "fold"

let test_url _ =
  let open Http in
  let open Http.Url in
  ( match of_string "file:/var/xapi/storage" with
  | File {path= "/var/xapi/storage"}, {uri= "/"; _} ->
      ()
  | _ ->
      assert false
  ) ;
  ( match of_string "http://root:foo@localhost" with
  | Http t, {uri= "/"; _} ->
      assert (t.auth = Some (Basic ("root", "foo"))) ;
      assert (t.ssl = false) ;
      assert (t.host = "localhost")
  | _ ->
      assert false
  ) ;
  ( match of_string "https://google.com/gmail" with
  | Http t, {uri= "/gmail"; _} ->
      assert (t.ssl = true) ;
      assert (t.host = "google.com")
  | _ ->
      assert false
  ) ;
  ( match of_string "https://xapi.xen.org/services/SM" with
  | Http t, {uri= "/services/SM"; _} ->
      assert (t.ssl = true) ;
      assert (t.host = "xapi.xen.org")
  | _ ->
      assert false
  ) ;
  ( match of_string "https://root:foo@xapi.xen.org:1234/services/SM" with
  | Http t, {uri= "/services/SM"; _} ->
      assert (t.auth = Some (Basic ("root", "foo"))) ;
      assert (t.port = Some 1234) ;
      assert (t.ssl = true) ;
      assert (t.host = "xapi.xen.org")
  | _ ->
      assert false
  ) ;
  ( match of_string "https://xapi.xen.org/services/SM?foo=bar" with
  | Http t, {uri= "/services/SM"; query_params= [("foo", "bar")]} ->
      assert (t.ssl = true) ;
      assert (t.host = "xapi.xen.org")
  | _ ->
      assert false
  ) ;
  let u = of_string "https://xapi.xen.org/services/SM?foo=bar" in
  let u' = set_uri u (get_uri u ^ "/data") in
  let s = to_string u' in
  assert (s = "https://xapi.xen.org/services/SM/data?foo=bar")

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

let test_read_http_request_header _ =
  let proxy_str = "TCP6 ::ffff:10.71.152.135 ::ffff:10.71.152.134 53772 443" in
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
               Http.read_http_request_header ~read_timeout:None fd
             in
             assert (actual_frame = frame) ;
             assert (actual_header = header) ;
             assert (actual_proxy = if proxy then Some proxy_str else None)
         )
     )

let _ =
  let suite =
    "HTTP test"
    >::: [
           "accept_simple" >:: test_accept_simple
         ; "accept_complex" >:: test_accept_complex
         ; "radix1" >:: test_radix_tree1
         ; "radix2" >:: test_radix_tree2
         ; "test_url" >:: test_url
         ; "test_read_http_request_header" >:: test_read_http_request_header
         ]
  in
  run_test_tt_main suite
