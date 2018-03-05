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

(** This unit tests verifies that we have not broken the interface, by feeding
    the test responses and requests from the rpc-light directory to a dummy
    client and server generated from the interface definitions.
    The values we expect in the responses and requests are the ones in the
    corresponding request / response files in the rpc-light directory. *)

open Xapi_storage

let base_path = "../../../../rpc-light/"

let readfile filename =
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
  let buffer = String.make (1024 * 1024) '\000' in
  let length = Unix.read fd buffer 0 (String.length buffer) in
  let () = Unix.close fd in
  String.sub buffer 0 length

let path direction call = base_path ^ call ^ "/" ^ direction

(** Extra Alcotest.TESTABLE comparators *)
module Cmp = struct
  (** Create an Alcotest testable for something for which we have an [Rpc.Types.typ] *)
  let testable_of_rpc typ =
    let fmt = Fmt.of_to_string (fun v -> Rpcmarshal.marshal typ v |> Rpc.to_string) in
    Alcotest.testable fmt (=)

  (** Alcotest TESTABLE for the volume type *)
  let volume = testable_of_rpc Xapi_storage.Control.typ_of_volume
end

let test_volume =
  Xapi_storage.Control.{
    key="test_key";
    uuid=Some "test_uuid";
    name="test_name";
    description="test_description";
    read_write=true;
    sharable=false;
    virtual_size=0L;
    physical_utilisation=0L;
    uri=["uri1"];
    keys=[]
  }

(** Check that we successfully parse the responses and
    that the content of the parsed responses is correct. *)
let check_response_parser =
  let file_rpc call =
    let path = path "response" call.Rpc.name in
    readfile path |> Xmlrpc.response_of_string
  in

  let module R : Idl.RPCfunc = struct let rpc = file_rpc end in

  let sr =
    let module Sr = Xapi_storage.Control.Sr(Idl.GenClientExnRpc(R)) in

    let attach () =
      Alcotest.(check string) "Sr.attach return value" (Sr.attach "" "" "") "the_storage_repository";
    in
    let detach () =
      Alcotest.(check unit) "Sr.detach return value" (Sr.detach "" "") ()
    in
    let ls () =
      Alcotest.(check (array Cmp.volume)) "Sr.ls return value" [|test_volume|] (Sr.ls "" "")
    in

    [ "SR.attach", `Quick, attach
    ; "SR.detach", `Quick, detach
    ; "SR.ls", `Quick, ls
    ]
  in

  let volume =
    let module Volume = Xapi_storage.Control.Volume(Idl.GenClientExnRpc(R)) in

    let create () =
      Alcotest.(check Cmp.volume) "Volume.create return value" test_volume (Volume.create "" "" "" "" 0L false)
    in
    let clone () =
      Alcotest.(check Cmp.volume) "Volume.clone return value" test_volume (Volume.clone "" "" "")
    in
    let snapshot () =
      Alcotest.(check Cmp.volume) "Volume.snapshot return value" test_volume (Volume.snapshot "" "" "")
    in
    let destroy () =
      Alcotest.(check unit) "Volume.destroy" () (Volume.destroy "" "" "")
    in
    [ "Volume.create", `Quick, create
    ; "Volume.clone", `Quick, clone
    ; "Volume.snapshot", `Quick, snapshot
    ; "Volume.destroy", `Quick, destroy
    ]
  in

  sr @ volume

(** Check that we successfully parse the request and
    that the content of the parsed request is correct. *)
let check_request_parser =
  let call server call =
    let path = path "request" call in
    readfile path |> Xmlrpc.call_of_string |> server
  in

  let sr =
    let module Sr = Xapi_storage.Control.Sr(Idl.GenServerExn()) in
    let server = Idl.server Sr.implementation in

    let attach () =
      Sr.attach (fun dbg uuid uri ->
          Alcotest.(check string) "Sr.attach dbg" "OpaqueRef:65d6b084-07f3-0985-2478-64e989653b23" dbg;
          Alcotest.(check string) "Sr.attach uuid" "65a478f3-066a-71e6-339e-025d8ae4e992" uuid;
          Alcotest.(check string) "Sr.attach uri" "test_uri" uri;
          "");
      call server "SR.attach" |> ignore
    in
    let detach () =
      Sr.detach (fun dbg sr ->
          Alcotest.(check string) "Sr.detach dbg" "OpaqueRef:97bcede2-24b3-07c7-ce55-5d8aaf597750" dbg;
          Alcotest.(check string) "Sr.detach sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
          ());
      call server "SR.detach" |> ignore
    in
    let ls () =
      Sr.ls (fun dbg sr ->
          Alcotest.(check string) "Sr.ls dbg" "OpaqueRef:fbd1e3ed-ba49-3b9a-c04c-1ba3077d0029" dbg;
          Alcotest.(check string) "Sr.ls sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
          [||]);
      call server "SR.ls" |> ignore
    in
    [ "Sr.attach", `Quick, attach
    ; "Sr.detach", `Quick, detach
    ; "Sr.ls", `Quick, ls ]
  in

  let volume =
    let module Volume = Xapi_storage.Control.Volume(Idl.GenServerExn()) in
    let server = Idl.server Volume.implementation in

    let create () =
      Volume.create (fun dbg sr name description size sharable ->
          Alcotest.(check string) "Volume.create dbg" "OpaqueRef:9aa50d0c-4bf8-a03e-9796-3ca65459638a" dbg;
          Alcotest.(check string) "Volume.create sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
          Alcotest.(check string) "Volume.create name" "test_name" name;
          Alcotest.(check string) "Volume.create description" "test_description" description;
          Alcotest.(check int64) "Volume.create size" 4000000L size;
          Alcotest.(check bool) "Volume.create sharable" false sharable;
          test_volume);
      call server "create" |> ignore
    in

    [ "Volume.create", `Quick, create
    ]
  in

  sr @ volume

let () = Alcotest.run "suite"
    [ "check_response_parser", check_response_parser
    ; "check_response_parser", check_request_parser
    ]
