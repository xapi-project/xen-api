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
    corresponding request / response files in the rpc-light directory.
    It also has a test to check the interoperability between the generated
    client and server. *)

open Xapi_storage

let base_path = "../../rpc-light/"

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
    print_endline (Xmlrpc.string_of_call call);
    readfile path |> Xmlrpc.response_of_string
  in

  let module R : Idl.RPCfunc = struct let rpc = file_rpc end in

  let sr =
    let module Sr = Xapi_storage.Control.Sr(Idl.GenClientExnRpc(R)) in

    let attach () =
      Alcotest.(check string) "Sr.attach return value" (Sr.attach "" []) "the_storage_repository";
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

let unimplemented _ = failwith "unimplemented"

let sr_server () =
  let module Sr = Xapi_storage.Control.Sr(Idl.GenServerExn()) in

  Sr.attach (fun dbg configuration ->
      Alcotest.(check string) "Sr.attach dbg" "OpaqueRef:65d6b084-07f3-0985-2478-64e989653b23" dbg;
      Alcotest.(check (list (pair string string))) "Sr.attach configuration" ["a","b"; "c","d"] configuration;
      "attach_response");
  Sr.detach (fun dbg sr ->
      Alcotest.(check string) "Sr.detach dbg" "OpaqueRef:97bcede2-24b3-07c7-ce55-5d8aaf597750" dbg;
      Alcotest.(check string) "Sr.detach sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
      ());
  Sr.ls (fun dbg sr ->
      Alcotest.(check string) "Sr.ls dbg" "OpaqueRef:fbd1e3ed-ba49-3b9a-c04c-1ba3077d0029" dbg;
      Alcotest.(check string) "Sr.ls sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
      [||]);

  Sr.probe unimplemented;
  Sr.create unimplemented;
  Sr.destroy unimplemented;
  Sr.stat unimplemented;
  Sr.set_name unimplemented;
  Sr.set_description unimplemented;

  Idl.server Sr.implementation

let volume_server () =
  let module Volume = Xapi_storage.Control.Volume(Idl.GenServerExn()) in

  Volume.create (fun dbg sr name description size sharable ->
      Alcotest.(check string) "Volume.create dbg" "OpaqueRef:9aa50d0c-4bf8-a03e-9796-3ca65459638a" dbg;
      Alcotest.(check string) "Volume.create sr" "65a478f3-066a-71e6-339e-025d8ae4e992" sr;
      Alcotest.(check string) "Volume.create name" "test_name" name;
      Alcotest.(check string) "Volume.create description" "test_description" description;
      Alcotest.(check int64) "Volume.create size" 4000000L size;
      Alcotest.(check bool) "Volume.create sharable" false sharable;
      test_volume);

  Volume.snapshot unimplemented;
  Volume.clone unimplemented;
  Volume.destroy unimplemented;
  Volume.set_name unimplemented;
  Volume.set_description unimplemented;
  Volume.set unimplemented;
  Volume.unset unimplemented;
  Volume.resize unimplemented;
  Volume.stat unimplemented;
  Volume.compare unimplemented;
  Volume.similar_content unimplemented;
  Volume.enable_cbt unimplemented;
  Volume.disable_cbt unimplemented;
  Volume.data_destroy unimplemented;
  Volume.list_changed_blocks unimplemented;

  Idl.server Volume.implementation

(** Check that we successfully parse the request and
    that the content of the parsed request is correct. *)
let check_request_parser =
  let call server call =
    let path = path "request" call in
    readfile path |> Xmlrpc.call_of_string |> (server ())
  in

  let sr =
    let detach () = call sr_server "SR.detach" |> ignore in
    let ls () = call sr_server "SR.ls" |> ignore in

    [ "SR.detach", `Quick, detach
    ; "SR.ls", `Quick, ls
    ]
  in

  let volume =
    let create () = call volume_server "Volume.create" |> ignore in

    [ "Volume.create", `Quick, create
    ]
  in

  sr @ volume

(** Check that the generated client and server correctly communicate with each other *)
let test_client_server =
  let rpc server call =
    print_endline ("call: " ^ (Rpc.string_of_call call));
    let response = call |> (server ()) in
    print_endline ("response: " ^ (Rpc.string_of_response response));
    response
  in
  let sr =
    let module R : Idl.RPCfunc = struct
      let rpc = rpc sr_server
    end in
    let module Sr = Xapi_storage.Control.Sr(Idl.GenClientExnRpc(R)) in

    let attach () =
      Alcotest.(check string)
        "SR.attach response"
        "attach_response"
        (Sr.attach "OpaqueRef:65d6b084-07f3-0985-2478-64e989653b23" ["a","b"; "c","d"])
    in

    let ls () =
      Alcotest.(check (array Cmp.volume))
        "SR.attach response"
        [||]
        (Sr.ls "OpaqueRef:fbd1e3ed-ba49-3b9a-c04c-1ba3077d0029" "65a478f3-066a-71e6-339e-025d8ae4e992")
    in

    let set_name () =
      Alcotest.(check unit)
        "SR.stat response"
        ()
        (Sr.set_name "OpaqueRef:65d6b084-07f3-0985-2478-64e989653b23" "sr" "new_name")
    in

    [ "SR.attach", `Quick, attach
    ; "SR.ls", `Quick, ls
    ]
  in

  sr

let () = Alcotest.run "suite"
    [ "check_response_parser", check_response_parser
    ; "check_request_parser", check_request_parser
    ; "test_client_server", test_client_server
    ]
