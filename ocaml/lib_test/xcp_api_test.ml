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

open OUnit

let ( |> ) a b = b a
let id x = x

open Control

module S = (SR_test(Lwt): SR with type 'a t = 'a Lwt.t)
module S_d = SR_server_dispatcher(S)
module SR = SR_client(S_d)


module V = (Volume_test(Lwt): Volume with type 'a t = 'a Lwt.t)
module V_d = Volume_server_dispatcher(V)
module Volume = Volume_client(V_d)

let base_path = "../../rpc-light/"

let readfile filename =
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
  let buffer = String.make (1024 * 1024) '\000' in
  let length = Unix.read fd buffer 0 (String.length buffer) in
  let () = Unix.close fd in
  String.sub buffer 0 length

let expect_ok = function
  | `Ok _ -> ()
  | `Error e -> raise e

let check_request_parser f relative_path =
  (base_path ^ relative_path) |> readfile |> Xmlrpc.call_of_string |> f |> expect_ok

let check_sr_request_parser = check_request_parser Control.Types.SR.In.of_call

let sr_attach_request _ = check_sr_request_parser "sr.attach/request"
let sr_detach_request _ = check_sr_request_parser "sr.detach/request"
let sr_scan_request   _ = check_sr_request_parser "sr.scan/request"

let check_volume_request_parser = check_request_parser Control.Types.Volume.In.of_call

let volume_clone_request      _ = check_volume_request_parser "volume.clone/request"
let volume_create_request     _ = check_volume_request_parser "volume.create/request"
let volume_destroy_request    _ = check_volume_request_parser "volume.destroy/request"
let volume_snapshot_request   _ = check_volume_request_parser "volume.snapshot/request"

let sr_attach_response _ =
  let xml = readfile (base_path ^ "sr.attach/response") in
  let resp = Xmlrpc.response_of_string xml in
  match Control.result_of_response resp with
  | `Ok x -> let (_: Control.Types.SR.Attach.Out.t) = Control.Types.SR.Attach.Out.t_of_rpc x in ()
  | `Error e -> raise e

let sr_detach_response _ =
  let xml = readfile (base_path ^ "sr.detach/response") in
  let resp = Xmlrpc.response_of_string xml in
  match Control.result_of_response resp with
  | `Ok x -> let (_: Control.Types.SR.Detach.Out.t) = Control.Types.SR.Detach.Out.t_of_rpc x in ()
  | `Error e -> raise e

let exception_marshal_unmarshal e _ =
  let e = Control.Cancelled "foo" in
  match Control.result_of_response (Control.response_of_exn e) with
  | `Error e' when e = e' -> ()
  | `Ok x -> failwith "unexpected success"
  | `Error e -> raise e

let exception_marshal_unmarshal1 = exception_marshal_unmarshal (Control.Cancelled "bad luck")

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xcp-api protocol code";

  let suite = "xen-api" >:::
              [
                "sr_attach_request" >:: sr_attach_request;
                "sr_attach_response" >:: sr_attach_response;
                "sr_detach_request" >:: sr_detach_request;
                "sr_detach_response" >:: sr_detach_request;
                "exception_marshal_unmarshal1" >:: exception_marshal_unmarshal1;
                "sr_scan_request" >:: sr_scan_request;
                "volume_clone_request" >:: volume_clone_request;
                "volume_create_request" >:: volume_create_request;
                "volume_destroy_request" >:: volume_destroy_request;
                "volume_snapshot_request" >:: volume_snapshot_request;
              ] in
  run_test_tt ~verbose:!verbose suite
