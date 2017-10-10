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

open Xen_api

module Fake_IO = struct
  type 'a t = T of 'a
  let return x = T x
  let (>>=) t f = match t with
    | T x -> f x

  let (>>) m n = m >>= fun _ -> n

  let rec iter f = function
    | [] -> return ()
    | x :: xs -> f x >>= fun () -> (iter f xs)

  type ic = string Queue.t
  type oc = string Queue.t
  type conn = unit

  let read_line ic =
    if Queue.is_empty ic then begin
      (*			Printf.fprintf stderr "read_line = None\n%!"; *)
      return None
    end else begin
      let chunk = Queue.pop ic in
      (*			Printf.fprintf stderr "read_line = %s\n%!" chunk;*)
      return (Some chunk)
    end

  let read ic n =
    let chunk = Queue.pop ic in
    (*		Printf.fprintf stderr "read %d\n%!" n;*)
    assert (String.length chunk <= n);
    return chunk

  let read_exactly ic buf off len =
    return (if Queue.is_empty ic then false else begin
        let chunk = Queue.pop ic in
        String.blit chunk 0 buf off len;
        true
      end)

  let read_exactly ic len =
    let buf = Bytes.create len in
    read_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None


  let write oc string = Queue.push string oc; return ()

  let flush _oc = return ()

  type connection = {
    address: Uri.t;
    ic: ic;
    oc: ic;
  }
  let connections = ref []

  let open_connection address =
    let ic = Queue.create () and oc = Queue.create () in
    let c = { address; ic; oc } in
    connections := c :: !connections;
    return (Ok (ic, oc))

  let close (ic, oc) =
    let this_one c = c.ic == ic && c.oc == oc in
    ignore(List.find this_one !connections);
    connections := List.filter (fun c -> not(this_one c)) !connections;
    return ()

  let timeofday = ref 0.
  let gettimeofday () = !timeofday

  let num_sleeps = ref 0
  let sleep x = incr num_sleeps; timeofday := !timeofday +. x; return ()
end

module C = Client.Client

let test_login_fail _ =
  let module M = Xen_api.Make(Fake_IO) in
  let open Fake_IO in
  let rpc req =
    let xml = Xmlrpc.string_of_call req in
    M.rpc (M.make (Uri.of_string "http://127.0.0.1/")) xml
    >>= function
    | Ok _ -> failwith "should have failed with No_response"
    | Error e -> raise e in
  timeofday := 0.;
  num_sleeps := 0;
  begin
    try
      let _session_id = C.Session.login_with_password ~rpc:rpc ~uname:"root" ~pwd:"password" ~version:"1.0" ~originator:"xen-api test" in
      ()
    with Xen_api.No_response -> ()
  end;
  assert_equal ~printer:string_of_float ~msg:"timeofday" 31. !timeofday;
  assert_equal ~printer:string_of_int ~msg:"num_sleeps" 31 !num_sleeps;
  ()

let test_login_success _ =
  let session_id = "OpaqueRef:9e9cf047-76d7-9f3a-62ca-cb7bacf5a4e1" in
  let result = Printf.sprintf "<methodResponse><params><param><value><struct><member><name>Status</name><value>Success</value></member><member><name>Value</name><value>%s</value></member></struct></value></param></params></methodResponse>" session_id in
  let module Fake_IO = struct
    include Fake_IO
    let open_connection address =
      let ic = Queue.create () and oc = Queue.create () in
      Queue.push "HTTP/1.1 200 OK\r\n" ic;
      Queue.push (Printf.sprintf "Content-length: %d\r\n" (String.length result)) ic;
      Queue.push "\r\n" ic;
      Queue.push result ic;
      let c = { address; ic; oc } in
      connections := c :: !connections;
      return (Ok (ic, oc))
  end in
  let open Fake_IO in
  let module M = Xen_api.Make(Fake_IO) in
  let rpc call =
    let s = Xmlrpc.string_of_call call in
    M.rpc (M.make (Uri.of_string "http://127.0.0.1/")) s
    >>= function
    | Ok x -> Xmlrpc.response_of_string x
    | Error e -> raise e in
  let session_id' = C.Session.login_with_password ~rpc ~uname:"root" ~pwd:"password" ~version:"1.0" ~originator:"xen-api test" in
  assert_equal ~msg:"session_id" session_id (API.Ref.string_of session_id')

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xen-api protocol code";

  let suite = "xen-api" >:::
              [
                "login_fail" >:: test_login_fail;
                "login_success" >:: test_login_success;
              ] in
  run_test_tt ~verbose:!verbose suite
