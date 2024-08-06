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

open Xen_api

module Fake_IO = struct
  type 'a t = T of 'a

  let return x = T x

  let ( >>= ) t f = match t with T x -> f x

  type ic = string Queue.t

  type oc = string Queue.t

  type conn = unit

  let read_line ic =
    if Queue.is_empty ic then
      (*			Printf.fprintf stderr "read_line = None\n%!"; *)
      return None
    else
      let chunk = Queue.pop ic in
      (*			Printf.fprintf stderr "read_line = %s\n%!" chunk;*)
      return (Some chunk)

  let read ic n =
    let chunk = Queue.pop ic in
    (*		Printf.fprintf stderr "read %d\n%!" n;*)
    assert (String.length chunk <= n) ;
    return chunk

  let write oc string = Queue.push string oc ; return ()

  let flush _oc = return ()

  type connection = {ic: ic; oc: ic}

  let connections = ref []

  let open_connection _ =
    let ic = Queue.create () and oc = Queue.create () in
    let c = {ic; oc} in
    connections := c :: !connections ;
    return (Ok (ic, oc))

  let close (ic, oc) =
    let this_one c = c.ic == ic && c.oc == oc in
    ignore (List.find this_one !connections) ;
    connections := List.filter (fun c -> not (this_one c)) !connections ;
    return ()

  let timeofday = ref 0.

  let gettimeofday () = !timeofday

  let num_sleeps = ref 0

  let sleep x =
    incr num_sleeps ;
    timeofday := !timeofday +. x ;
    return ()
end

module C = Client.Client

let test_login_fail () =
  let module M = Xen_api.Make (Fake_IO) in
  let open Fake_IO in
  let rpc req =
    let xml = Xmlrpc.string_of_call req in
    M.rpc (M.make (Uri.of_string "http://127.0.0.1/")) xml >>= function
    | Ok _ ->
        failwith "should have failed with No_response"
    | Error e ->
        raise e
  in
  timeofday := 0. ;
  num_sleeps := 0 ;
  ( try
      let _session_id =
        C.Session.login_with_password ~rpc ~uname:"root" ~pwd:"password"
          ~version:"1.0" ~originator:"xen-api test"
      in
      ()
    with Xen_api.No_response -> ()
  ) ;
  Alcotest.(check @@ float Float.epsilon) "timeofday" 31. !timeofday ;
  Alcotest.(check int) "num_sleeps" 31 !num_sleeps ;
  ()

let test_login_success () =
  let session_id = "OpaqueRef:9e9cf047-76d7-9f3a-62ca-cb7bacf5a4e1" in
  let result =
    Printf.sprintf
      "<methodResponse><params><param><value><struct><member><name>Status</name><value>Success</value></member><member><name>Value</name><value>%s</value></member></struct></value></param></params></methodResponse>"
      session_id
  in
  let module Fake_IO = struct
    include Fake_IO

    let open_connection _ =
      let ic = Queue.create () and oc = Queue.create () in
      Queue.push "HTTP/1.1 200 OK\r\n" ic ;
      Queue.push
        (Printf.sprintf "Content-length: %d\r\n" (String.length result))
        ic ;
      Queue.push "\r\n" ic ;
      Queue.push result ic ;
      let c = {ic; oc} in
      connections := c :: !connections ;
      return (Ok (ic, oc))
  end in
  let open Fake_IO in
  let module M = Xen_api.Make (Fake_IO) in
  let rpc call =
    let s = Xmlrpc.string_of_call call in
    M.rpc (M.make (Uri.of_string "http://127.0.0.1/")) s >>= function
    | Ok x ->
        Xmlrpc.response_of_string x
    | Error e ->
        raise e
  in
  let session_id' =
    C.Session.login_with_password ~rpc ~uname:"root" ~pwd:"password"
      ~version:"1.0" ~originator:"xen-api test"
  in
  Alcotest.(check string) "session_id" session_id (API.Ref.string_of session_id')

let () =
  Alcotest.run "xen-api-client"
    [
      ( "login"
      , [
          ("fail", `Quick, test_login_fail)
        ; ("success", `Quick, test_login_success)
        ]
      )
    ]
