(*
 * Copyright (C) 2012-2014 Citrix Systems Inc.
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

open Core
open Async

open Xen_api_async_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"
let enable_debug = ref false

let debug fmt =
  Printf.ksprintf
    (fun txt ->
       if !enable_debug
       then eprintf "%s\n%!" txt
    ) fmt

let error fmt =
  Printf.ksprintf
    (fun txt ->
       eprintf "Error: %s\n%!" txt
    ) fmt

let info fmt =
  Printf.ksprintf
    (fun txt ->
       eprintf "%s\n%!" txt
    ) fmt

let exn_to_string = function
  | Api_errors.Server_error(code, params) ->
    Printf.sprintf "%s %s" code (String.concat ~sep:" " params)
  | e -> failwith (Printf.sprintf "Unexpected error: %s" (Exn.to_string e))

let watch_events rpc session_id =
  let open Event_types in
  let module StringMap = Map.Make(String) in

  let root = ref StringMap.empty in

  let update map ev =
    (* type-specific table *)
    let ty = match StringMap.find map ev.ty with
      | None -> StringMap.empty
      | Some x -> x in
    let ty = match ev.op with
      | `add
      | `_mod ->
        begin match ev.snapshot with
          | None ->
            error "Event contained no snapshot";
            ty
          | Some s ->
            StringMap.update ty ev.reference ~f:(fun _ -> s)
        end
      | `del -> StringMap.remove ty ev.reference in
    if StringMap.is_empty ty
    then StringMap.remove map ev.ty
    else StringMap.update map ev.ty ~f:(fun _ -> ty) in

  let compare () =
    let open Event_types in
    Event.from ~rpc ~session_id ~classes:["*"] ~token:"" ~timeout:0. >>= fun rpc ->
    let e = event_from_of_rpc rpc in
    if e.events = [] then error "Empty list of events";
    let current = List.fold_left ~init:StringMap.empty ~f:update e.events in
    Sequence.iter ~f:(fun (key, diff) -> match key, diff with
        | key, `Left _ -> error "Replica has extra table: %s" key
        | key, `Right _ -> error "Replica has missing table: %s" key
        | _, `Unequal(_,_) -> ()
      ) (StringMap.symmetric_diff !root current ~data_equal:(fun _ _ -> true));
    List.iter ~f:(fun key ->
        match StringMap.find !root key with
        | None ->
          error "Table missing in replica: %s" key
        | Some root_table ->
          let current_table = StringMap.find_exn current key in
          Sequence.iter ~f:(fun (key, diff) -> match key, diff with
              | r, `Left rpc -> error "Replica has extra object: %s: %s" r (Jsonrpc.to_string rpc)
              | r, `Right rpc -> error "Replica has missing object: %s: %s" r (Jsonrpc.to_string rpc)
              | r, `Unequal(rpc1, rpc2) -> error "Replica has out-of-sync object: %s: %s <> %s" r (Jsonrpc.to_string rpc1) (Jsonrpc.to_string rpc2)
            ) (StringMap.symmetric_diff root_table current_table ~data_equal:(fun a b -> a = b))
      ) (StringMap.keys current);
    return () in

  let rec loop token =
    Event.from ~rpc ~session_id ~classes:["*"] ~token ~timeout:30. >>= fun rpc ->
    debug "received event: %s" (Jsonrpc.to_string rpc);
    let e = event_from_of_rpc rpc in
    List.iter ~f:(fun ev -> root := update !root ev) e.events;
    compare () >>= fun () ->
    info "object counts: %s" (String.concat ~sep:", " (List.map ~f:(fun key ->
        Printf.sprintf "%s (%d)" key (StringMap.length (StringMap.find_exn !root key))
      ) (StringMap.keys !root)));
    loop e.token in
  loop ""

let main () =
  let rpc = make !uri in
  Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.0" ~originator:"event_test"
  >>= fun session_id ->
  let a = watch_events rpc session_id in
  let b = watch_events rpc session_id in
  a >>= fun () ->
  b >>= fun () ->
  Session.logout ~rpc ~session_id
  >>= fun () ->
  shutdown 0;
  return ()


let _ =
  Arg.parse [
    "-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
    "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
    "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
    "-debug", Arg.Set enable_debug, (Printf.sprintf "Enable debug logging (default %b)" !enable_debug);
  ] (fun x -> eprintf "Ignoring argument: %s\n" x)
    "Simple example which tracks the server state via events";

  let (_: unit Deferred.t) = main () in
  never_returns (Scheduler.go ())
