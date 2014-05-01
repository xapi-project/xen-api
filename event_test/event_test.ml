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

open Core.Std
open Async.Std

open Xen_api
open Xen_api_async_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"
let enable_debug = ref false

let debug fmt =
        Printf.ksprintf
                (fun txt ->
                        if !enable_debug
                        then Printf.fprintf stderr "%s\n%!" txt
                ) fmt

let error fmt =
        Printf.ksprintf
                (fun txt ->
                        Printf.fprintf stderr "Error: %s\n%!" txt
                ) fmt

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat ~sep:" " params)
	| e -> failwith "XXX: figure out core/async error handling"

let watch_events rpc session_id =
        let open Event_types in
        let module StringMap = Map.Make(String) in

        let root = ref StringMap.empty in

        let rec loop token =
                Event.from rpc session_id ["*"] token 30. >>= fun rpc ->
                debug "received event: %s" (Jsonrpc.to_string rpc);
                let e = event_from_of_rpc rpc in
                List.iter ~f:(fun ev ->
                        (* type-specific table *)
                        let ty = match StringMap.find !root ev.ty with
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
                                        StringMap.add ty ~key:ev.reference ~data:s 
                                 end
                        | `del -> StringMap.remove ty ev.reference in
                        root := StringMap.add !root ~key:ev.ty ~data:ty
                ) e.events;
                loop e.token in
        loop ""

let main () =
	let rpc = make !uri in
	Session.login_with_password rpc !username !password "1.0"
	>>= fun session_id ->
        watch_events rpc session_id >>= fun result ->
        ( if result
          then Printf.printf "Event test OK\n"
          else Printf.printf "Event test failed\n"  );
	Session.logout rpc session_id
	>>= fun () ->
	shutdown (if result then 0 else 1);
	return ()


let _ =
	Arg.parse [
		"-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
                "-debug", Arg.Set enable_debug, (Printf.sprintf "Enable debug logging (default %b)" !enable_debug);
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which tracks the server state via events";

	let (_: unit Deferred.t) = main () in
	never_returns (Scheduler.go ())
