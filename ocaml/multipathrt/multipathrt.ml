(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* Multipath test suite *)

open Client
open Globs

let rpc_of_hostname host xml =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~srcstr:"multipathrt" ~dststr:"xapi" ~transport:(TCP(host, 80)) ~http:(xmlrpc ~version:"1.0" "/") xml

let _ =
  Arg.parse [
    "-h", Arg.Set_string hostname, "Hostname of the XenServer";
    "-u", Arg.Set_string username, "Username";
    "-p", Arg.Set_string password, "Password";
    "-tc", Arg.Set_int tc, "Test case number"]
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-h hostname] [-u username] [-p password] [-tc test case number]" binary_name);
  
  (* Sanity check *)
  if !hostname="" then failwith "Need a host name!";
  if !username="" then failwith "Need a user name!";
  if !password="" then failwith "Need a password!";
  if !tc=0 then failwith "Need a test case!";

  (* Get a session *)
  let rpc = rpc_of_hostname !hostname in
  let session = Client.Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.4" in
  try
    begin
      match !tc with
      (* Device-mapper table integrity tests *)
      | 9068 -> Tc_9068.run_iscsi rpc session
      | 9069 -> Tc_9069.run_netapp rpc session
      | 9078 -> Tc_9078.run_fc rpc session
      (* Path fail-over time tests *)
      | 9071 -> Tc_9071.run_iscsi rpc session
      | 9072 -> Tc_9072.run_netapp rpc session
      | 9079 -> Tc_9079.run_fc rpc session
      (* Alert tests *)
      | 9074 -> Tc_9074.run rpc session
      | _ -> failwith "Unknown test case!"
    end;
    Printf.printf "PASS\n"
  with
  | Multipathrt_exceptions.Test_error str ->
    Printf.printf "Test error: %s\n" str;
    Printf.printf "ERROR\n"
  | Failure str ->
    Printf.printf "Fatal error: %s\n" str;
    Printf.printf "FAIL\n"
  | e ->
    Printf.printf "Unexpected exception: %s\n" (Printexc.to_string e);
    Printf.printf "ERROR\n"
