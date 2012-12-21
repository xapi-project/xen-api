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
(* LVHD test suite *)

open Client
open Globs

let rpc_of_hostname host =
  fun xml -> 
	  let open Xmlrpc_client in
	  let http = xmlrpc ~version:"1.0" "/" in
	  XMLRPC_protocol.rpc ~srcstr:"lvhdrt" ~dststr:"xapi" ~transport:(TCP(host, 80)) ~http xml

let _ =
  Arg.parse [
    "-h", Arg.Set_string hostname, "Hostname of the XenServer";
    "-u", Arg.Set_string username, "Username";
    "-p", Arg.Set_string password, "Password";
    "-tc", Arg.Set_int tc, "Test case number"]
    (fun _ -> failwith "Invalid argument")
    "Usage: lvhdrt [-h hostname] [-u username] [-p password] [-tc test case number]";
  
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
      | 8699 -> Tc_8699.tc1 rpc session
      | 8670 -> Tc_8670.resize_test rpc session; Tc_8670.clone_test rpc session
      | 8682 -> Tc_8682.run rpc session
      | 8700 -> Tc_8700.run rpc session
      | 8707 -> Tc_8707.run rpc session
      | 8766 -> Tc_8766.run rpc session
	  | 8775 -> Tc_8775.run rpc session
      | _ -> failwith "Unknown test case!"
    end;
    Printf.printf "PASS\n"
  with
  | Lvhdrt_exceptions.Test_error str ->
    Printf.printf "Test error: %s\n" str;
    Printf.printf "ERROR\n"
  | Failure str ->
    Printf.printf "Fatal error: %s\n" str;
    Printf.printf "FAIL\n"
