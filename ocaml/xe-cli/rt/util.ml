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
(* Utility functions *)

open Pervasiveext

type t_cli = ?dolog:bool -> string -> (string * string)list -> string list * Unix.process_status

(* constants *)
let user = "root"
let password = "xenroot"
let host = ref "localhost"

let log=Testlog.log

(* Util functions for running CLI commands and collecting result codes
   and output *)

let run_command ?(dolog=true) cmd =
  log Log.Debug "Executing: %s" cmd;
  Printf.printf "Executing: %s\n%!" cmd;
  let ic = Unix.open_process_in cmd in
  let result : (string list) ref = ref [] in
  let read_str () =
    try
      while true do
	result := (input_line ic)::(!result);
      done
    with _ -> () in
  let _ = read_str() in
  result := List.rev !result;
  if dolog then List.iter (fun l -> log Log.Debug "%s" l) !result;
  let rc = Unix.close_process_in ic in
    (!result,rc)


let run_command_with_timeout cmd timeout =
  log Log.Debug "Executing with timeout: %s" cmd;
  let ic = Unix.open_process_in cmd in
  let f = Unix.descr_of_in_channel ic in
  let (a,b,c) = (Unix.select [f] [] [] timeout) in
  if List.length a = 0 
  then
    (log Log.Debug "Command timed out";
     ignore_int (Sys.command "killall nc");
     None)
  else
    let result : (string list) ref = ref [] in
    let read_str () =
      try
	while true do
	  result := (input_line ic) :: (!result) 
	done
      with _ -> () in
    let _ = read_str() in
    result := List.rev (!result);
    let rc = Unix.close_process_in ic in
    List.iter (fun l -> log Log.Debug "%s" l) !result;
    Some (!result,rc)

type pwspec =
    | NoPassword
    | Password of string
    | PasswordFile of string

let cli_with_pwspec ?(dolog=true) is_offhost cmd params pwspec =
  let rec mk_params l =
    match l with
	[] -> ""
      | ((k,v)::kvs) -> k^"=\""^v^"\""^" "^(mk_params kvs) in
  let param_str = mk_params params in
  let cli_base_string =
    (!Commands.xe)^" "^cmd
    ^(if is_offhost then
	  " -h "^(!host) else "")
    ^" "
    ^(match pwspec with
	| NoPassword -> ""
	| Password s -> "-u "^user^" -pw "^s
	| PasswordFile s -> "-pwf "^s)
    ^" "^param_str in
    run_command ~dolog cli_base_string

let cli_offhost_with_pwspec ?dolog cmd params pwspec =
  cli_with_pwspec ?dolog true cmd params pwspec

let cli_onhost ?dolog cmd params =
  cli_with_pwspec ?dolog false cmd params (NoPassword)

let cli_onhost_with_pwd ?dolog pwd cmd params =
  cli_with_pwspec ?dolog false cmd params (Password pwd)

let cli_offhost_with_pwd ?dolog pwd cmd params =
  cli_offhost_with_pwspec ?dolog cmd params (Password pwd)

let cli_offhost_with_pwf ?dolog pwf cmd params =
  cli_offhost_with_pwspec ?dolog cmd params (PasswordFile pwf)

let cli_offhost ?dolog cmd params =
  cli_offhost_with_pwd ?dolog password cmd params

  

(* Misc util funcs *)

let grep lines patt =
  let regex = Str.regexp patt in
  List.filter (fun l -> try ignore(Str.search_forward regex l 0); true with _ -> false) lines


			  


    
