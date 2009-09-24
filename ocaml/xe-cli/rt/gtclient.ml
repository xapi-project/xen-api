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


module Protocol = Gtcomms.Make_Protocol (struct type t=Gtmessages.message end)
module Client = Gtcomms.Client (Protocol)
module Server = Gtcomms.Server (Protocol)

open Gtmessages

let rec nthtl l n =
  if n=0 then l else nthtl (List.tl l) (n-1)

let _ =
  let addr = Sys.argv.(1) in
  let msg = 
    match Sys.argv.(2) with
      "test" ->     Test
    | "shutdown" -> Shutdown (int_of_string Sys.argv.(3))
    | "reboot" ->   Reboot (int_of_string Sys.argv.(3))
    | "crash" ->    Crash 
    | "checkcd" ->  CheckCD (nthtl (Array.to_list Sys.argv) 3)    
    | "checkcdfail" -> CheckCDFail (nthtl (Array.to_list Sys.argv) 3)
    | "checkvif" -> CheckVIF Sys.argv.(3)
    | "checkdisks" -> CheckDisks (nthtl (Array.to_list Sys.argv) 3)
    | "checkmountdisk" -> CheckMountDisks (nthtl (Array.to_list Sys.argv) 3)
    | "setuptestdisk" -> SetupTestDisk Sys.argv.(3)
    | _ -> raise (Failure "Unknown command!")
  in 
  try
    let ans = Client.emit_answer addr 8085 msg in
    match ans with
      CmdResult str -> print_endline str
    | _ -> exit 0
  with
    exc -> 
      Printf.printf "Exception trapped: %s\n" (Printexc.to_string exc);
      exit 1
