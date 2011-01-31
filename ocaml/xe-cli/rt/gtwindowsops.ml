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

let rec read ic cur = 
  try
    let line = input_line ic in
    read ic (line::cur) 
  with
    _ -> List.rev cur

let shutdown timeout =
  let ic = Unix.open_process_in (Printf.sprintf "SHUTDOWN -s -t %d" timeout) in
  let result = read ic [] in
  (try close_in ic with _ -> ());
  Gtmessages.CmdResult (String.concat "\n" result)

let reboot timeout =
  let ic = Unix.open_process_in (Printf.sprintf "SHUTDOWN -r -t %d" timeout) in
  let result = read ic [] in
  (try close_in ic with _ -> ());
  Gtmessages.CmdResult (String.concat "\n" result)

 let crash () =
   Gtmessages.CmdResult "Unimplemented in windows!"

let rec myfilter func l =
  match l with
    x::xs -> if func x then x::myfilter func xs else myfilter func xs
  | _ -> []

let parse_diskpart ls =
  Printf.printf "In parse_diskpart\n";
  flush_all ();
  let volumes = (myfilter (fun l -> try 
    let substr = String.lowercase (String.sub l 2 6) in
    Printf.printf "1. substr='%s' comparing with '%s'\n" substr "volume";
    Printf.printf "   result=%b\n" (substr="volume");
    flush_all ();
    substr="volume" with _ -> false) ls) in
  List.iter (fun l -> Printf.printf "%s\n" l) volumes;
  let cds = List.filter (fun l -> try 
    Printf.printf "2. substr=%s\n" (String.sub l 30 4);
    "CDFS"=String.sub l 32 4 with _ -> false) volumes in
  let mapfn driveline = 
    let drive = String.sub driveline 15 1 in
    drive in
  let cddrives = List.map mapfn cds in
  cddrives
    
let checkcds devices _ =
  let (ic,oc) = Unix.open_process "DISKPART.EXE" in
  Printf.fprintf oc "list volume\nexit\n";
  flush_all ();
  let result = read ic [] in
  (try close_out oc with _ -> ());
  (try close_in ic with _ -> ());
  let drives = parse_diskpart result in
  let mapfn drive =
    let cmd = "DIR " ^ drive ^ ":\\" in
      Printf.printf "cmd=%s\n" cmd;
      flush_all ();
      let ic = Unix.open_process_in cmd in
      let result = read ic [] in
      (try close_in ic with _ -> ());
      String.concat "\n" result
    in
  Gtmessages.CmdResult (String.concat "\n" (List.map mapfn drives))
     
let checkdisks devices =
  let (ic,oc) = Unix.open_process "DISKPART.EXE" in
  Printf.fprintf oc "list disk\nexit\n";
  flush_all ();
  let result = read ic [] in
  (try close_in ic with _ -> ());
  (try close_out oc with _ -> ());  
  Gtmessages.CmdResult (String.concat "\n" result)

let checkmountdisks devices =
  let ic = Unix.open_process_in "DIR D:\\" in
  let result = read ic [] in
  (try close_in ic with _ -> ());
  Gtmessages.CmdResult (String.concat "\n" result)

let checkvifs vif =
  let ic = Unix.open_process_in "ipconfig /all" in
  let result = read ic [] in
  (try close_in ic with _ -> ());
  Gtmessages.CmdResult (String.concat "\n" result)

let logerr msg = 
  let oc = open_out "c:\\gtserver.log" in
  Printf.fprintf oc "%s\n" msg;
  close_out oc
