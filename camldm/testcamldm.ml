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
open Camldm

let _ =
  let name = Sys.argv.(1) in
  let start = Int64.of_string Sys.argv.(2) in
  let len = Int64.of_string Sys.argv.(3) in
  let dev = Sys.argv.(4) in
  let offset = Int64.of_string Sys.argv.(5) in
  let dev2 = Sys.argv.(6) in
  let offset2 = Int64.of_string Sys.argv.(7) in

  let buf = String.create 512 in

  Camldm.create name [| { start=start; 
			  len=len; 
			  map = Striped {chunk_size=8L; 
					 dests=[| {device=dev;offset=offset}; 
						  {device=dev2;offset=offset2} |] } } |];

  let s = Camldm.table name in
  let (major,minor) = s.major,s.minor in
  let nod = "/tmp/foobar" in
  Camldm.mknod nod 0o644 (Int32.to_int major) (Int32.to_int minor);
  let ifd = Unix.openfile nod [Unix.O_RDONLY] 0o000 in
  Printf.printf "Status:\nexists: %b\nsuspended: %b\nlive_table: %b\ninactive_table: %b\n" s.exists s.suspended s.live_table s.inactive_table;
  Printf.printf "open_count: %ld\nevent_nr: %ld\nmajor: %ld\nminor: %ld\n"
    s.open_count s.event_nr s.major s.minor;
  Printf.printf "read_only: %b\n" s.read_only;
  Printf.printf "\nTable:\n";
  List.iter (fun (s,l,t,p) -> Printf.printf " %Ld %Ld %s %s\n" s l t p) s.targets;
  let input = Unix.read ifd buf 0 10 in
  Printf.printf "input=%d\n" input;
  for i=0 to 2 do 
    Printf.printf "%d " (int_of_char buf.[i])
  done;
  let name=Printf.sprintf "/sys/block/dm-%ld/dev" minor in
  Printf.printf "Got minor=%ld - looking for: %s\n" minor name;
  let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
  let input = Unix.read fd buf 0 10 in
  Printf.printf "input=%d\n" input;
  for i=0 to 2 do 
    Printf.printf "%d " (int_of_char buf.[i])
  done;
  Printf.printf "\n";
  Unix.close fd


  (*List.iter (fun (a,b,c,d,e,f) -> Printf.printf "%d %d %Ld %Ld %s %s" a b c d e f) l; *)
(*  Camldm.remove name*)

    
