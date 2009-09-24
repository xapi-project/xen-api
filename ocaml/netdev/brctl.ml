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

(* Quick mini-implementation of brctl to check our bridge lib is working *)

open Netdev
open Printf

let usage_and_quit () = 
      List.iter (fun x -> output_string stderr x; output_string stderr "\n") 
	[
	  "commands:";
	  "\taddbr <bridge>            add bridge";
	  "\tdebr  <bridge>            delete bridge";
	  "\taddif <bridge> <device>   add interface to bridge";
	  "\tdelif <bridge> <device>   delete interface from bridge";
	  "\tshow                      show a list of bridges"
	];
      exit 1

let _ = 
  if Array.length Sys.argv < 2 then usage_and_quit ();

  match Sys.argv.(1) with
  | "show" ->
      let all = Bridge.list () in
      let print = printf "%-15s %-23s %-15s %-16s\n" in
      print "bridge_name" "bridge id" "STP enabled" "interfaces";
      List.iter 
	(fun bridge ->
	   let id = Bridge.get_id bridge in
	   let stp = if Bridge.get_stp_state bridge then "yes" else "no" in
	   let ifs = Bridge.intf_list bridge in
	   let first_if = if ifs = [] then "" else List.hd ifs in
	   print bridge id stp first_if;
	   if ifs <> [] then List.iter (print "" "" "") (List.tl ifs)
	) all
  | "addbr" ->
      if Array.length Sys.argv <> 3 then usage_and_quit ();
      let bridge = Sys.argv.(2) in
      Bridge.add bridge
  | "delbr" ->
      if Array.length Sys.argv <> 3 then usage_and_quit ();
      let bridge = Sys.argv.(2) in
      Bridge.del bridge
  | "addif" ->
      if Array.length Sys.argv <> 4 then usage_and_quit ();
      let bridge = Sys.argv.(2) in
      let iface = Sys.argv.(3) in
      Bridge.intf_add bridge iface
  | "delif" ->
      if Array.length Sys.argv <> 4 then usage_and_quit ();
      let bridge = Sys.argv.(2) in
      let iface = Sys.argv.(3) in
      Bridge.intf_del bridge iface
  | _ -> 
      eprintf "Unknown command";
      exit 1
	      
