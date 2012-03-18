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
(** Helper functions for handling common types of xenstore watches *)

module D = Debug.Debugger(struct let name = "xenops" end)
open D
open Xenstore

type path = string

exception Timeout of float

(** When we wake up we evaluate a condition and either decide to keep waiting
    or return a result *)
type 'a result = 
  | KeepWaiting
  | Result of 'a

let result_map f = function
  | KeepWaiting -> KeepWaiting
  | Result x -> Result (f x)
      
(** Represents a set of paths to watch and a condition to evaluate when any
    of the paths change *)
type 'a t = { paths: path list;
	      evaluate: xs:Xs.xsh -> 'a result }
    
let map f x = { x with evaluate = fun ~xs -> result_map f (x.evaluate ~xs) }
  
(** Block waiting for a result *)
let wait_for ~xs ?(timeout=300.) (x: 'a t) =
  let result = ref None in

  (*let start_time = Unix.gettimeofday () in
  let time_taken () = Unix.gettimeofday () -. start_time in *)

  let callback (path, _) = 
    match x.evaluate ~xs with
    | KeepWaiting -> false
    | Result x -> result := Some x; true in
  try
	let paths = Listext.List.setify x.paths in
    debug "watch: watching xenstore paths: [ %s ] with timeout %f seconds" (String.concat "; " paths) timeout;
    (* If the list of paths is empty then we don't receive /any/ watch events and so
       we'll always block until the timeout. If the condition depends on no xenstore paths
       then we can consider it to be vacuously true (or a bug if the condition fn evaluates
       to false). Note this code is required in order to migrate diskless VMs: CA-15011 *)
    if x.paths = [] 
    then ignore(callback ("/", "X"))
    else Xs.monitor_paths xs (List.map (fun path -> path, "X") paths) timeout callback;
      begin match !result with
      | Some x -> 
	  x
      | None -> 
	  (* should never happen *) 
	  failwith "internal error in Watch.wait: perhaps the list of paths was empty but the condition reports false?"
      end
  with Xs.Timeout -> 
    error "watch: timeout while watching xenstore after %f seconds" timeout;
    (* Extra debugging to see if we've missed a watch somewhere *)
    (match x.evaluate ~xs with
     | KeepWaiting -> raise (Timeout timeout)
     | Result y ->
	 warn "watch: timeout after %f but condition has become true: are we unlucky or did we miss a watch?" timeout;
	 y)

(** Wait for a node to appear in the store and return its value *)
let value_to_appear (path: path): string t = 
  { paths = [ path ];
    evaluate = fun ~xs -> 
      try 
	let v = xs.Xs.read path in
	(*debug "watch: value has appeared: %s = %s" path v;*)
	Result v
      with Xenbus.Xb.Noent -> KeepWaiting }

(** Wait for a node to disappear from the store *)
let key_to_disappear (path: path) : unit t = 
  { paths = [ path ];
    evaluate = fun ~xs -> 
      try 
	ignore(xs.Xs.read path); KeepWaiting 
      with Xenbus.Xb.Noent ->
	(*debug "watch: key has disappeared: %s" path;*)
	Result () 
  }

(** Wait for a node to appear with a particular value *)
let value_to_become (path: path) (v: string) : unit t = 
  { paths = [ path ];
    evaluate = fun ~xs -> 
      try 
	if xs.Xs.read path = v 
	then begin
	  (*debug "watch: value has become: %s = %s" path v;*)
	  Result () 
	end else KeepWaiting 
      with Xenbus.Xb.Noent -> KeepWaiting }

(** Wait for a set of conditions simultaneously. Note when any watch fires we
    re-evaluate everything which isn't necessarily the most efficient thing to do. *)
let all_of (watches: 'a t list) = 
  let result_table = Hashtbl.create 10 in
  List.iter (fun watch -> Hashtbl.replace result_table watch None) watches;
  let evaluate_subtask ~xs (x: 'a t) = 
    if Hashtbl.find result_table x = None then match x.evaluate ~xs with
    | KeepWaiting -> ()
    | Result y -> Hashtbl.replace result_table x (Some y) in
  let all_subtasks_completed () = Hashtbl.fold (fun _ result acc -> result <> None && acc) result_table true in
  let make_result_list () = 
    List.map (fun watch -> match Hashtbl.find result_table watch with
	       | Some x -> x
	       | None -> (* should never happen *) failwith "internal error in Watch.all_of") watches in
  { paths = List.concat (List.map (fun x -> x.paths) watches);
    evaluate = fun ~xs ->
      List.iter (fun watch -> evaluate_subtask ~xs watch) watches;
      if all_subtasks_completed ()
      then Result(make_result_list ())
      else KeepWaiting } 

(** Wait for any of a set of tagged conditions to become true. Return the tag of 
    the first condition and the result. *)
let any_of (watches: ('a * 'b t) list) = 
  let rec check_each ~xs = function
    | (tag, x) :: rest -> 
	begin match x.evaluate ~xs with
	| Result r -> Result (tag, r) (* tag and value *)
	| KeepWaiting -> check_each ~xs rest
	end
    | [] -> KeepWaiting in
  { paths = List.concat (List.map (fun (_, x) -> x.paths) watches);
    evaluate = fun ~xs -> check_each ~xs watches }

