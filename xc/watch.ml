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

open Xenops_utils
module D = Debug.Make(struct let name = "watch" end)
open D

open Xenstore

type path = string

exception Timeout of float

(** Represents a condition to wait for *)
type 'a t = { evaluate: Xs.xsh -> 'a }

let has_fired ~xs (x: 'a t) =
  try
    x.evaluate xs;
    true
  with Xs_protocol.Eagain ->
    false
    
let map f x = { evaluate = fun xs -> f (x.evaluate xs) }
  
(** Block waiting for a result *)
let wait_for ~xs ?(timeout=300.) (x: 'a t) =
  let task = Xs.wait x.evaluate in
  let cancel = Scheduler.(one_shot (Delta (int_of_float timeout)) "cancel watch" (fun () -> Xs_client_unix.Task.cancel task)) in
  try
	let result = Xs_client_unix.Task.wait task in
    Scheduler.cancel cancel;
    result
  with Xs_client_unix.Cancelled ->
    error "watch: timeout while watching xenstore after %f seconds" timeout;
    raise (Timeout timeout)

(** Wait for a node to appear in the store and return its value *)
let value_to_appear (path: path): string t = 
  { evaluate = fun xs -> 
      try 
        let v = xs.Xs.read path in
        v
      with Xs_protocol.Enoent _ -> raise Xs_protocol.Eagain
  }

(** Wait for a node to disappear from the store *)
let key_to_disappear (path: path) : unit t = 
  { evaluate = fun xs -> 
      try
	    ignore(xs.Xs.read path); raise Xs_protocol.Eagain 
      with Xs_protocol.Enoent _ ->
        () 
  }

(** Wait for a node to appear with a particular value *)
let value_to_become (path: path) (v: string) : unit t = 
  { evaluate = fun xs -> 
      try 
        if xs.Xs.read path <> v
	    then raise Xs_protocol.Eagain
      with Xs_protocol.Enoent _ -> raise Xs_protocol.Eagain
  }

(** Wait for a set of conditions simultaneously. Note when any watch fires we
    re-evaluate everything which isn't necessarily the most efficient thing to do. *)
let all_of (watches: 'a t list) = 
  { evaluate = fun xs ->
      List.map (fun x -> x.evaluate xs) watches
  }

(** Wait for any of a set of tagged conditions to become true. Return the tag of 
    the first condition and the result. *)
let any_of (watches: ('a * 'b t) list) = 
  let rec check_each xs = function
    | (tag, x) :: rest -> 
      begin
        try
          let r = x.evaluate xs in
          (tag, r) (* tag and value *)
        with Xs_protocol.Eagain ->
          check_each xs rest
      end
    | [] -> raise Xs_protocol.Eagain in
  { evaluate = fun xs -> check_each xs watches }

