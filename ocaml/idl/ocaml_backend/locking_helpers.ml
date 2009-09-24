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
open Threadext
open Pervasiveext

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Debugger(struct let name = "locking_helpers" end)
open D

(* We store the locks in a hashtable whose keys are VM references and values are abstract
   locking tokens. We can check we are the current lock owner by calling "assert_locked" with
   the token -- this helps us avoid screwups like the following:
      let f () () = ... do something which needs a lock ...
      let result = with_lock f () in
      (* result is a higher order function *)
      result () (* oops: side-effect happens here outside the lock *)
*)

let mutex = Mutex.create ()
let cvar = Condition.create ()
type token = int64 (* abstract *)

let locks : (API.ref_VM, token) Hashtbl.t = Hashtbl.create 10

exception Lock_not_held (** Raised by assert_locked if we screw up the use of 'with_lock' *)

let assert_locked (target: API.ref_VM) (token: token) =
  let held = 
    Mutex.execute mutex
      (fun () -> Hashtbl.fold (fun target' token' acc -> acc || ((target = target') && (token = token'))) locks false) in
  if not held then begin
    error "VM lock not held for VM %s" (Ref.string_of target);
    raise Lock_not_held
  end


let next_token = ref 0L

let acquire_lock (target: API.ref_VM)  =
  let already_locked () = Hashtbl.mem locks target in

  let token = Mutex.execute mutex
    (fun () ->
       while already_locked () do Condition.wait cvar mutex done;
       let token = !next_token in
       next_token := Int64.add !next_token 1L;
       Hashtbl.replace locks target token;
       token
    ) in
  debug "Acquired lock on VM %s with token %Ld" (Ref.string_of target) token;
  token

(* Two internal errors which should never happen by construction *)
exception Internal_error_vm_not_locked
exception Internal_error_token_mismatch

let release_lock (target: API.ref_VM) (token: token) = 
  Mutex.execute mutex
    (fun () ->
       if not(Hashtbl.mem locks target) then begin
	 error "VM %s not locked at all: lock cannot be released" (Ref.string_of target);
	 raise Internal_error_vm_not_locked
       end;
       let token' = Hashtbl.find locks target in
       if token <> token' then begin
	 error "VM %s locked with token %Ld: cannot be unlocked by token %Ld" (Ref.string_of target) token' token;
	 raise Internal_error_token_mismatch
       end;
       Hashtbl.remove locks target;
       Condition.broadcast cvar
    );
  debug "Released lock on VM %s with token %Ld" (Ref.string_of target) token
      
(** Execute (f x) with lock held *)
let with_lock_only (target : API.ref_VM) f x =
  let token = acquire_lock target in
  finally
    (fun () -> f token x)
    (fun () -> release_lock target token)

exception Sanity_check_failed
(* Perform a primitive sanity check  *)
let sanity_check () = 
  let vm = Ref.of_string "locking_helpers_test" in
  (* leak token *)
  let token = with_lock_only vm (fun token () -> token) () in
  try
    assert_locked vm token;
    raise Sanity_check_failed
  with Lock_not_held -> ()

(* ******************************************************************************************************** *)

(* To avoid the scenario where a long_running_op (like suspend) generates an event (like a domain shutdown)
   which clogs up the vm_lifecycle_op queue (otherwise full of reboots) we have a convention where, when
   an API thread is already servicing a VM, it will create a specific queue of thunks for that VM and 
   drain it itself before it returns. This means that the main API thread will service *most* but not all
   of the events it is responsible for generating. Crucially it only misses events which happen when it's
   about to finish anyway and the long running part of the operation has finished. *)

module Per_VM_Qs = struct
  let m = Mutex.create ()
  let qs = Hashtbl.create 10 

  (** Add a new per-VM queue for the given VM. *)
  let add (vm: API.ref_VM) = 
    Mutex.execute m
      (fun () ->
	 (* should never happen: *)
	 if Hashtbl.mem qs vm then failwith (Printf.sprintf "Per_VM_Q already exists for %s" (Ref.string_of vm));
	 let q = Queue.create () in
	 Hashtbl.replace qs vm q
      )
  (** Remove the per-VM queue for the given VM and return it for draining to the caller *)
  let remove (vm: API.ref_VM) = 
    Mutex.execute m
      (fun () ->
	 if not (Hashtbl.mem qs vm) then failwith (Printf.sprintf "Per_VM_Q does not exist for %s" (Ref.string_of vm));
	 let q = Hashtbl.find qs vm in
	 Hashtbl.remove qs vm;
	 q
      )
  (** Execute all the thunks on the given per-VM queue *)
  let drain token q = 
    (* I suppose we could convert this function to point-free style :) *)
    Queue.iter 
      (fun (description, x) -> 
	 debug "pop(per-VM queue) = %s" description;
	 try x token with e -> debug "Ignoring exception: %s" (ExnHelper.string_of_exn e)
      ) q

  let with_per_vm_q (vm: API.ref_VM) token f x = 
    (* Must already have the VM locked otherwise two people might try to add separate per-VM queues *)
    assert_locked vm token;
    add vm;
    finally 
      (fun () -> f token x)
      (fun () -> drain token (remove vm))

  let string_of_queue q = 
    let items = List.rev (Queue.fold (fun acc (description, _) -> description::acc) [] q) in
    Printf.sprintf "[ %s ](%d)" (String.concat "; " items) (List.length items) 

  let maybe_push (vm: API.ref_VM) description thunk = 
    Mutex.execute m
      (fun () ->
	 if Hashtbl.mem qs vm then begin
	   let q = Hashtbl.find qs vm in
	   Queue.push (description, thunk) q;
	   debug "push(per-VM queue, %s); queue = %s" description (string_of_queue q);
	   true
	 end else false)
end


let with_lock (target : API.ref_VM) f x = with_lock_only target (fun token x -> Per_VM_Qs.with_per_vm_q target token f x) x
