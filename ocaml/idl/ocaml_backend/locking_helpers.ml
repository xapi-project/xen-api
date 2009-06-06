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
let with_lock (target : API.ref_VM) f x =
  let token = acquire_lock target in
  finally
    (fun () -> f token x)
    (fun () -> release_lock target token)

exception Sanity_check_failed
(* Perform a primitive sanity check  *)
let sanity_check () = 
  let vm = Ref.of_string "locking_helpers_test" in
  (* leak token *)
  let token = with_lock vm (fun token () -> token) () in
  try
    assert_locked vm token;
    raise Sanity_check_failed
  with Lock_not_held -> ()
