(* Some functions to record vdi_activates in the local database such that, in the
   event of a pool-master change (and possible subsequent reversion of the database)
   we can resynchronise the VDI.sm_config fields relating to attachment *)

(* Note that when the SM backends can manage this themselves, this code should be
   removed *)

(* We could augment this to include attachments as well as activations. This could then
   be used as input to the refcounting code, to help prevent a leaked attachment *)

open Stringext
open Threadext

module StringSet = Set.Make(String)

module D=Debug.Debugger(struct let name="xapi_local_vdi_state" end)
open D

(* The local database keys to use *)
let key_ro = "vdi_activations_ro"
let key_rw = "vdi_activations_rw"

(* Perform all mutations holding this lock *)
let lock = Mutex.create () 

(* Marshalling/unmarshalling *)
let set_of_string str = 
	let t x = match String_unmarshall_helper.set (fun x -> x) x with
		| [ r ] -> r
		| _ -> failwith (Printf.sprintf "Failed to parse VDI location info from local database: %s" x) in
	try 
		let list = String_unmarshall_helper.set t str in
		List.fold_left (fun set elt -> StringSet.add elt set) StringSet.empty list
	with e ->
		error "Unexpected error in Xapi_local_vdi_state.set_of_string: %s" (Printexc.to_string e);
		StringSet.empty

let string_of_set set =
	let list = StringSet.fold (fun elt list -> elt::list) set [] in 
	let t x = String_marshall_helper.set (fun x -> x) [ x ] in
	String_marshall_helper.set t list

(* Helper function *)
let mutate rw f =
	let key = if rw then key_rw else key_ro in
	Mutex.execute lock (fun () ->	
		let cur = Localdb.get_with_default key "()" in
		let set = set_of_string cur in
		let newset = f set in
		let value = string_of_set newset in
		Localdb.put key value)

(* Exposed functions *)
let activate location rw =
	mutate rw (fun set -> StringSet.add location set)

let deactivate location =
	mutate true (fun set -> StringSet.remove location set);
	mutate false (fun set -> StringSet.remove location set)

let clear () =
	mutate true (fun _ -> StringSet.empty);
	mutate false (fun _ -> StringSet.empty)

let iter f =
	let (set_ro, set_rw) = Mutex.execute lock (fun () ->
		let get key = 
			let cur = Localdb.get_with_default key "()" in 
			set_of_string cur 
		in
		(get key_ro, get key_rw)
	) in
	StringSet.iter (f false) set_ro;
	StringSet.iter (f true) set_rw

let fold f x =
	let (set_ro, set_rw) = Mutex.execute lock (fun () -> 
		let get key = 
			let cur = Localdb.get_with_default key "()" in
			set_of_string cur
		in
		(get key_ro, get key_rw)
	) in
	StringSet.fold (f true) set_rw (StringSet.fold (f false) set_ro x)
