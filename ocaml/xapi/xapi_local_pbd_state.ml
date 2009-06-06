(** Code to remember local PBD.currently_attached state, so that we can resynchronise quickly over master failover. *)

open Threadext
module D=Debug.Debugger(struct let name="xapi" end)
open D

let m = Mutex.create ()

let get_already_locked () = 
  let attached_string = try Localdb.get Constants.local_currently_attached_pbds with _ -> "()" in
  
  (* Fetch the list of PBDs which we know are already attached to us, even though the database state may differ *)
  try
    let t x = match String_unmarshall_helper.set (fun x -> x) x with
      | [ r ] -> Ref.of_string r
      | _ -> failwith (Printf.sprintf "Failed to parse PBD reference from local database: %s" x) in
    String_unmarshall_helper.set t attached_string
  with e ->
    error "Unexpected error in Xapi_local_pbd_state.get: %s" (Printexc.to_string e);
    [] 

let get () = Mutex.execute m get_already_locked

let modify to_remove to_add = 
  Mutex.execute m
    (fun () ->
       let current = get_already_locked () in
       let with_removed = List.filter (fun x -> not(List.mem x to_remove)) current in
       let new_to_add = List.filter (fun x -> not(List.mem x with_removed)) to_add in
       let updated = with_removed @ new_to_add in
       (* flush the new set to disk *)
       (* Make a string *)
       let t x = String_marshall_helper.set (fun x -> x) [ Ref.string_of x ] in
       Localdb.put Constants.local_currently_attached_pbds (String_marshall_helper.set t updated)
    )

let clear () = Mutex.execute m (fun () -> Localdb.put Constants.local_currently_attached_pbds "()")

let add x = 
  debug "adding PBD %s" (Ref.string_of x);
  try
    modify [] [ x ];
    debug "done"
  with e -> debug "adding PBD error: %s" (Printexc.to_string e)
let remove x = 
  debug "removing PBD %s" (Ref.string_of x);
  modify [ x ] [];
  debug "done"

  
(* Set my PBD.currently_attached fields in the Pool database to match the local one *)
let resynchronise ~__context ~pbds = 
  let should_be_attached_pbds = get () in
  debug "resynchronise_my_pbds: should_be_attached_pbds (%d): %s" (List.length should_be_attached_pbds) (String.concat "; " (List.map Ref.string_of should_be_attached_pbds));

  (* Update the currently_attached flag from every one of my PBDs *)
  List.iter (fun self -> Db.PBD.set_currently_attached ~__context ~self ~value:(List.mem self should_be_attached_pbds)) pbds

