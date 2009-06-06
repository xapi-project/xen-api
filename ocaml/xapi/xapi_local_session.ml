(** Code to handle local sessions, used so that slaves can communicate even when
    the master is down. *)

type t = { 
  r: API.ref_session;
  pool: bool;
  last_active: Date.iso8601 }

open Threadext

let m = Mutex.create ()
let table = Hashtbl.create 10

let get_all ~__context = Mutex.execute m (fun () -> Hashtbl.fold (fun k v acc -> k :: acc) table [])

let create ~__context ~pool = 
  let r = Ref.make () in
  let session = { r = r; pool = pool; last_active = Date.of_float (Unix.gettimeofday ()) } in
  Mutex.execute m (fun () -> Hashtbl.replace table r session);
  r

let get_record ~__context ~self = 
  Mutex.execute m (fun () -> Hashtbl.find table self)

let destroy ~__context ~self = 
  Mutex.execute m (fun () -> Hashtbl.remove table self)

let local_session_hook ~__context ~session_id = 
  try ignore(get_record ~__context ~self:session_id); true
  with _ -> false
    
