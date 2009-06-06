(* The ref_index keeps an index of references -> name_label/uuid.
   This data is accessible only on the pool master, since the data all lives there.
   Unlike the "first-class" db calls, these are not marshalled over the wire if called
   on a slave -- so don't ever call them on slaves! :) *)

  (* lookups are not serialised wrt dbcache mutex since they come into this
     module directly. Hence need separate mutex here. *)
open Pervasiveext

let ref_index_mutex = Mutex.create()

(* Keep track of all references, and which class a reference belongs to: *)
type indexrec = {name_label:string option; uuid: string; _ref:string }
let ref_cache : (string,indexrec) Hashtbl.t = Hashtbl.create 100
let uuid_cache : (string,indexrec) Hashtbl.t = Hashtbl.create 100

let string_of (x: indexrec) = 
  Printf.sprintf "%s%s" x.uuid (default "" (may (fun name -> Printf.sprintf " (%s)" name) x.name_label))

let insert indexrec =
  Threadext.Mutex.execute ref_index_mutex 
    (fun ()->
       Hashtbl.replace ref_cache indexrec._ref indexrec;
       Hashtbl.replace uuid_cache indexrec.uuid indexrec)

let _internal_lookup key = 
  if Hashtbl.mem ref_cache key 
  then Hashtbl.find ref_cache key
  else Hashtbl.find uuid_cache key

let remove key =
  Threadext.Mutex.execute ref_index_mutex 
    (fun ()->
       let x = _internal_lookup key in
       Hashtbl.remove ref_cache x._ref;
       Hashtbl.remove uuid_cache x.uuid)

let update_name_label key new_label =
  Threadext.Mutex.execute ref_index_mutex
    (fun ()->
       try
	 let irec = { (Hashtbl.find ref_cache key) with name_label = Some new_label} in
	 Hashtbl.replace ref_cache irec._ref irec;
	 Hashtbl.replace uuid_cache irec.uuid irec
       with _ -> ())

let update_uuid key new_uuid =
  Threadext.Mutex.execute ref_index_mutex
    (fun () ->
       try
	 let irec = Hashtbl.find ref_cache key in
	 let old_uuid = irec.uuid in
	 let newrec = {irec with uuid=new_uuid} in
	 Hashtbl.replace ref_cache newrec._ref newrec;
	 Hashtbl.remove uuid_cache old_uuid;
	 Hashtbl.replace uuid_cache newrec.uuid newrec
       with _ -> ())
	
let lookup key =
  Threadext.Mutex.execute ref_index_mutex 
    (fun ()->try (Some (_internal_lookup key)) with _ -> None)
