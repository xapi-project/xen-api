(** Manage the lifecycle of HA metadata VDI *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

open Client
open Listext
open Stringext

(** Make sure we have plenty of room for the database *)
let minimum_size =
  let ( ** ) = Int64.mul in
  let mib = 1024L ** 1024L in
  256L ** mib

let create ~__context ~sr = 
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       Client.VDI.create ~rpc ~session_id
	 ~name_label:"Metadata for HA" 
	 ~name_description:"Used for master failover"
         ~sR:sr ~virtual_size:minimum_size ~_type:`metadata
         ~sharable:true ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:Xha_statefile.statefile_sm_config ~tags:[]
    )

(** Return a reference to a valid metadata VDI in the given SR.
    This function prefers to reuse existing VDIs to avoid leaking the VDI when HA is disabled without statefile access. *)
let find_or_create ~__context ~sr = 
  match
  List.filter 
    (fun self -> true
       && (Db.VDI.get_type ~__context ~self = `metadata)
       && (Db.VDI.get_virtual_size ~__context ~self >= minimum_size))
    (Db.SR.get_VDIs ~__context ~self:sr) with
    | x :: _ ->
	info "re-using existing metadata VDI: %s" (Db.VDI.get_uuid ~__context ~self:x);
	x
    | [] ->
	info "no suitable existing metadata VDI found; creating a fresh one";
	create ~__context ~sr


let list_existing () = 
  List.filter (fun x -> x.Static_vdis.reason = Xapi_globs.metadata_vdi_reason) (Static_vdis.list ()) 

(** Detach all statefiles attached with reason, to clear stale state *)
let detach_existing ~__context = 
  let vdis = list_existing() in
  List.iter (fun x -> Static_vdis.permanent_vdi_detach_by_uuid ~__context ~uuid:x.Static_vdis.uuid) vdis

open Pervasiveext

(** Attempt to flush the database to the metadata VDI *)
let flush_database ~__context = 
  try
    Backend_xml.flush_db_to_redo_log Db_backend.cache;
    true
  with _ -> false
