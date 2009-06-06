module D = Debug.Debugger(struct let name = "xapi" end)
open D

(* High-level functions called when rolling upgrade 'starts' and 'stops' where
   start and stop are defined by the logic in db_gc.ml. *)

open Threadext

(* CA-13190 VDI.location fields set by Rio hosts on non-ISO SRs are set incorrectly
   and should all be reset with the uuid values *)
let ca_13190_vdi_location_update () = 
  debug "CA-13190: fixing VDI.location fields across upgrade";
  Server_helpers.exec_with_new_task "upgrading VDI.location fields" 
    (fun __context ->
       Mutex.execute Xapi_sr.scan_upgrade_lock
	 (fun () ->
	    List.iter (fun sr -> match Db.SR.get_content_type ~__context ~self:sr with
		       | "iso" -> ()
		       | _ ->
			   (* all other types need upgrading *)
			   List.iter (fun vdi ->
					let uuid = Db.VDI.get_uuid ~__context ~self:vdi in
					Db.VDI.set_location ~__context ~self:vdi ~value:uuid)
			     (Db.SR.get_VDIs ~__context ~self:sr))
	      (Db.SR.get_all ~__context)
	 )
    )

let start () = 
  ()

let stop () = 
  (* Rio -> Miami (via all versions) *)
  ca_13190_vdi_location_update ()
