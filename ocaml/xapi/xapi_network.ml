open Threadext
open Xapi_network_types
open Client

module D=Debug.Debugger(struct let name="xapi" end)
open D

open Db_filter

let get_allowed_messages ~__context ~self = []

(* Instantiate the Network (ie bridge) on this host provided it wouldn't 
   destroy existing Networks (e.g. slaves of a bond) in use by something (VIF
   or management interface). 
   Note special-case handling of new management interfaces: we skip the 
   check for the existing management interface (essential otherwise switching
   from a bond slave to a bond master would fail) and we make sure to call
   Nm.bring_pif_up with the management_interface argument so it can make sure
   the default gateway is set up correctly *)
let attach_internal ?(management_interface=false) ~__context ~self () =
  let host = Helpers.get_localhost () in
  let shafted_pifs, local_pifs = 
    Xapi_network_attach_helpers.assert_can_attach_network_on_host ~__context ~self ~host ~overide_management_if_check:management_interface in

  (* Ensure bridge exists and is up *)
  let bridge = Db.Network.get_bridge ~__context ~self in
  let current = Netdev.Bridge.list () in
  if not(List.mem bridge current) then Netdev.Bridge.add bridge;
  if not(Netdev.Link.is_up bridge) then Netdev.Link.up bridge;

  (* Check if we're a guest-installer network: *)
  let other_config = Db.Network.get_other_config ~__context ~self in
  if (List.mem_assoc Xapi_globs.is_guest_installer_network other_config)
    && (List.assoc Xapi_globs.is_guest_installer_network other_config = "true")
  then Xapi_network_real.setup_guest_installer_network ~__context bridge other_config;

  (* Mark shafted PIFs as not currently_attached *)
  List.iter
    (fun pif ->
       let uuid = Db.PIF.get_uuid ~__context ~self:pif in
       debug "Marking PIF as detached: %s" uuid;
       Db.PIF.set_currently_attached ~__context ~self:pif ~value:false)
    shafted_pifs;
  
  (* Create the new PIF.
     NB if we're doing this as part of a management-interface-reconfigure then
     we might be just about to loose our current management interface... *)
  List.iter
    (fun pif ->
       let uuid = Db.PIF.get_uuid ~__context ~self:pif in
       debug "Trying to attach PIF: %s" uuid;
       Nm.bring_pif_up ~__context ~management_interface pif
    ) local_pifs
  

	  
let detach bridge_name = 
  Xapi_network_real.maybe_shutdown_guest_installer_network bridge_name;
  if Netdev.Bridge.exists bridge_name then begin
    List.iter (fun iface ->
		 D.warn "Untracked interface %s exists on bridge %s: deleting" iface bridge_name;
		 Netdev.Link.down iface;
		 Netdev.Bridge.intf_del bridge_name iface
	      ) (Netdev.Bridge.intf_list bridge_name);
    Netdev.Link.down bridge_name;
    Netdev.Bridge.del bridge_name
  end

(** Network.attach external call *)
let attach ~__context ~network ~host = attach_internal ~__context ~self:network ()

let counter = ref 0
let mutex = Mutex.create ()
let stem = "xapi"

let do_bridge_gc rpc session_id =
  let all_networks = Client.Network.get_all_records_where ~rpc ~session_id ~expr:"true" in
  let db_bridge_names = List.map (fun r->r.API.network_bridge) (List.map snd all_networks) in
  let my_bridges = Netdev.Bridge.list () in
    List.iter
      (fun mybridge -> if not (List.mem mybridge db_bridge_names) then detach mybridge)
      my_bridges

let network_gc_func() =
  Server_helpers.exec_with_new_task "network bridge gc"
    (fun __context ->
      let other_config = 
	try
	  let [pool] = Db.Pool.get_all ~__context in
	  Db.Pool.get_other_config ~__context ~self:pool
	with _ -> []
      in
      
      let skip = (List.mem_assoc Xapi_globs.gc_network_disable other_config 
		   && (List.assoc Xapi_globs.gc_network_disable other_config = "true")) in
      
      if not skip then
	Helpers.call_api_functions ~__context
	  (fun rpc session_id ->
	    do_bridge_gc rpc session_id
	  )
      else
	debug "Skipping network GC")
    

(** Internal fn used by slave to create new network records on master during pool join operation *)
let pool_introduce ~__context ~name_label ~name_description ~other_config ~bridge =
  let r = Ref.make() and uuid = Uuid.make_uuid() in
  Db.Network.create ~__context ~ref:r ~uuid:(Uuid.to_string uuid)
    ~current_operations:[] ~allowed_operations:[]
    ~name_label ~name_description ~bridge ~other_config ~blobs:[] ~tags:[];
  r
  
(** Attempt to create a bridge with a unique name *)
let create ~__context ~name_label ~name_description ~other_config ~tags =
	Mutex.execute mutex (fun () ->
		let networks = Db.Network.get_all ~__context in
		let bridges = List.map (fun self -> Db.Network.get_bridge ~__context ~self) networks in
		let rec loop () = 
			let name = stem ^ (string_of_int !counter) in
			incr counter;
			if List.mem name bridges then loop () 
			else 
				let r = Ref.make () and uuid = Uuid.make_uuid () in
				Db.Network.create ~__context ~ref:r ~uuid:(Uuid.to_string uuid)
				  ~current_operations:[] ~allowed_operations:[]
				  ~name_label ~name_description ~bridge:name ~other_config ~blobs:[] ~tags;
				r in
		loop ()) 

(** WARNING WARNING WARNING: called with the master dispatcher lock; do nothing but basic DB calls
    here without being really sure *)
let destroy ~__context ~self =
	let vifs = Db.Network.get_VIFs ~__context ~self in
	let connected = List.filter 
	  (fun self -> 
	     Db.VIF.get_currently_attached ~__context ~self || Db.VIF.get_reserved ~__context ~self
	  ) vifs in
	if connected <> []
	then raise (Api_errors.Server_error (Api_errors.network_contains_vif,List.map Ref.string_of connected));
	let pifs = Db.Network.get_PIFs ~__context ~self in
	if pifs <> [] 
	then (raise (Api_errors.Server_error (Api_errors.network_contains_pif,List.map Ref.string_of pifs)));
	(* destroy all the VIFs now rather than wait for the GC thread. *)
	List.iter (fun vif ->
		     Helpers.log_exn_continue (Printf.sprintf "destroying VIF: %s" (Ref.string_of vif))
		       (fun vif -> Db.VIF.destroy ~__context ~self:vif) vif) vifs;
	Db.Network.destroy ~__context ~self

let create_new_blob ~__context ~network ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.Network.add_to_blobs ~__context ~self:network ~key:name ~value:blob;
  blob
