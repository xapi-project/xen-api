(** Common code between the fake and real servers for dealing with Hosts *)

(* (C) XenSource 2006-2007 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

open Db_filter
open Record_util (* for host_operation_to_string *)
open Threadext

let all_operations = [ `provision; `evacuate; `reboot; `shutdown;
		       `vm_start; `vm_resume; `vm_migrate; `power_on ]

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' = 
  let _ref = Ref.string_of _ref' in
  let current_ops = List.map snd record.Db_actions.host_current_operations in

  let table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_operations;
  let set_errors (code: string) (params: string list) (ops: API.host_allowed_operations_set) =
    List.iter (fun op ->
		 if Hashtbl.find table op = None
		 then Hashtbl.replace table op (Some(code, params))) ops in

  (* evacuate, reboot and shutdown are all exclusive *)
  let exclusive : API.host_allowed_operations_set = [ `evacuate; `reboot; `shutdown ] in
  (* If an exclusive operation is in progress then nothing else may happen
     except power_on which we handle separately *)
  if List.fold_left (fun acc op -> acc || (List.mem op exclusive)) false current_ops
  then set_errors Api_errors.other_operation_in_progress
    [ "host"; _ref; host_operation_to_string (List.hd current_ops) ] (List.filter (fun x -> x <> `power_on) all_operations);
  (* If any operation is in progress then the exclusive ones must be blocked *)
  if List.length current_ops > 0
  then set_errors Api_errors.other_operation_in_progress
    [ "host"; _ref; host_operation_to_string (List.hd current_ops) ]
    exclusive;

  (* Prevent more than one provision happening at a time to prevent extreme dom0
     load (in the case of the debian template). Once the template becomes a 'real'
     template we can relax this. *)
  if List.mem `provision current_ops
  then set_errors Api_errors.other_operation_in_progress
    [ "host"; _ref; host_operation_to_string `provision ]
    [ `provision ];

  (* The host must be disabled before reboots or shutdowns are permitted *)
  if record.Db_actions.host_enabled
  then set_errors Api_errors.host_not_disabled [] [ `reboot; `shutdown ];

  (* The host must be (thought to be down) before power_on is possible *)
  begin 
    try 
      if Db.Host_metrics.get_live ~__context ~self:record.Db_actions.host_metrics 
      then set_errors Api_errors.host_is_live [ _ref ] [ `power_on ] 
    with _ -> () 
  end;
  (* The power-on-host plugin must be available before power_on is possible *)
  begin 
    try Unix.access (Filename.concat Xapi_globs.xapi_plugins_root Constants.power_on_plugin) [ Unix.X_OK ]
    with _ -> set_errors Api_errors.xenapi_missing_plugin [ Constants.power_on_plugin ] [ `power_on ]
  end;

  (* All other operations may be parallelised *)
  table
  
let throw_error table op = 
  if not(Hashtbl.mem table op)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_host_helpers.assert_operation_valid unknown operation: %s" (host_operation_to_string op) ]));

  match Hashtbl.find table op with
  | Some (code, params) -> raise (Api_errors.Server_error(code, params))
  | None -> ()

let assert_operation_valid ~__context ~self ~(op:API.host_allowed_operations) = 
  let all = Db.Host.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op

let update_allowed_operations ~__context ~self : unit =
  let all = Db.Host.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
  (* CA-18377: If there's a rolling upgrade in progress, only send Miami keys across the wire. *)
  let keys = if Helpers.rolling_upgrade_in_progress ~__context
    then Listext.List.intersect keys Xapi_globs.host_operations_miami
    else keys in
  Db.Host.set_allowed_operations ~__context ~self ~value:keys

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.Host.get_current_operations ~__context ~self in
  let set = (fun value -> Db.Host.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

let disable  ~__context ~host = ()

let enable  ~__context ~host = ()

let shutdown  ~__context ~host = ()

let reboot  ~__context ~host = ()

let update_host_metrics ~__context ~host ~memory_total ~memory_free = 
  (* If HA is enabled then we don't set the live flag at all.
     If the node is marked as shutting down then we ignore the heartbeats. *)
  let pool = Helpers.get_pool ~__context in
  let ha_enabled = Db.Pool.get_ha_enabled ~__context ~self:pool in
  let shutting_down =  
    Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
      (fun () -> List.mem host !Xapi_globs.hosts_which_are_shutting_down) in
  let should_set_live = not ha_enabled && not shutting_down in

  let last_updated = Date.of_float (Unix.gettimeofday ()) in
  let m = Db.Host.get_metrics ~__context ~self:host in
  let exists = try ignore(Db.Host_metrics.get_uuid ~__context ~self:m); true with _ -> false in
  if not(exists) then begin
      let r = Ref.make () and uuid = Uuid.to_string (Uuid.make_uuid ()) in
	(* create metrics with live=false, then set live=true; this is because the db semantics for persist=false tables
	   is to persist the creates, but not subsequent writes *)
      Db.Host_metrics.create ~__context ~ref:r ~uuid ~memory_total ~memory_free ~last_updated ~live:false ~other_config:[];
      Db.Host.set_metrics ~__context ~self:host ~value:r;
      if should_set_live then begin
	Db.Host_metrics.set_live ~__context ~self:m ~value:true;
	update_allowed_operations ~__context ~self:host
      end
  end else begin
      Db.Host_metrics.set_memory_total ~__context ~self:m ~value:memory_total;
      Db.Host_metrics.set_memory_free ~__context ~self:m ~value:memory_free;
      Db.Host_metrics.set_last_updated ~__context ~self:m ~value:last_updated;
      if should_set_live then begin
	Db.Host_metrics.set_live ~__context ~self:m ~value:true;
	update_allowed_operations ~__context ~self:host
      end	
  end

(* When the Host.shutdown and Host.reboot calls return to the master, the slave is 
   shutting down asycnronously. We immediately set the Host_metrics.live to false 
   and add the host to the global list of known-dying hosts. *)
let mark_host_as_dead ~__context ~host ~reason =
  Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
    (fun () -> Xapi_globs.hosts_which_are_shutting_down := host :: !Xapi_globs.hosts_which_are_shutting_down);
  (* The heartbeat handling code (HA and non-HA) will hopefully ignore the heartbeats
     and leave the host as dead from now until it comes back with a Pool.hello *)
  Xapi_hooks.host_pre_declare_dead ~__context ~host ~reason;
  begin
    try
      let metrics = Db.Host.get_metrics ~__context ~self:host in
      Db.Host_metrics.set_live ~__context ~self:metrics ~value:false;
      update_allowed_operations ~__context ~self:host
    with e ->
      info "Caught and ignoring exception setting host %s to dead: %s" (Ref.string_of host) (ExnHelper.string_of_exn e)
  end;
  Xapi_hooks.host_post_declare_dead ~__context ~host ~reason



