open Threadext
open Client
open Lvhdrt_exceptions

let trash_vdi_plugin = "lvhdrt-trash-vdi"
let test_runtime = 60. *. 60. *. 2.

module Background_thread = struct
  type t = {
    m: Mutex.t;
    mutable shutting_down: bool;
    mutable t: Thread.t option;
    mutable iterations: int;
  }
  let make one_iteration_fn = 
    let repeat_until_shutting_down state f = 
      while Mutex.execute state.m (fun () -> not state.shutting_down) do
	begin
	  try
	    f ();
	  with e ->
	    Mutex.execute state.m (fun () -> state.shutting_down <- true);
	    raise e
	end;
	state.iterations <- state.iterations + 1;
      done in
    let state = { m = Mutex.create ();
		  shutting_down = false;
		  t = None;
		  iterations = 0 } in
    let t = Thread.create (fun () -> repeat_until_shutting_down state one_iteration_fn) () in
    state.t <- Some t;
    state
  let stopping state = Mutex.execute state.m (fun () -> state.shutting_down)
  let stop state = 
    Mutex.execute state.m (fun () -> state.shutting_down <- true);
    Opt.iter Thread.join state.t;
    state.t <- None;
    state.iterations
end

let stdout_m = Mutex.create () 

let debug (fmt: ('a , unit, string, unit) format4) =
  (* Convert calendar time, x, to tm in UTC *)
  let of_float x = 
    let time = Unix.gmtime x in
    Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
      (time.Unix.tm_year+1900)
      (time.Unix.tm_mon+1)
      time.Unix.tm_mday
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec in

  Mutex.execute stdout_m
    (fun () ->
       Printf.kprintf (fun s -> Printf.printf "%s [%d] %s\n" (of_float (Unix.gettimeofday ())) (Thread.id (Thread.self ())) s; flush stdout) fmt 
    )

(* Plug a disk into dom0 then unplug and clean up *)
let vbd_plug_unplug rpc session_id vdi () = 
  debug "test: vbd_plug_unplug vdi = %s" (Ref.string_of vdi);
  Utils.with_attached_vdi rpc session_id vdi 
    (fun _ _ -> ()
(*
       debug "sleeping for 5s to give the other thread a chance";
       Thread.delay 5.
*)
    )

let clone_delete rpc session_id vdi () = 
  debug "test: clone_delete vdi = %s" (Ref.string_of vdi);
  let vdi' = Client.VDI.clone rpc session_id vdi [] in
  Client.VDI.destroy rpc session_id vdi'

let snapshot_delete rpc session_id vdi () = 
  debug "test: snapshot_delete vdi = %s" (Ref.string_of vdi);
  let vdi' = Client.VDI.snapshot rpc session_id vdi [] in
  Client.VDI.destroy rpc session_id vdi'

let resize rpc session_id vdi () = 
  debug "test: resize vdi = %s" (Ref.string_of vdi);
  let virtual_size = Client.VDI.get_virtual_size rpc session_id vdi in
  Client.VDI.resize rpc session_id vdi (Int64.add virtual_size 1L)

let scan rpc session_id vdi () = 
  let sr = Client.VDI.get_SR rpc session_id vdi in
  debug "test: scan sr = %s" (Ref.string_of sr);
  Client.SR.scan rpc session_id sr

let pbd_unplug_plug rpc session_id vdi () = 
  let sr = Client.VDI.get_SR rpc session_id vdi in
  debug "test: pbd_unplug_plug sr = %s" (Ref.string_of sr);
  List.iter
    (fun pbd ->
       Client.PBD.unplug rpc session_id pbd;
       Client.PBD.plug rpc session_id pbd)
    (Client.SR.get_PBDs rpc session_id sr)

let suspend_vm rpc session_id vm = 
  let start = Unix.gettimeofday () in
  let limit = 20. *. 60. in
  let finished = ref false in
  while Unix.gettimeofday () -. start < limit && not(!finished) do
    try
      debug "attempting suspend";
      Client.VM.suspend rpc session_id vm;
      finished := true;
    with Api_errors.Server_error(code, params) when code = Api_errors.vm_missing_pv_drivers ->
      debug "test: suspend_vm waiting for PV drivers";
      Thread.delay 1.
  done;
  if not !finished then failwith "Failed to detect PV drivers after %.0f seconds" limit

let vm_start_unstart rpc session_id vm () = 
  debug "test: vm_start_unstart vm = %s" (Ref.string_of vm);
  Client.VM.start rpc session_id vm false false;
  suspend_vm rpc session_id vm;
  Client.VM.resume rpc session_id vm false false;
  Client.VM.hard_shutdown rpc session_id vm

let do_everything rpc session_id vm vdi () = 
  vbd_plug_unplug rpc session_id vdi ();
  clone_delete rpc session_id vdi ();
  snapshot_delete rpc session_id vdi ();
  resize rpc session_id vdi ();
  scan rpc session_id vdi ();
  pbd_unplug_plug rpc session_id vdi ();
  vm_start_unstart rpc session_id vm ()

let total_trashings = ref 0

let trash_untrash_vdi rpc session_id vdi = 
  debug "calling SR.lvhd_stop_using_these_vdis_and_call_script with vdi = %s" (Ref.string_of vdi);
  let vdi_uuid = Client.VDI.get_uuid rpc session_id vdi in
  Thread.delay 0.1;
  try
    match Client.SR.lvhd_stop_using_these_vdis_and_call_script rpc session_id [ vdi ]  trash_vdi_plugin  "main" [ "vdi-uuid", vdi_uuid ] with
    | "LV inactive" ->
	debug "Ignoring glitch: LV currently inactive"
    | "Data restored" ->
	debug "Successful LV manipulation";
	incr total_trashings
  with Api_errors.Server_error(code, params) as e ->
    let pause_and_ignore = List.mem code [ Api_errors.sr_no_pbds (* concurrent PBD.unplug; plug loops *) ] in
    debug "Caught %s [ %s ] %s" code (String.concat "; " params) (if pause_and_ignore then "-- ignoring" else "-- FATAL");
    if pause_and_ignore
    then Thread.delay 5.
    else raise e

let run rpc session =
  Random.self_init();

  (* Make sure the plugin actually exists *)
  begin 
    try
      ignore (Client.SR.lvhd_stop_using_these_vdis_and_call_script rpc session [ ]  trash_vdi_plugin  "main" [ ])
    with 
    | Api_errors.Server_error(code, params) when code = Api_errors.xenapi_missing_plugin ->
	raise (Test_error (Printf.sprintf "ERROR: you must install the plugin %s" trash_vdi_plugin))
    | Api_errors.Server_error(code, params) when code = Api_errors.xenapi_plugin_failure ->
	debug "Precheck: %s plugin detected" trash_vdi_plugin
  end;

  let master = Utils.get_master rpc session in

  (* Find an LVHD SR *)
  let sr = Utils.find_lvhd_sr rpc session in
  debug "Using LVHD SR: %s" (Ref.string_of sr);

  Utils.with_sacrificial_vm rpc session
    (fun vm ->
       debug "Will use VM: %s" (Ref.string_of vm);
       (* Choose one of the VM's disks *)
       let vbds = Client.VM.get_VBDs rpc session vm in
       let nonempty_vbds = List.filter (fun x -> not (Client.VBD.get_empty rpc session x)) vbds in
       if nonempty_vbds = [] then failwith "VM appears to have no non-empty VBDs";
       let vdi = Client.VBD.get_VDI rpc session (List.hd nonempty_vbds) in
       debug "Will use VDI: %s" (Ref.string_of vdi);

       let number_trash_iterations = ref 0 in

       let start = Unix.gettimeofday () in
       let background_loop = Background_thread.make (do_everything rpc session vm vdi) in
       Pervasiveext.finally
	 (fun () ->
	    while Unix.gettimeofday () -. start < test_runtime && not (Background_thread.stopping background_loop) do
	      trash_untrash_vdi rpc session vdi;
	      incr number_trash_iterations;
	      Thread.delay 0.1;
(*
	      Thread.delay 5.
*)
	    done
	 )
	 (fun () ->
	    let iterations = Background_thread.stop background_loop in
	    debug "After %.2f seconds the background thread completed %d iterations and the VDI was stopped %d times" (Unix.gettimeofday () -. start) iterations !number_trash_iterations;
	    debug " of which, %d performed the VHD manipulation" !total_trashings
	 )
    );
  debug "Test passed"
