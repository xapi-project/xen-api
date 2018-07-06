open Xenlight
open Xenstore
open Xenops_helpers

let write_private backend_domid frontend_domid devid private_list =
  with_xs (fun xs ->
      (*	let private_data_path = Hotplug.get_private_data_path_of_device device in*)
      let private_data_path = Printf.sprintf "/xapi/%d/private/vif/%d" frontend_domid devid in
      Xs.transaction xs (fun t ->
          t.Xst.mkdir private_data_path;
          t.Xst.setperms private_data_path (backend_domid, Xsraw.PERM_NONE, []);
          t.Xst.writev private_data_path
            (("backend-kind", "vif") :: ("backend-id", string_of_int backend_domid) :: private_list);
        )
    )

(*
let rec wait_for_events ctx =
	let open Event in
	print_endline "waiting for next event";
	let ty = match event.ty with
		| Domain_shutdown _ -> "domain shutdown"
		| Domain_death -> "domain death"
		| Disk_eject _ -> "disk eject"
		| Operation_complete _ -> "operation complete"
		| Domain_create_console_available -> "domain create console available"
	in
	Printf.printf "got event: %s\n" ty
*)

let test ctx =
 (*
	let physinfo = Physinfo.get ctx in
	Printf.printf "threads_per_core: %ld\n" physinfo.Physinfo.threads_per_core;
	print_endline "done.";

	let print_nic nic =
		let open Device_nic in
		let mac = String.concat ":" (Array.to_list (Array.map (fun n -> Printf.sprintf "%02x" n) nic.mac)) in
		Printf.printf "\tNIC %d | MAC: %s, bridge: %s, backend_domid: %d\n" nic.devid mac nic.bridge nic.backend_domid
	in

	let print_dominfo info =
		let open Dominfo in
		let hvm = match info.domain_type with
			| DOMAIN_TYPE_INVALID -> "unknown"
			| DOMAIN_TYPE_HVM -> "HVM"
			| DOMAIN_TYPE_PV -> "PV"
		in
		Printf.printf "domid %d (%s) [%s] | current: %Ld, shared: %Ld, paged: %Ld, max: %Ld\n"
			info.domid hvm info.uuid info.current_memkb info.shared_memkb info.paged_memkb info.max_memkb;

		let nics = Device_nic.list ctx info.domid in
		List.iter print_nic nics
	in
	let dominfo_list = Dominfo.list ctx in
	List.iter print_dominfo dominfo_list;

	ignore (Thread.create print_endline "\n\ntest Dominfo.get");
	print_dominfo (Dominfo.get ctx 0);
	*)

  (* let event_thread = Lwt_preemptive.detach Xenlight.event_loop ctx in *)
  let _ = Xenlight_events.event_loop_lwt ctx in

  (* print_endline "creating a NIC\n"; *)
  Lwt_io.printl "creating a NIC" >>= fun () ->

  let open Xenlight.Device_nic in
  let nic = {
    backend_domid=0; devid=0; mtu=1500; model = ""; mac=[|0x0a;0x7a;0xb8;0xf3;0x3d;0x8d|]; ip = ""; bridge="xenbr0"; ifname = "";
    script="/etc/xensource/scripts/vif"; nictype=Xenlight.NIC_TYPE_VIF; rate_bytes_per_interval=0L; rate_interval_usecs=0l;
  } in
  write_private 0 52 0 ["vif-uuid", "e72759bd-4788-582e-23ee-ff448119a38f"; "network-uuid", "cf449e61-9a3f-e8d2-8da6-483b0127632d";
                        "locking-mode", "unlocked"];
  Xenlight_events.device_nic_add_async ctx nic 52 >>= fun r ->
  let t = Lwt_io.printlf "result: %d" r in

  evenable_domain_death ctx 52 0;
  Lwt.join [t]


let _ =
  let logger = Xentoollog.create_stdio_logger ~level:Xentoollog.Debug () in
  let ctx = ctx_alloc logger in
  Lwt_main.run (test ctx)

