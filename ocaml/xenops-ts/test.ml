(*
 * Copyright (c) 2007 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * Xenops test suite
 *)

open Printf
open Pervasiveext
open Stringext
open Threadext

(** xenvm helper *)
type domain_config = {
	name: string;
	output: string;
	no_mem_check: bool;
	kernel: string;
	cmdline: string;
	initrd: string;
	disks: (string * Device.Vbd.physty * string
	       * Device.Vbd.mode * Device.Vbd.devty) list;
	vifs: string list;
	mem: int;
	vcpus: int;
	hvm: bool;
}

type status = Running | Rebooting | Suspended | Paused

let status_of_string = function
	| "running"   -> Running
	| "suspended" -> Suspended
	| "paused"    -> Paused
	| "rebooting" -> Rebooting
	| _           -> failwith "unknown status"

let xenvm_cmd identifier s =
	match Xenvmlib.request identifier s with
	| Xenvmlib.Ok          -> None
	| Xenvmlib.Error error -> failwith (sprintf "error xenvm-cmd: %s" error)
	| Xenvmlib.Msg msg     -> Some msg
	| Xenvmlib.Unknown s   -> failwith (sprintf "error unknown reply: %s" s)

let xenvm_start config =
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let config_to_kvxenvmcfg cfg =
		("debug", "true") ::
		("output", cfg.output) ::
		("no_mem_check", string_of_bool cfg.no_mem_check) ::
		("uuid", uuid) ::
		("name", cfg.name) ::
		("hvm", string_of_bool cfg.hvm) ::
		("kernel", cfg.kernel) ::
		("initrd", cfg.initrd) ::
		("memory", string_of_int cfg.mem) ::
		("vcpus", string_of_int cfg.vcpus) ::
		("cmdline", cfg.cmdline) ::
		List.map (fun (physpath,physty,virtpath,mode,devtype) ->
			"disk", sprintf "%s:%s:%s:%s:%s"
			                physpath (Device.Vbd.string_of_physty physty)
			                virtpath (Device.Vbd.string_of_mode mode)
			                (Device.Vbd.string_of_devty devtype)) cfg.disks @
		List.map (fun vifstr -> "vif", vifstr) cfg.vifs
		in

	let write_kv fd k v =
		let s = sprintf "%s = %s\n" k v in
		ignore (Unix.write fd s 0 (String.length s));
		in
	(* create the config file *)
	let file = sprintf "/tmp/xenvm-config-%d" (Random.int 10000) in
	let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o640 in
	finally (fun () ->
		List.iter (fun (k, v) -> write_kv fd k v) (config_to_kvxenvmcfg config)
	) (fun () -> Unix.close fd);

	(* start xenvm *)
	let cmdarg = [| "xenvm"; file |] in
	let pid = Unix.fork () in
	if pid = 0 then (
		finally (fun () -> Unix.execvp cmdarg.(0) cmdarg)
			(fun () -> exit 127)
	);
	Unix.sleep 1;
	pid, uuid

let xenvm_stop pid =
	match snd (Unix.waitpid [] pid) with
	| Unix.WEXITED 0    -> ()
	| Unix.WEXITED rc   -> failwith (sprintf "xenvm exited with rc = %d" rc)
	| Unix.WSIGNALED si -> failwith (sprintf "xenvm killed si = %d" si)
	| Unix.WSTOPPED i   -> failwith (sprintf "xenvm stopped %d" i)

let xenvm_force_stop identifier pid =
	ignore (xenvm_cmd identifier "destroy");
	xenvm_stop pid

let xenvm_getdomid identifier =
	match Xenvmlib.request identifier "domid" with
	| Xenvmlib.Msg msg -> int_of_string msg
	| _                -> failwith "getdomid failed: reply is not what expected"

let xenvm_status identifier =
	match Xenvmlib.request identifier "status" with
	| Xenvmlib.Msg msg -> status_of_string msg
	| _                -> failwith "getstatus failed: reply is not what expected"

let assert_bool b =
	if b then () else failwith "boolean assertion failed"

let skip () = failwith "test skipped"

let with_xc f =
	let xc = Xc.interface_open () in
	finally (fun () -> f xc) (fun () -> Xc.interface_close xc)

(** tests *)
let test_dmesg () =
	let xc = Xc.interface_open () in
	finally (fun () ->
		let s = Xc.readconsolering xc in
		let ls = String.split '\n' s in
		let found = ref false in
		List.iter (fun s ->
			if String.startswith "(XEN)" s then found := true) ls;
		if not !found then
			failwith "no \"^(XEN)\" line found"
	) (fun () -> Xc.interface_close xc)

let test_start cfg () =
	let pid, uuid = xenvm_start cfg in
	xenvm_force_stop uuid pid

let wait_running uuid =
	let timeout = ref 10 in
	while xenvm_status uuid <> Running && !timeout > 0
	do
		Unix.sleep 1;
		decr timeout
	done;
	if !timeout = 0 then
		failwith "domain wait running timeout"

let test_shutdown cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	ignore (xenvm_cmd uuid "shutdown");
	xenvm_stop pid

let test_reboot cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	let domid = xenvm_getdomid uuid in
	ignore (xenvm_cmd uuid "reboot");
	if xenvm_status uuid <> Rebooting then
		failwith "domain is not rebooting";
	wait_running uuid;
	let newdomid = xenvm_getdomid uuid in
	printf "reboot: old %d new %d\n" domid newdomid;
	if domid = newdomid then
		failwith "domid still the same after reboot";
	ignore (xenvm_cmd uuid "shutdown");
	xenvm_stop pid


let test_pause cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	try
		ignore (xenvm_cmd uuid "pause");
		if xenvm_status uuid <> Paused then
			failwith "domain didn't paused";
		ignore (xenvm_cmd uuid "unpause");
		if xenvm_status uuid <> Running then
			failwith "domain didn't unpaused";
		ignore (xenvm_cmd uuid "shutdown");
		xenvm_stop pid
	with exn ->
		xenvm_force_stop uuid pid;
		raise exn

let test_suspend cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	Unix.sleep 5;
	try
		ignore (xenvm_cmd uuid "suspend /tmp/suspend_image");
		if xenvm_status uuid <> Suspended then
			failwith "domain didn't suspend";
		ignore (xenvm_cmd uuid "restore /tmp/suspend_image");
		if xenvm_status uuid <> Running then
			failwith "domain didn't restore";
		ignore (xenvm_cmd uuid "shutdown");
		xenvm_stop pid
	with exn ->
		xenvm_force_stop uuid pid;
		raise exn

let test_chkpoint cfg () = skip ()

let test_parallel nb cfg () =
	let vms = ref [] in
	for i = 0 to nb - 1
	do
		let cfg_vm = { cfg with name = cfg.name ^ string_of_int i } in
		try
			let pid, uuid = xenvm_start cfg in
			vms := (i, pid, uuid, cfg_vm) :: !vms
		with _ -> ()
	done;

	let bootedvms = List.fold_left (fun acc (i, pid, uuid, cfg_vm) ->
		try
			wait_running uuid;
			(i, pid, uuid, cfg_vm) :: acc
		with _ ->
			begin try xenvm_force_stop uuid pid with _ -> () end;
			acc) [] !vms in
	List.iter (fun (i, pid, uuid, cfg_vm) -> ignore (xenvm_cmd uuid "shutdown")) bootedvms;
	List.iter (fun (i, pid, uuid, cfg_vm) -> xenvm_stop pid) bootedvms;
	()

let test_really_parallel nb cfg () =
	let cfgs = ref [] in
	let vms_mutex = Mutex.create () in
	let sync_mutex = Mutex.create () in
	let vms = ref [] in
	for i = 0 to nb - 1
	do
		cfgs := { cfg with name = cfg.name ^ string_of_int i } :: !cfgs
	done;

	let start_vm cfg =
		Mutex.execute sync_mutex (fun () -> ());
		let pid, uuid = xenvm_start cfg in
		Mutex.execute vms_mutex (fun () -> vms := (pid, uuid, cfg) :: !vms)
		in

	Mutex.lock sync_mutex;
	let ths = List.map (fun cfg -> Thread.create start_vm cfg) !cfgs in
	Mutex.unlock sync_mutex;

	List.iter Thread.join ths;

	let bootedvms = List.fold_left (fun acc (pid, uuid, cfg_vm) ->
		try
			wait_running uuid;
			(pid, uuid, cfg_vm) :: acc
		with _ ->
			begin try xenvm_force_stop uuid pid with _ -> () end;
			acc) [] !vms in
	List.iter (fun (pid, uuid, cfg_vm) -> ignore (xenvm_cmd uuid "shutdown")) bootedvms;
	List.iter (fun (pid, uuid, cfg_vm) -> xenvm_stop pid) bootedvms;
	()

let test_sched cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	try
		let domid = xenvm_getdomid uuid in
		with_xc (fun xc ->
			let ctrl = Xc.sched_credit_domain_get xc domid in
			let newctrl = { ctrl with Xc.weight = ctrl.Xc.weight * 2 } in
			Xc.sched_credit_domain_set xc domid newctrl
		);
		ignore (xenvm_cmd uuid "shutdown");
		xenvm_stop pid
	with exn ->
		xenvm_force_stop uuid pid;
		raise exn

let test_pinning cfg () =
	let pid, uuid = xenvm_start cfg in
	wait_running uuid;
	try
		let domid = xenvm_getdomid uuid in
		with_xc (fun xc ->
			let a = Domain.vcpu_affinity_get ~xc domid 0 in
			a.(1) <- false;
			Domain.vcpu_affinity_set ~xc domid 0 a
		);
		ignore (xenvm_cmd uuid "shutdown");
		xenvm_stop pid
	with exn ->
		xenvm_force_stop uuid pid;
		raise exn

let all = [
	"dmesg", test_dmesg;
]

let all_dom = [
	"domain starting", test_start;
	"domain shutdowning", test_shutdown;
	(* "domain reboot", test_reboot; *)
	"domain pause", test_pause;
	"domain suspend", test_suspend;
	"domain chkpoint", test_chkpoint;
	"4 dom // building", test_parallel 4;
	"16 dom // building", test_parallel 16;
	(*
	"4 dom really //", test_really_parallel 4;
	"10 dom really //", test_really_parallel 10;
	*)
	"domain cpu sched", test_sched;
	"domain cpu pinning", test_pinning;
]
