(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* test GTK UI *)

open Printf
open Stringext

(* default connection parameters *)
let host = ref ""
let host_dest = ref ""
let port = ref 8086
let port_dest = ref 8085
let username = ref "root"
let password = ref "xenroot"
let configfile = ref ""

let last_id = ref 0

let rpc xml =
	Xmlrpcclient.do_xml_rpc ~version:"1.0" ~host:!host ~port:!port ~path:"/" xml

let double_fork f =
	match Unix.fork () with
	| 0 ->
		begin match Unix.fork () with
		| 0 -> f (); exit 0
		| pid -> ignore (Unix.waitpid [] pid); exit 0
		end
	| pid ->
		() (* ignore(Unix.waitpid [] pid) *)

let get_task taskid =
	ignore (taskid)

let execute a =
	match Unix.fork () with
	| 0 -> Unix.execvp a.(0) a
	| _ -> ()

let size_with_suffix n =
	let gigabyte = 1073741824L
	and megabyte = 1048576L
	and kilobyte = 1024L in
	if Int64.compare n gigabyte > 0 then
		sprintf "%.2fGb" ((Int64.to_float n) /. (Int64.to_float gigabyte))
	else
		if Int64.compare n megabyte > 0 then
			sprintf "%.2fMb" (((Int64.to_float n) /. (Int64.to_float megabyte)))
		else
			sprintf "%.2fKb" (((Int64.to_float n) /. (Int64.to_float kilobyte)))

(** Define functions that can be called on remote *)
module Fct = functor(Remote: API.API) -> struct
	let session_id = ref (Ref.make())

	let session_init () =
		let session = Remote.Session.login_with_password ~rpc
		                                         ~uname:!username
		                                         ~pwd:!password in
		session_id := session

	let get_event_id () =
		0
		(*Int64.to_int (Remote.Event.get_latest_id ~session_id:!session_id) *)

	let get_events id1 id2 =
		[]
		(*
		let evs = Remote.Event.get ~session_id:!session_id
		                           ~sid:(Int64.of_int id1)
		                           ~eid:(Int64.of_int id2) in
		List.map (fun e ->
			let l = String.split (fun c -> c = ' ') e in
			List.nth l 0, List.nth l 1, List.nth l 2) evs
		*)

	let get_all_vms () =
		let vms = Remote.VM.get_all ~rpc ~session_id:!session_id in
		vms

	let get_all_srs () =
		let srs = Remote.SR.get_all ~rpc ~session_id:!session_id in
		srs

	let get_all_vdis sr =
		let vdis = Remote.SR.get_VDIs ~rpc ~session_id:!session_id ~self:sr in
		vdis

	let get_vdi_info vdi =
		let name = Remote.VDI.get_name_label ~rpc ~session_id:!session_id ~self:vdi in
		let size = Remote.VDI.get_virtual_size ~rpc ~session_id:!session_id ~self:vdi in
		name, size


	let get_power_state vm : string =
		let state = Remote.VM.get_power_state ~rpc ~session_id:!session_id ~self:vm in
		match state with
		| `Halted       -> "halted"
		| `Paused       -> "paused"
		| `Running      -> "running"
		| `Suspended    -> "suspend"
		| `ShuttingDown -> "shuttingdown"
		| `Migrating    -> "migrating"

	let get_uuid vm =
		Remote.VM.get_uuid ~rpc ~session_id:!session_id ~self:vm

	let get_name vm =
		Remote.VM.get_name_label ~rpc ~session_id:!session_id ~self:vm

	let get_info vm = get_uuid vm, get_name vm, get_power_state vm

	let start vm =
		get_task
			(Remote.Async.VM.start
				~session_id:!session_id
				~start_paused:true
				~vm)

	let unpause vm = Remote.VM.unpause ~rpc ~session_id:!session_id ~vm
	let pause vm = Remote.VM.pause ~rpc ~session_id:!session_id ~vm

	let suspend vm =
		get_task (Remote.Async.VM.suspend ~session_id:!session_id ~vm)

	let resume vm =
		get_task (Remote.Async.VM.resume ~session_id:!session_id ~vm ~start_paused:true)

	let shutdown vm =
		get_task (Remote.Async.VM.clean_shutdown ~session_id:!session_id ~vm)

	let reboot vm =
		get_task (Remote.Async.VM.clean_reboot ~session_id:!session_id ~vm)

	let clone vm =
		get_task (Remote.Async.VM.clone ~session_id:!session_id ~vm ~new_name:"vm cloned")

	let destroy vm =
		Remote.VM.destroy ~rpc ~session_id:!session_id ~self:vm

		(*
	let migrate vm =
		get_task (Remote.Async.VM.migrate ~session_id:!session_id ~vm
		            ~port:(Int64.of_int !port_dest)
		            ~live:true)
		*)

	let open_console_port uri uuid =
		let port' = 0 in
		let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		Unix.handle_unix_error (Unix.setsockopt s Unix.SO_REUSEADDR) true;
		Unix.handle_unix_error (Unix.bind s) (Unix.ADDR_INET (Unix.inet_addr_any, port'));

		let port' = begin match Unix.getsockname s with
		| Unix.ADDR_INET(_, port) -> port
		| _ -> failwith "Failed to discover local socket address"
		end in
		Unix.handle_unix_error (Unix.listen s) 5;
		Printf.printf "Listening on port %d; forking child" port';
		(* Function (to execute in a fork()ed child to proxy the data *)
		(*
		let make_proxy () =
			let fd, _ = Unix.accept s in
			let path = Printf.sprintf "%s?uuid=%s" uri (Uuid.to_string uuid) in
			let headers = Xmlrpcclient.connect_headers !host path (Ref.string_of !session_id) in
			Xmlrpcclient.do_http_rpc !host !port headers "" (fun task_id ->
				Unixext.Unix.proxy fd);
			Unix.close s in
		double_fork make_proxy;
		*)
		port'
end

(* Remote Procedure *)
module RP = Fct(Client.Client)

let destroy () = GMain.Main.quit ()

open Gobject.Data

let cols = new GTree.column_list
let col_vm = cols#add string	(* string column *)
let col_uuid = cols#add string  (* string column *)
let col_name = cols#add string	(* string column *)
let col_status = cols#add string (* string column *)

let cols_vdi = new GTree.column_list
let col_vdi_uuid = cols_vdi#add string
let col_vdi_name = cols_vdi#add string
let col_vdi_size = cols_vdi#add string

let cols_sr = new GTree.column_list
let col_sr_uuid = cols_sr#add string

let create_model () =
	let store = GTree.list_store cols in
	store

let create_model_vdi () =
	let store = GTree.list_store cols_vdi in
	store

let create_model_sr () =
	let store = GTree.list_store cols_sr in
	store

let selected: string ref = ref ""
let selected_vdi: string ref = ref ""

let selection_changed (model:#GTree.model) selection () =
	let pr path =
		let row = model#get_iter path in
		let vm = model#get ~row ~column:col_vm in
		selected := vm;
	in
	List.iter pr selection#get_selected_rows

let selection_vdi_changed (model:#GTree.model) selection () =
	let pr path =
		let row = model#get_iter path in
		let vm = model#get ~row ~column:col_vdi_uuid in
		selected_vdi := vm;
	in
	List.iter pr selection#get_selected_rows

let add_column view title col =
	let col = (GTree.view_column ~title
			  ~renderer:(GTree.cell_renderer_text [],
				      [ "text", col ]) ()) in
	ignore (view#append_column col)

let create_view view =
	add_column view "UUID" col_uuid;
	add_column view "Name" col_name;
	add_column view "Status" col_status;
	view

let create_vdi_view view =
	add_column view "VDI" col_vdi_uuid;
	add_column view "Name" col_vdi_name;
	add_column view "Size" col_vdi_size;
	view

let main () =
	(* callbacks things *)
	let exec_callback act f =
		if !selected <> "" then (
			printf "%s %s\n" act !selected;
			f (Ref.of_string !selected)
		) else
			printf "no vm selected\n";
		in

	let start_callback () = exec_callback "start" RP.start
	and unpause_callback () = exec_callback "unpause" RP.unpause
	and pause_callback () = exec_callback "pause" RP.pause
	and suspend_callback () = exec_callback "suspend" RP.suspend
	and resume_callback () = exec_callback "resume" RP.resume
	and shutdown_callback () = exec_callback "shutdown" RP.shutdown
	and reboot_callback () = exec_callback "reboot" RP.reboot
	and clone_callback () = exec_callback "clone" RP.clone
	and destroy_callback () = exec_callback "destroy" RP.destroy
	(*and migrate_callback () = exec_callback "migrate" RP.migrate *)
	and console_callback () =
		let port = RP.open_console_port Constants.console_text_uri (Uuid.uuid_of_string !selected) in
		(* execute [| "gnome-terminal"; "-x"; "telnet"; "localhost"; string_of_int port; |] *)
		execute [| "gnome-terminal"; "-x"; "/home/vhanquez/bin/client"; "localhost"; string_of_int port; |]
	and vncviewer_callback () =
		let port = RP.open_console_port Constants.console_uri (Uuid.uuid_of_string !selected) in
		execute [| "vncviewer"; "localhost:" ^ string_of_int port; |]
		in

	(* ui things *)
	let file = if Sys.file_exists "xapimon.glade" then
		"xapimon.glade"
	else if Sys.file_exists "/home/vhanquez/bin/xapimon.glade" then
		"/home/vhanquez/bin/xapimon.glade"
	else
		"ocaml/xapimon/xapimon.glade" in
	let xml = Glade.create ~file () in
	let window = new GWindow.window (Obj.magic (Glade.get_widget xml ~name:"window")) in

	let notebook = new GPack.notebook (Obj.magic (Glade.get_widget xml ~name:"notebook")) in

	let treeview = new GTree.view (Obj.magic (Glade.get_widget xml ~name:"treeview")) in
	let model = create_model () in

	let treeview_vdi = new GTree.view (Obj.magic (Glade.get_widget xml ~name:"treeview_vdi")) in
	let model_vdi = create_model_vdi () in

	let combobox_sr = new GEdit.combo_box (Obj.magic (Glade.get_widget xml ~name:"combobox_sr")) in
	let model_sr = create_model_sr () in
	combobox_sr#set_model (model_sr :> GTree.model);

	let connect_button name callback =
		let button = new GButton.button (Obj.magic (Glade.get_widget xml ~name)) in
		ignore (button#connect#clicked ~callback)
		in
	connect_button "toolbutton_destroy" destroy_callback;
	connect_button "toolbutton_start" start_callback;
	connect_button "toolbutton_pause" pause_callback;
	connect_button "toolbutton_unpause" unpause_callback;
	connect_button "toolbutton_shutdown" shutdown_callback;
	connect_button "toolbutton_reboot" reboot_callback;
	connect_button "toolbutton_suspend" suspend_callback;
	connect_button "toolbutton_resume" resume_callback;
	connect_button "toolbutton_console" console_callback;
	connect_button "toolbutton_vnc" vncviewer_callback;
	connect_button "toolbutton_clone" clone_callback;
	(*connect_button "toolbutton_migrate" migrate_callback; *)

	create_view treeview;
	create_vdi_view treeview_vdi;
	treeview#set_model (Some (model :> GTree.model));
	treeview_vdi#set_model (Some (model_vdi :> GTree.model));

	treeview#selection#connect#changed ~callback:(selection_changed model treeview#selection);
	treeview#selection#connect#changed ~callback:(selection_vdi_changed model treeview_vdi#selection);

	let get_vdi_iter vdi =
		let iter = model_vdi#get_iter_first in
		match iter with
		| None -> model_vdi#append ()
		| Some iter -> (
			let found = ref false and hasnext = ref true in
			while not (!found) && !hasnext
			do
				if (model_vdi#get ~row:iter ~column:col_vdi_uuid) =
				   (Ref.string_of vdi) then
					found := true
				else
					hasnext := model_vdi#iter_next iter
			done;
			if not !found then
				model_vdi#append ()
			else
				iter
		) in

	let get_sr_iter sr =
		let iter = model_sr#get_iter_first in
		match iter with
		| None -> model_sr#append ()
		| Some iter -> (
			let found = ref false and hasnext = ref true in
			while not (!found) && !hasnext
			do
				if (model_sr#get ~row:iter ~column:col_sr_uuid) =
				   (Ref.string_of sr) then
					found := true
				else
					hasnext := model_sr#iter_next iter
			done;
			if not !found then
				model_sr#append ()
			else
				iter
		) in

	let get_iter uuid =
		let iter = model#get_iter_first in
		match iter with
		| None -> model#append ()
		| Some iter -> (
			let found = ref false and hasnext = ref true in
			while not (!found) && !hasnext
			do
				if (model#get ~row:iter ~column:col_uuid)
				   = uuid then
					found := true
				else
					hasnext := model#iter_next iter
			done;
			if not !found then
				model#append ()
			else
				iter
		) in

	let delete_vm_ref r =
		let iter = model#get_iter_first in
		match iter with
		| None -> ()
		| Some iter -> (
			let found = ref false and hasnext = ref true in
			while not (!found) && !hasnext
			do
				if (model#get ~row:iter ~column:col_vm)
				   = r then (
					model#remove iter;
					found := true
				) else
					hasnext := model#iter_next iter
			done
		)
		in

	let poll_vm () =
		let vms = RP.get_all_vms () in
		List.iter (fun vm ->
			let uuid, name, state = RP.get_info vm in
			let iter = get_iter uuid in

			model#set ~row:iter ~column:col_vm (Ref.string_of vm);
			model#set ~row:iter ~column:col_uuid (uuid);
			model#set ~row:iter ~column:col_name name;
			model#set ~row:iter ~column:col_status state) vms
		in
	let poll_storage () =
		let srs = RP.get_all_srs () in
		List.iter (fun sr ->
			let iter = get_sr_iter sr in
			model_sr#set ~row:iter ~column:col_sr_uuid (Ref.string_of sr);

			let vdis = RP.get_all_vdis sr in
			List.iter (fun vdi ->
				let iter = get_vdi_iter vdi in
				let name, size = RP.get_vdi_info vdi in

				model_vdi#set ~row:iter ~column:col_vdi_uuid (Ref.string_of vdi);
				model_vdi#set ~row:iter ~column:col_vdi_name name;
				model_vdi#set ~row:iter ~column:col_vdi_size (size_with_suffix size)
			) vdis
		) srs
		in

	let poll () =
		try
		let id = RP.get_event_id () in
		if id > !last_id then (
			let events = RP.get_events !last_id id in
			let delevents = List.filter (fun (ty,op,_) ->
				ty = "VM" && op = "del") events in
			List.iter (fun (_,_,ref) ->
				printf "deleting %s\n%!" ref;
				delete_vm_ref ref) delevents;
			List.iter (fun (ty,op,ref) ->
				eprintf "ty %s op %s ref %s\n%!" ty op ref) events;
			last_id := id;
			poll_vm ();
			poll_storage ();
		) else
			();
		true
		with
			exn -> printf "exception: %s\n%!" (Printexc.to_string exn);
			true
		in

	window#show ();
	window#set_default_size ~width:1024 ~height:400;
	window#connect#destroy ~callback:GMain.quit;

	poll_vm ();
	poll_storage ();
	last_id := RP.get_event_id ();

	let id = GMain.Timeout.add ~ms:1000 ~callback:poll in ignore (id);

	GMain.Main.main ()

let _ =
(* parse args *)
	let args = [
	("--host", Arg.Set_string host, "set hostname");
	("--port", Arg.Set_int port, "set port");
	("--dhost", Arg.Set_string host_dest, "set migration host");
	("--dport", Arg.Set_int port_dest, "set migration port"); ]; in
	Arg.parse args (fun x -> eprintf "unknown arg: %s" x) "xapimon";

	if !host = "" then (
		eprintf "error: no host specified\n";
		exit 1
	);
	printf "connecting to %s:%d\n" !host !port;

	RP.session_init ();
	GtkMain.Main.init ();
	main()

