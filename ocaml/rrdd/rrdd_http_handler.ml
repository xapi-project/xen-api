module D = Debug.Debugger(struct let name="rrdd_http_handler" end)
open D

open Threadext
open Rrdd_shared

(* A handler for unarchiving RRDs. Only called on pool master. *)
let unarchive_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
	debug "unarchive_rrd_handler: start";
	let query = req.Http.Request.query in
	let uuid = List.assoc "uuid" query in
	let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
	let rrd = rrd_of_gzip path in
	let header_content =
		Http.http_200_ok ~version:"1.0" ~keep_alive:false ()
			@ ["Access-Control-Allow-Origin: *"] in
	Http_svr.headers s header_content;
	Rrd.to_fd rrd s

(* A handler for putting a VM's RRD data into the Http response.
 * The rrdd assumes that it has RRD for the vm_uuid, since xapi confirmed this
 * with rrdd over XMLRPC before forwarding the HTTP request --- see rrdd_proxy
 * in xapi.
 *)
let get_vm_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
	debug "get_vm_rrd_handler: start";
	let query = req.Http.Request.query in
	let vm_uuid = List.assoc "uuid" query in
	let rrd = Mutex.execute mutex
		(fun () -> Rrd.copy_rrd (Hashtbl.find vm_rrds vm_uuid).rrd) in
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	Rrd.to_fd rrd s

(* A handler for putting the host's RRD data into the Http response. *)
let get_host_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
	debug "get_host_rrd_handler: start";
	let query = req.Http.Request.query in
	let rrd = Mutex.execute mutex (fun _ ->
		debug "Received request for Host RRD.";
		Rrd.copy_rrd (match !host_rrd with
			Some rrdi -> rrdi.rrd | None -> failwith "No host RRD available!")
	) in
	Http_svr.headers s
		(Http.http_200_ok ~version:"1.0" ~keep_alive:false () @
			["Access-Control-Allow-Origin: *"]);
	Rrd.to_fd ~json:(List.mem_assoc "json" query) rrd s

(* Get an XML/JSON document (as a string) representing the updates since the
 * specified start time. *)
let get_host_stats ?(json = false) ~(start : int64) ~(interval : int64)
		~(cfopt : Rrd.cf_type option) ~(is_host : bool) ?(uuid : string option) () =
	Mutex.execute mutex (fun () ->
		let prefixandrrds =
			let vmsandrrds = Hashtbl.fold (fun k v acc -> (k, v)::acc) vm_rrds [] in
			let vmsandrrds =
			match uuid with
			| None -> vmsandrrds
			| Some uuid -> List.filter (fun (k, v) -> k = uuid) vmsandrrds
			in
			let vm_rrds = List.map (fun (k, v) -> "vm:" ^ k ^ ":", v.rrd) vmsandrrds in
			if is_host then match !host_rrd with None -> vm_rrds | Some rrdi ->
				("host:" ^ localhost_uuid ^ ":", rrdi.rrd)::vm_rrds
			else vm_rrds
		in
		Rrd.export ~json prefixandrrds start interval cfopt)

(* Writes XML/JSON representing the updates since the specified start time to
 * the file descriptor that corresponds to the client HTTP connection. *)
let get_rrd_updates_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
	let query = req.Http.Request.query in
	let start = Int64.of_string (List.assoc "start" query) in
	let cfopt = try Some (Rrd.cf_type_of_string (List.assoc "cf" query)) with _ -> None in
	let interval = try Int64.of_string (List.assoc "interval" query) with _ -> 0L in
	let is_host = List.mem_assoc "host" query in
	let uuid = try Some (List.assoc "vm_uuid" query) with _ -> None in
	let json = List.mem_assoc "json" query in
	let reply = get_host_stats ~json ~start ~interval ~cfopt ~is_host ?uuid () in
	let headers = Http.http_200_ok_with_content (Int64.of_int (String.length reply))
		~version:"1.1" ~keep_alive:false () in
	let headers =
		if json then headers else headers @ [Http.Hdr.content_type ^ ": text/xml"] in
	Http_svr.headers s headers;
	ignore (Unix.write s reply 0 (String.length reply))

(* Reads RRD information sent from the client over HTTP through the file
 * descriptor. The handler either archives the data, or updates the relevant
 * field in the memory. If archiving, it is guaranteed by rrdd_proxy to be
 * called on the master, and that the uuid represents a VM, not a host. *)
let put_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
	let query = req.Http.Request.query in
	let uuid = List.assoc "uuid" query in
	let is_host = bool_of_string (List.assoc "is_host" query) in
	(* Tell the client that we are ready to receive the data. *)
	Http_svr.headers s (Http.http_200_ok ());
	let rrd = rrd_of_fd s in
	(* By now, we know that the data represents a valid RRD. *)
	if List.mem_assoc "archive" query then (
		debug "Receiving RRD on the master for archiving, type=%s."
			(if is_host then "Host" else "VM uuid=" ^ uuid);
		archive_rrd ~uuid ~rrd:(Rrd.copy_rrd rrd) ()
	) else (
		debug "Receiving RRD for resident VM uuid=%s. Replacing in hashtable." uuid;
		Mutex.execute mutex (fun _ -> Hashtbl.replace vm_rrds uuid {rrd; dss = []})
	)
