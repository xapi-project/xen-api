module D = Debug.Debugger(struct let name="rrdd_http_handler" end)
open D

open Threadext
open Rrdd_shared

(* A handler for unarchiving RRDs. Only called on pool master. *)
let unarchive_rrd_handler (req: Http.Request.t) (s : Unix.file_descr) _ =
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
let get_vm_rrd_handler (req: Http.Request.t) (s : Unix.file_descr) _ =
	debug "get_vm_rrd_handler: start";
	let query = req.Http.Request.query in
	let vm_uuid = List.assoc "uuid" query in
	let rrd = Mutex.execute mutex
		(fun () -> Rrd.copy_rrd (Hashtbl.find vm_rrds vm_uuid).rrd) in
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	Rrd.to_fd rrd s

(* A handler for putting the host's RRD data into the Http response. *)
let get_host_rrd_handler (req: Http.Request.t) (s : Unix.file_descr) _ =
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
