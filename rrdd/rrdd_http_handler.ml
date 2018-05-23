module D = Debug.Make(struct let name="rrdd_http_handler" end)
open D

module Hashtblext = Xapi_stdext_std.Hashtblext
open Xapi_stdext_threads.Threadext
open Rrdd_shared

(* A handler for unarchiving RRDs. Only called on pool master. *)
let unarchive_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
  debug "unarchive_rrd_handler: start";
  let query = req.Http.Request.query in
  let uuid = List.assoc "uuid" query in
  let path = Rrdd_libs.Constants.rrd_location ^ "/" ^ uuid in
  let rrd = rrd_of_gzip path in
  let header_content =
    Http.http_200_ok ~version:"1.0" ~keep_alive:false ()
    @ ["Access-Control-Allow-Origin: *"] in
  Http_svr.headers s header_content;
  Rrd_unix.to_fd rrd s

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
  Rrd_unix.to_fd rrd s

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
  Rrd_unix.to_fd ~json:(List.mem_assoc "json" query) rrd s

(* A handler for putting the SR's RRD data into the Http response. *)
let get_sr_rrd_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
  debug "get_sr_rrd_handler: start";
  let query = req.Http.Request.query in
  let sr_uuid = List.assoc "uuid" query in
  let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd ( let rrdi =
                                                            try 
                                                              Hashtbl.find sr_rrds sr_uuid
                                                            with Not_found -> failwith "No SR RRD available!"
                                                          in rrdi.rrd)
                                ) in
  Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
  Rrd_unix.to_fd rrd s

(* Get an XML/JSON document (as a string) representing the updates since the
 * specified start time. *)
let get_host_stats ?(json = false) ~(start : int64) ~(interval : int64)
    ~(cfopt : Rrd.cf_type option) ~(is_host : string) ~(vm_uuid : string)
    ~(sr_uuid : string) () =
  Mutex.execute mutex (fun () ->
      let prefixandrrds =
        let vm_rrds = Hashtblext.to_list vm_rrds in
        let sr_rrds = Hashtblext.to_list sr_rrds in
        let host_rrds =
          if is_host = "true" then
            match !host_rrd with None -> [] | Some rrdi -> [("host:" ^ (Inventory.lookup Inventory._installation_uuid) ^ ":", rrdi.rrd)]
          else [] in
        let vmsandrrds =
          if vm_uuid = "all" then vm_rrds
          else if vm_uuid = "none" then []
          else List.filter (fun (k, _) -> k = vm_uuid) vm_rrds in
        let vm_rrds_altered = List.map (fun (k, v) -> "vm:" ^ k ^ ":", v.rrd) vmsandrrds in
        let srsandrrds =
          if sr_uuid = "all" then sr_rrds
          else if sr_uuid = "none" then []
          else List.filter (fun (k, _) -> k = sr_uuid) sr_rrds in
        let sr_rrds_altered = List.map (fun (k, v) -> "sr:" ^ k ^ ":", v.rrd) srsandrrds in
        host_rrds @ vm_rrds_altered @ sr_rrds_altered
      in
      Rrd_updates.export ~json prefixandrrds start interval cfopt)

(* Writes XML/JSON representing the updates since the specified start time to
 * the file descriptor that corresponds to the client HTTP connection. *)
let get_rrd_updates_handler (req : Http.Request.t) (s : Unix.file_descr) _ =
  let query = req.Http.Request.query in
  let start = Int64.of_string (List.assoc "start" query) in
  let cfopt = try Some (Rrd.cf_type_of_string (List.assoc "cf" query)) with _ -> None in
  let interval = try Int64.of_string (List.assoc "interval" query) with _ -> 0L in
  let query_associated_value key lst = try (List.assoc key lst) with _ -> "none" in
  let is_host = if (List.mem_assoc "host" query) then query_associated_value "host" query else "none" in
  let vm_uuid = if (List.mem_assoc "vm_uuid" query) then query_associated_value "vm_uuid" query else "all" in
  let sr_uuid = if (List.mem_assoc "sr_uuid" query) then query_associated_value "sr_uuid" query else "none" in
  let json = List.mem_assoc "json" query in
  let reply = get_host_stats ~json ~start ~interval ~cfopt ~is_host ~vm_uuid ~sr_uuid () in
  let headers = Http.http_200_ok_with_content (Int64.of_int (String.length reply))
      ~version:"1.1" ~keep_alive:true () in
  let headers =
    if json then headers else headers @ [Http.Hdr.content_type ^ ": text/xml"] in
  let headers = headers @ [
      "Access-Control-Allow-Origin: *";
      "Access-Control-Allow-Headers: X-Requested-With";
    ] in
  Http_svr.headers s headers;
  Unix.write s (Bytes.unsafe_of_string reply) 0 (String.length reply) |> ignore

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
    archive_rrd_internal ~uuid ~rrd:(Rrd.copy_rrd rrd) ()
  ) else (
    debug "Receiving RRD for resident VM uuid=%s. Replacing in hashtable." uuid;
    let domid = int_of_string (List.assoc "domid" query) in
    Mutex.execute mutex (fun _ ->
        Hashtbl.replace vm_rrds uuid {rrd; dss = []; domid}
      )
  )
