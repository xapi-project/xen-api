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
(** Module that defines API functions for Message objects
 * @group XenAPI functions
 *)

 
(** Message store *)

(* We use a filesystem based 'database': 
 *  Base directory: @VARDIR@/blobs/messages
 *  All messages go in there, filename=timestamp
 *  
 *  Symlinks are created to the messages for fast indexing:
 *  @VARDIR@/blobs/messages/VM/<uuid>/<timestamp> -> message
 *  @VARDIR@/blobs/messages/uuid/<message uuid> -> message
 *  @VARDIR@/blobs/messages/ref/<message ref> -> message
 *)

open Listext   
open Stringext
open Threadext
 
module D = Debug.Debugger(struct let name="xapi" end)
open D

let message_dir = Xapi_globs.xapi_blob_location ^ "/messages"

let event_mutex = Mutex.create ()

let in_memory_cache = ref []
let in_memory_cache_mutex = Mutex.create ()
let in_memory_cache_length = ref 0
let in_memory_cache_length_max = 512
let in_memory_cache_length_default = 256

let class_to_string cls = 
  match cls with 
    | `VM -> "VM" 
    | `Host -> "Host" 
    | `SR -> "SR" 
    | `Pool -> "Pool"
    | `VMPP -> "VMPP" 
    | _ -> "unknown"

let string_to_class str = 
  match str with 
    | "VM" -> `VM
    | "Host" -> `Host
    | "SR" -> `SR
    | "Pool" -> `Pool
    | "VMPP" -> `VMPP
    | _ -> failwith "Bad type"

(* We use the timestamp to name the file. For consistency, use this function *)
let timestamp_to_string f =
  Printf.sprintf "%0.5f" f
  
(************* Marshalling/unmarshalling functions ************)

let to_xml output _ref message =
  let tag n next () = 
    Xmlm.output output (`El_start (("",n),[])); 
    List.iter (fun x -> x ()) next; 
    Xmlm.output output `El_end 
  in
  let data dat () = Xmlm.output output (`Data dat) in

  Xmlm.output output (`Dtd None);
  tag "message" [
    tag "ref" [ data (Ref.string_of _ref) ];
    tag "name" [ data message.API.message_name ];
    tag "priority" [ data (Int64.to_string message.API.message_priority) ];
    tag "cls" [data (class_to_string message.API.message_cls) ];
    tag "obj_uuid" [data message.API.message_obj_uuid ];
    tag "timestamp" [data (Date.to_string message.API.message_timestamp) ];
    tag "uuid" [data message.API.message_uuid];
    tag "body" [data message.API.message_body]
  ] ()

let of_xml input =
  let current_elt = ref "" in
  let message = ref {
    API.message_name="";
    API.message_priority=0L;
    API.message_cls=`VM;
    API.message_obj_uuid="";
    API.message_timestamp=Date.never;
    API.message_body="";
    API.message_uuid = ""}
  in
  let _ref = ref "" in
  let rec f () = match Xmlm.input input with
  | `El_start ((ns,tag),attr) -> current_elt := tag; f ()
  | `El_end                   -> current_elt := ""; if Xmlm.eoi input then () else f ()
  | `Data dat                 -> 
    begin match !current_elt with
    | "name" -> message := {!message with API.message_name=dat}
    | "priority" -> message := {!message with API.message_priority=Int64.of_string dat}
    | "cls" -> message := {!message with API.message_cls=string_to_class dat}
    | "obj_uuid" -> message := {!message with API.message_obj_uuid=dat}
    | "timestamp" -> message := {!message with API.message_timestamp=Date.of_string dat}
    | "uuid" -> message := {!message with API.message_uuid=dat}
    | "body" -> message := {!message with API.message_body=dat}
    | "ref" -> _ref := dat
    | _ -> failwith "Bad XML!"
    end;
    f ()
  | `Dtd _ -> f () 
  in
  try 
    f ();
    (Ref.of_string !_ref,!message) 
  with e -> log_backtrace (); debug "Caught exception: %s" (Printexc.to_string e); raise e

let export_xml messages =
	let size = 500 * (List.length messages) in
	let buf = Buffer.create size in
	let output = Xmlm.make_output (`Buffer buf) in
	List.iter (function | r,m -> to_xml output r m) messages ;
	Buffer.contents buf

let import_xml xml_in =
	let split_xml =
		let ob = Buffer.create 600 in
		(* let i = Xmlm.make_input (`String (0, xml)) in *)
		let o = Xmlm.make_output (`Buffer ob) in
		let rec pull xml_in o depth =
			Xmlm.output o (Xmlm.peek xml_in);
			match Xmlm.input xml_in with
				| `El_start _ -> pull xml_in o (depth + 1)
				| `El_end -> if depth = 1 then () else pull xml_in o (depth - 1)
				| `Data _ -> pull xml_in o depth
				| `Dtd _ -> pull xml_in o depth
		in

		let out = ref [] in
		while not (Xmlm.eoi xml_in) do
			pull xml_in o 0 ;
			out := (Buffer.contents ob) :: !out ;
			Buffer.clear ob
		done ;
		!out in

	let rec loop = function
		| [] -> []
		| m :: ms ->
			  let im = Xmlm.make_input (`String (0, m)) in
			  (of_xml im) :: loop ms
	in loop split_xml


(********** Symlink functions *************)

let class_symlink cls obj_uuid = 
  let strcls = class_to_string cls in
  Printf.sprintf "%s/%s/%s" message_dir strcls obj_uuid

let uuid_symlink () = 
  Printf.sprintf "%s/uuids" message_dir

let ref_symlink () =
  Printf.sprintf "%s/refs" message_dir

(** Returns a list of tuples - (directory, filename) *)
let symlinks _ref message basefilename =
  List.map (fun (dir,fnameopt) ->
    let newfname = match fnameopt with
      | None -> basefilename
      | Some f -> f
    in
    (dir,dir ^ "/" ^ newfname))   
    [(class_symlink message.API.message_cls message.API.message_obj_uuid, None);
     (uuid_symlink (), Some message.API.message_uuid);
     (ref_symlink (), Some (Ref.string_of _ref));]


(** Check to see if the UUID is valid. This should not use get_by_uuid as
    this causes spurious exceptions to be logged... *)
let check_uuid ~__context ~cls ~uuid =
  try
    (match cls with
      | `VM -> ignore(Db.VM.get_by_uuid ~__context ~uuid)
      | `Host -> ignore(Db.Host.get_by_uuid ~__context ~uuid)
      | `SR -> ignore(Db.SR.get_by_uuid ~__context ~uuid)
      | `Pool -> ignore(Db.Pool.get_by_uuid ~__context ~uuid)
      | `VMPP -> ignore(Db.VMPP.get_by_uuid ~__context ~uuid)
    );
    true
  with _ -> 
    false

(*********** Thread_queue to exec the message script hook ***********)

let queue_push = ref (fun (description: string) (m : string) -> false)

let message_to_string (_ref,message) =
  let buffer = Buffer.create 10 in
  let output = Xmlm.make_output (`Buffer buffer) in
  to_xml output _ref message;
  Buffer.contents buffer

let handle_message ~__context message = 
  try
    if not (Pool_features.is_enabled ~__context Features.Email)
    then info "Email alerting is restricted by current license: not generating email"
    else begin
      let output, log = Forkhelpers.execute_command_get_output Xapi_globs.xapi_message_script [message] in
      debug "Executed message hook: output='%s' log='%s'" output log
    end
  with e ->
    error "Unexpected exception in message hook. Exception='%s'" (ExnHelper.string_of_exn e);
    log_backtrace ()

let start_message_hook_thread ~__context () =
  queue_push := (Thread_queue.make ~name:"email message queue" ~max_q_length:100 (handle_message ~__context)).Thread_queue.push_fn


(********************************************************************)

let get_last_n dir n =
	let cmp a b = compare
		(float_of_string a)
		(float_of_string b) (* oldest first, reverse later *)
	and files = List.filter
		(fun m -> try float_of_string m > 0.0 with _ -> false)
		(Array.to_list (Sys.readdir dir))
	and msg_of_file mf =
		let fn = message_dir ^ "/" ^ mf in
		let ic = open_in fn in
		let xi = Xmlm.make_input (`Channel ic) in
		let (_ref,msg) = Pervasiveext.finally
			(fun () -> of_xml xi)
			(fun () -> close_in ic) in
		let ts = Date.to_float msg.API.message_timestamp in
		(ts, _ref, msg)
	in
	List.rev_map msg_of_file
		(Listext.List.take n
			 (List.fast_sort cmp files))

let repopulate_cache () = try
	let msgs = get_last_n
		message_dir
		in_memory_cache_length_default in
	let len = List.length msgs in
	let n = Mutex.execute in_memory_cache_mutex
		(fun () ->
			in_memory_cache := msgs ;
			in_memory_cache_length := len ;
			!in_memory_cache_length
		) in
	debug "Repopulating in-memory cache of messages: Length=%d" n
	with e ->
		error "Exception '%s' in Xapi_message.repopulate_cache; cache left unchanged"
			(ExnHelper.string_of_exn e)

let cache_insert _ref message =
	let timestamp = Date.to_float message.API.message_timestamp in
	Mutex.execute in_memory_cache_mutex (fun () ->
		in_memory_cache :=
			(timestamp,_ref,message) :: !in_memory_cache ;

		in_memory_cache_length :=
			!in_memory_cache_length + 1 ;

		if !in_memory_cache_length > in_memory_cache_length_max then begin
			in_memory_cache := Listext.List.take
				in_memory_cache_length_default
				!in_memory_cache ;
			in_memory_cache_length := in_memory_cache_length_default ;
			debug "Pruning in-memory cache of messages: Length=%d (%d)"
				!in_memory_cache_length
				(List.length !in_memory_cache)
		end)

(** Write: write message to disk. Returns boolean indicating whether
	message was written *)
let write ~_ref ~message =
	(* Check if a message with _ref has already been written *)
	let message_exists () =
		let file = (ref_symlink ()) ^ "/" ^ (Ref.string_of _ref) in
		try Unix.access file [Unix.F_OK] ; true with _ -> false in
	(* Make sure the directory is there *)
	if not (Helpers.local_storage_exists ())
	then false (* couldn't write message *)
	else begin
		Unixext.mkdir_safe message_dir 0o700;
		let timestamp = ref (Date.to_float (message.API.message_timestamp)) in

		if message_exists () then false (* Don't overwrite message with same ref *)
		else try Mutex.execute event_mutex (fun () ->
			let fd, basefilename, filename =
				(* Try 10, no wait, 11 times to create message file *)
				let rec doit n =
					if n>10 then failwith "Couldn't create a file" else begin
						let basefilename = timestamp_to_string !timestamp in
						let filename = message_dir ^ "/" ^ basefilename in
						try
							let fd = Unix.openfile filename
								[Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
							(* Set file's timestamp to message timestamp *)
							Unix.utimes filename !timestamp !timestamp ;
							fd, basefilename, filename
						with _ -> begin
							(* We may be copying messages from another
							   pool, in which case we may have
							   filename collision (unlikely, but
							   possible). So increment the filename
							   and try again, but leave the original
							   timestamp in the message untouched. *)
							timestamp := !timestamp +. 0.00001 ;
							doit (n+1)
						end
					end
				in doit 0
			in

			(* Write the message to file *)
			let oc = Unix.out_channel_of_descr fd in
			let output = Xmlm.make_output (`Channel oc) in
			to_xml output _ref message;
			close_out oc;

			(* Message now written, let's symlink it in various places *)
			let symlinks = symlinks _ref message basefilename in
			List.iter (fun (dir,newpath) ->
				Unixext.mkdir_rec dir 0o700;
			  Unix.symlink filename newpath) symlinks
		 ) ; true (* End of event_mutex *)
		 with _ -> false
	end

(** create: Create a new message, and write to disk. Returns null ref
	if write failed, or message ref otherwise. *)
let create ~__context ~name ~priority ~cls ~obj_uuid ~body =
	debug "Message.create %s %Ld %s %s" name priority
		(class_to_string cls) obj_uuid;

	(if not (Encodings.UTF8_XML.is_valid body)
	 then raise (Api_errors.Server_error
					 (Api_errors.invalid_value, ["UTF8 expected"]))) ;
	(if not (check_uuid ~__context ~cls ~uuid:obj_uuid)
	 then raise (Api_errors.Server_error
					 (Api_errors.uuid_invalid, [class_to_string cls; obj_uuid]))) ;

	let _ref = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in

	let timestamp = Mutex.execute event_mutex (fun () ->
		Unix.gettimeofday ()) in

	let message = {API.message_name=name;
				   API.message_uuid=uuid;
				   API.message_priority=priority;
				   API.message_cls=cls;
				   API.message_obj_uuid=obj_uuid;
				   API.message_timestamp=Date.of_float timestamp;
				   API.message_body=body;}
	in

	(* Write the message to disk *)
	let was_written = write ~_ref ~message in

	(* Insert a written message into in_memory_cache *)
	(if was_written then cache_insert _ref message) ;

	(* Emit a create event (with the old event API). If the message
	   hasn't been written, we may want to also emit a del even, for
	   consistency (since the reference for the message will never be
	   valid again. *)
	let xml = API.To.message_t message in
	Xapi_event.event_add ~snapshot:xml "message" "add" (Ref.string_of _ref);
	let (_: bool) = (!queue_push) name (message_to_string (_ref,message)) in
	(*Xapi_event.event_add ~snapshot:xml "message" "del" (Ref.string_of _ref);*)

	(* Return the message ref, or Ref.null if the message wasn't written *)
	if was_written then _ref else Ref.null

      
let deleted : (Date.iso8601 * API.ref_message) list ref = ref [Date.never, Ref.null]
let ndeleted = ref 1
let deleted_mutex = Mutex.create ()
      
let destroy_real basefilename =
  let filename = message_dir ^ "/" ^ basefilename in
  let ic = open_in filename in
  let (_ref,message) = Pervasiveext.finally 
    (fun () -> of_xml (Xmlm.make_input (`Channel ic)))
    (fun () -> close_in ic) 
  in
  let symlinks = symlinks _ref message basefilename in
  List.iter (fun (dir,newpath) ->
    Unixext.unlink_safe newpath) symlinks;
  Unixext.unlink_safe filename;
  let xml = API.To.message_t message in
  Mutex.execute event_mutex 
	  (fun () ->
		   deleted := (Date.of_float (Unix.gettimeofday ()), _ref) :: !deleted;
		   ndeleted := !ndeleted + 1;
		   if !ndeleted > 1024
		   then
			   (deleted := Listext.List.take 512 !deleted;
				ndeleted := 512)		    
	  );
  Xapi_event.event_add ~snapshot:xml "message" "del" (Ref.string_of _ref)

let destroy ~__context ~self = 
  (* Find the original message so we know where the symlinks will be *)
  let symlinkfname = (ref_symlink ()) ^ "/" ^ (Ref.string_of self) in
  let fullpath = 
    try Unix.readlink symlinkfname 
    with _ -> raise (Api_errors.Server_error (Api_errors.handle_invalid, [Datamodel._message; Ref.string_of self]))
  in
  let basefilename = List.hd (List.rev (String.split '/' fullpath)) in
  destroy_real basefilename


(* Gc the messages - leave only the number of messages defined in 'Xapi_globs.message_limit' *)
let gc ~__context =
  if (try (Unix.access message_dir [Unix.F_OK]; true) with _ -> false) then
    begin
      let allmsg = List.filter_map 
	(fun msg -> 
	   try 
	     Some (float_of_string msg, msg) 
	   with _ -> 
	     None) 
	(Array.to_list (Sys.readdir message_dir)) 
      in
      if List.length allmsg > Xapi_globs.message_limit then
	begin
	  let sorted = List.sort (fun (t1,_) (t2,_) -> compare t1 t2) allmsg in
	  let n = List.length sorted in
	  let to_reap = n - Xapi_globs.message_limit in
	  let rec reap_one i msgs =
	    if i=to_reap then () else
	      begin
		begin
		  try destroy_real (snd (List.hd msgs)) 
		  with e -> 
		    debug "Failed to destroy message %s" (snd (List.hd msgs));
		    debug "Caught exception %s" (Printexc.to_string e)
		end;
		reap_one (i+1) (List.tl msgs)
	      end
	  in
	  reap_one 0 sorted
	end
    end

let get_real_inner dir filter since =
  try
    let allmsgs = Array.to_list (Sys.readdir dir) in
    let since_f = since in
	let messages = List.filter (fun msg -> try float_of_string msg > since_f with _ -> false) allmsgs in
	let messages = List.filter_map (fun msg_fname ->
		let filename = dir ^ "/" ^ msg_fname in
      try 
	let ic = open_in filename in
	let (_ref,msg) = Pervasiveext.finally (fun () -> of_xml (Xmlm.make_input (`Channel ic))) (fun () -> close_in ic) in	  
			if filter msg then Some (float_of_string msg_fname,_ref,msg) else None
      with _ -> None) messages
    in
	List.sort (fun (t1,r1,m1) (t2,r2,m2) -> compare t2 t1) messages
  with _ -> [] (* Message directory missing *)

let get_real dir filter since =
	List.map (fun (_,r,m) -> (r,m)) (get_real_inner dir filter since)

let get ~__context ~cls ~obj_uuid ~since =
  (* Read in all the messages for a particular object *)
  let class_symlink = class_symlink cls obj_uuid in
  (if not (check_uuid ~__context ~cls ~uuid:obj_uuid) then raise (Api_errors.Server_error (Api_errors.uuid_invalid, [])));
  get_real class_symlink (fun _ -> true) (Date.to_float since)

let get_since ~__context ~since =
  get_real message_dir (fun _ -> true) (Date.to_float since)

let get_since_for_events ~__context since =
	let now = Mutex.execute event_mutex (fun () -> Unix.gettimeofday ()) in
	let result = Mutex.execute in_memory_cache_mutex
		(fun () ->
			 match !in_memory_cache with
			 | (last_in_memory, _, _) :: _ when last_in_memory > since ->
				   Some (List.filter_map
					         (fun (timestamp,_ref,msg) ->
						          if timestamp > since then Some (_ref, msg) else None)
					         !in_memory_cache)
			 | _ -> None) in
	match result with
		| Some x -> (now,x)
		| None ->
	(now, get_real message_dir (fun _ -> true) since)

let get_by_uuid ~__context ~uuid =
  try
    let message_filename = (uuid_symlink ()) ^ "/" ^ uuid in
    let ic = open_in message_filename in
    let (_ref,_) = Pervasiveext.finally (fun () -> of_xml (Xmlm.make_input (`Channel ic))) (fun () -> close_in ic) in
    _ref
  with
      _ -> raise (Api_errors.Server_error (Api_errors.uuid_invalid, [ uuid ]))

let get_all ~__context =
  try
    let allmsgs = Array.to_list (Sys.readdir (ref_symlink ())) in
    List.map (fun r -> Ref.of_string r) allmsgs
  with _ -> []

let get_record ~__context ~self =
  try
    let symlinkfname = (ref_symlink ()) ^ "/" ^ (Ref.string_of self) in
    let fullpath = Unix.readlink symlinkfname in
    let ic = open_in fullpath in
    let (_ref,message) = Pervasiveext.finally 
      (fun () -> of_xml (Xmlm.make_input (`Channel ic)))
      (fun () -> close_in ic) 
    in message
  with _ ->
    raise (Api_errors.Server_error (Api_errors.handle_invalid, ["message";(Ref.string_of self)]))

let get_all_records ~__context =
  get_real message_dir (fun _ -> true) (0.0)

let get_all_records_where ~__context ~expr =
  get_real message_dir (fun _ -> true) (0.0)

let register_event_hook () =
	Mutex.execute in_memory_cache_mutex (fun () ->
		let messages = get_real_inner message_dir (fun _ -> true) 0.0 in
		let last_256 = List.take 256 messages in
		in_memory_cache := last_256;
		let get_ts (ts,_,_) = ts in
		debug "Constructing in-memory-cache: most length=%d" (List.length last_256);
		(try debug "newest=%f oldest=%f" (get_ts (List.hd last_256)) (get_ts (List.hd (List.rev last_256))) with _ -> ());
		in_memory_cache_length := List.length !in_memory_cache);
	Xapi_event.message_get_since_for_events := get_since_for_events

(* Query params: cls=VM etc, obj_uuid=<..>, min_priority. Returns the last
   days worth of messages as an RSS feed. *)  
let rss_handler (req: Http.Request.t) (bio: Buf_io.t) _ =
  let query = req.Http.Request.query in
  req.Http.Request.close <- true;
  debug "Message handler";
  let s = Buf_io.fd_of bio in
  Buf_io.assert_buffer_empty bio;
  Xapi_http.with_context ~dummy:true "Obtaining the RSS message feed" req s
    (fun __context ->
      let now = Unix.gettimeofday () in      
      let since = 
	if List.mem_assoc "since" query then
	  Date.of_float (float_of_string (List.assoc "since" query))
	else
	  let since=now -. (3600.0 *. 24.0) in
	  Date.of_float since
      in
      let messages = 
	if List.mem_assoc "cls" query then
	  let cls = string_to_class (List.assoc "cls" query) in
	  let obj_uuid = List.assoc "obj_uuid" query in
	  get ~__context ~cls ~obj_uuid ~since
	else
	  get_since ~__context ~since 
      in
      let items = List.map (fun (_ref,message) ->
	let body = Printf.sprintf "<h3>%s: %s</h3><p>%s</p>" 
	  (class_to_string message.API.message_cls) 
	  message.API.message_obj_uuid
	  message.API.message_body	   
	in
	{Rss.item_title=message.API.message_name;
	 Rss.item_link=None;
	 Rss.item_description=body;
	 Rss.item_pubdate=Date.rfc822_to_string 
	    (Date.rfc822_of_float (Date.to_float message.API.message_timestamp))}) messages 
      in
      let channel =
	{Rss.chan_title="XenServer Messages";
	 Rss.chan_description="Message from the XenServer";
	 Rss.chan_language="en";
	 Rss.chan_pubdate=Date.to_string (Date.of_float now);
	 Rss.chan_items=items;}
      in
      let body = Xml.to_string (Rss.to_xml [channel]) in
      let body = "<?xml version=\"1.0\"?>" ^ body in
      Http_svr.headers s ((Http.http_200_ok_with_content 
			      (Int64.of_int (String.length body)) 
			      ~version:"1.1" ~keep_alive:false ())@[Http.Hdr.content_type ^": application/rss+xml"]);
      ignore(Unix.write s body 0 (String.length body)))

(** Handler for PUTing messages to a host.
	Query params: { cls=<obj class>, uuid=<obj uuid> } *)
let handler (req: Http.Request.t) fd _ =
	let query = req.Http.Request.query in
	req.Http.Request.close <- true ;
	debug "Xapi_message.handler: receiving messages" ;

	let check_query param =
		if not (List.mem_assoc param query) then begin
			error "Xapi_message.handler: HTTP request for message lacked %s parameter" param ;
			Http_svr.headers fd (Http.http_400_badrequest ()) ;
			failwith (Printf.sprintf "Xapi_message.handler: Missing %s parameter" param)
		end in

	(* Check query for required params *)
	check_query "uuid" ; check_query "cls" ;

	Xapi_http.with_context ~dummy:true "Xapi_message.handler" req fd
		(fun __context -> try
			(* Get and check query parameters *)
			let uuid = List.assoc "uuid" query
			and cls = List.assoc "cls" query in
			let cls = try string_to_class cls with _ ->
				failwith ("Xapi_message.handler: Bad class " ^ cls) in
			if not (check_uuid ~__context ~cls ~uuid) then
				failwith ("Xapi_message.handler: Bad uuid " ^ uuid) ;

			(* Tell client we're good to receive *)
			Http_svr.headers fd (Http.http_200_ok ()) ;

			(* Read messages in, and write to filesystem *)
			let xml_in = Xmlm.make_input
				(`Channel (Unix.in_channel_of_descr fd)) in
			let messages = import_xml xml_in in
			List.iter (function (r,m) -> ignore (write r m)) messages ;

			(* Flush cache and reload *)
			repopulate_cache () ;

			with e -> error "Xapi_message.handler: caught exception '%s'"
				(ExnHelper.string_of_exn e)
		)
