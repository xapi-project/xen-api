(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Xapi_stdext_monadic
open Xapi_stdext_std
open Xapi_stdext_threads

open Rrd

let version = "0.1.3"



module Stdout = struct

	(* Options *)
	let print_debug = ref false

	(* Output helper functions *)
	let warn ?(oc=stdout) msg = Printf.fprintf oc "WARNING: %s\n%!" msg
	let print ?(oc=stdout) msg = Printf.fprintf oc "%s\n%!" msg

	let time_of_float x = 
		let time = Unix.gmtime x in
		Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec

	let stdout_m = Mutex.create ()
	let debug (fmt: ('a , unit, string, unit) format4) =
		if !print_debug then
			Threadext.Mutex.execute stdout_m
				(fun () ->
					Printf.kprintf
						(fun s -> Printf.printf "%s [%d] %s\n" 
							(time_of_float (Unix.gettimeofday ())) 
							(Thread.id (Thread.self ())) s; flush stdout) fmt)
		else
			Printf.kprintf (fun _ -> ()) fmt

	let string_of_float flt = 
		if fst (modf flt) = 0. then Printf.sprintf "%.0f" flt
		else                        Printf.sprintf "%f" flt
end

module XAPI = struct

	(* Options *)
	let delay = ref 120.

	include Client

	let rpc xml =
		let open Xmlrpc_client in
		let http = xmlrpc ~version:"1.0" "/" in
		XMLRPC_protocol.rpc ~srcstr:"rrd2csv" ~dststr:"xapi" 
			~transport:(Unix (Filename.concat "/var/lib/xcp" "xapi")) ~http xml

	(* execute f within an active session *)
	let rec retry_with_session f x =
		let session =
			let rec aux () = 
				try Client.Session.login_with_password
					~rpc ~uname:"" ~pwd:"" ~version:"1.4" ~originator:"rrd2csv"
				with _ -> Thread.delay !delay; aux () in
			aux () in
		let logout_unsafe session =
			try Client.Session.logout ~rpc ~session_id:session
			with _ -> ()
		in
		try
			let result = f session x, session in
			logout_unsafe session;
			result
		with e ->
			logout_unsafe session;
			Stdout.warn 
				(Printf.sprintf "Got '%s', trying with a new session ..."
					 (Printexc.to_string e));
			Thread.delay !delay;
			retry_with_session f x

	let get_vm_name_label ~session_id ~uuid =
		let vm = Client.VM.get_by_uuid ~rpc ~session_id ~uuid in
			Client.VM.get_name_label ~rpc ~session_id ~self:vm

	let get_host_name_label ~session_id ~uuid =
		let host = Client.Host.get_by_uuid ~rpc ~session_id ~uuid in
			Client.Host.get_name_label ~rpc ~session_id ~self:host

end

let vm_uuid_to_name_label_map = Hashtbl.create 20
let host_uuid_to_name_label_map = Hashtbl.create 10

let get_vm_name_label vm_uuid =
	if Hashtbl.mem vm_uuid_to_name_label_map vm_uuid then
		Hashtbl.find vm_uuid_to_name_label_map vm_uuid
	else begin
		let name_label, _session_id = XAPI.retry_with_session (fun session_id () ->
			XAPI.get_vm_name_label ~session_id ~uuid:vm_uuid) () in
		Hashtbl.replace vm_uuid_to_name_label_map vm_uuid name_label;
		name_label
	end

let get_host_name_label host_uuid =
	if Hashtbl.mem host_uuid_to_name_label_map host_uuid then
		Hashtbl.find host_uuid_to_name_label_map host_uuid
	else begin
		let name_label, _session_id = XAPI.retry_with_session (fun session_id () ->
			XAPI.get_host_name_label ~session_id ~uuid:host_uuid) () in
		Hashtbl.replace host_uuid_to_name_label_map host_uuid name_label;
		name_label
	end

module Ds_selector = struct
	type t = {
		cf: Rrd.cf_type option;
		owner: Rrd.ds_owner option;
		uuid: string;
		metric: string;
		enabled: bool;
	}

	let empty = { cf=None; owner=None; uuid="";  metric=""; enabled=true }

	let make ?cf ?owner ?(uuid="") ?(enabled=true) metric =
		{ cf; owner; uuid; metric; enabled }

	let of_datasource ?(uuid="") ?owner (ds : Data_source.t) =
		{ empty with owner; uuid;  
			metric=ds.Data_source.name; enabled=ds.Data_source.enabled }

	let of_string str =
		let open Rrd in
			let splitted = Xstringext.String.split ':' str in
			match splitted with
				| [cf; owner; uuid; metric] -> 
					{ empty with 
						cf=(try Some (cf_type_of_string cf) with _ -> None);
						owner=(match owner with 
							| "vm" -> Some (VM uuid)
							| "sr" -> Some (SR uuid)
							| "host" -> Some Host 
							| _ -> None);
						uuid=uuid; metric=metric
					}
				| [metric] -> { empty with metric=metric }
				| _ -> failwith "ds_selector_of_string"
					

	(* R2.5.1. Data-source names containing commas or newline
	   characters should be quoted with double-quotes, as per
	   sections 2.5, 2.6 and 2.7 of RFC4180 *)
	let escape_metric s =
		let quote s = Printf.sprintf "\"%s\"" s in
		if String.contains s '"' then
			quote (Xstringext.String.map_unlikely s 
					   (function '\"' -> Some "\"\"" | _ -> None))
		else if String.contains s ',' || String.contains s '\n'
		then quote s
		else       s
				
	let to_string ?(escaped=false) ds_s =
		let open Rrd in
        let string_repr = Printf.sprintf "%s:%s:%s:%s" 
				(try cf_type_to_string (Opt.unbox ds_s.cf) with Not_found -> "")
				(try match (Opt.unbox ds_s.owner) with 
					| VM _ -> "vm" 
					| Host -> "host" 
					| SR _ -> "sr"
				 with _ -> "")
				ds_s.uuid
				ds_s.metric
				in (if escaped then (escape_metric string_repr) else string_repr)

	let to_string_uuid ?(escaped=false) ds_s =
		to_string ~escaped ds_s

	let to_string_both ?(escaped=false) ds_s =
		let open Rrd in
		let string_repr = Printf.sprintf "%s:%s:%s (%s):%s" 
				(try cf_type_to_string (Opt.unbox ds_s.cf) with Not_found -> "")
				(try match (Opt.unbox ds_s.owner) with 
					| VM _ -> "vm" 
					| Host -> "host" 
					| SR _ -> "sr"
				 with _ -> "")
				ds_s.uuid
				(try match (Opt.unbox ds_s.owner) with
					| VM _ -> get_vm_name_label ds_s.uuid
					| Host -> get_host_name_label ds_s.uuid
					| SR _ -> ""
					with _ -> "")
				ds_s.metric
				in (if escaped then (escape_metric string_repr) else string_repr)

	let to_string_name_label ?(escaped=false) ds_s =
		let open Rrd in
		let string_repr = Printf.sprintf "%s:%s:%s:%s" 
				(try cf_type_to_string (Opt.unbox ds_s.cf) with Not_found -> "")
				(try match (Opt.unbox ds_s.owner) with 
					| VM _ -> "vm" 
					| Host -> "host" 
					| SR _ -> "sr"
					with _ -> "")
				(try match (Opt.unbox ds_s.owner) with
					| VM _ -> get_vm_name_label ds_s.uuid
					| Host -> get_host_name_label ds_s.uuid
					| SR _ -> ""
					with _ -> "")
					ds_s.metric
		in (if escaped then (escape_metric string_repr) else string_repr)

	let to_metric ?(escaped=false) ds_s =
		if escaped then escape_metric ds_s.metric else ds_s.metric

	(* Returns true if d "passes" the filter f, i.e. if fields of d
	   match the non-null fields of f *)
	let filter11 f d =
		true 
		&& (Xstringext.String.startswith f.metric d.metric || f.metric = "")
		&& (f.cf = d.cf || f.cf = None) 
		&& (match f.owner, d.owner with
			| None, _ -> true
			| Some (VM _), Some (VM _) -> f.uuid = d.uuid || f.uuid = ""
			| Some (SR _), Some (SR _) -> f.uuid = d.uuid || f.uuid = ""
			| Some Host, Some Host		 -> true
			| _ -> false
		) 
		&& (f.uuid = d.uuid || f.uuid = "")

	(* Returns true if d "passes" at least one of the filters fs, with
	   the same condition as explained above *)
	let filterN1_OR fs d =
		if fs = [] then true else
			List.fold_left (fun acc f -> acc || (filter11 f d)) false fs
				
	(* Returns the d \in ds that passes at least one of the filters
	   fs *)
	let filterNN_OR fs ds =
		List.fold_left (fun acc d -> 
			if filterN1_OR fs d then d::acc else acc) [] ds

	let filter = filterNN_OR

end

module RRDD = struct
	include Rrd_client.Client
end


module Xport = struct

	(* RRD xport format manipulation module

		 Basic XML xport structure:

		 <xport>
			 <meta>
				 <start>...</start>
				 <step>...</step>
				 <end>...</end>
				 <rows>...</rows>
				 <columns>...</columns>
				 <legend>
						<entry>...</entry>
						...
				 </legend>
			 </meta>
			 <data>
				 <row>
					 <t>...</t>
					 <v>...</v>
					 ...
				 </row>
				 ...
			 </data>
		 </xport>
	*)

	(* Basic XML representation *)

	exception Parse_error of string
	type xml_tree = El of string * xml_tree list | D of string

	(* Xport.t structure *)

	type meta = {
		time_start: int64;
		time_step: int64;
		time_end: int64;
		entries: Ds_selector.t list; (* XXX: remove when merging *)
		(* entries: Ds_selector.t list; *)
	}

	type update = {
		timestamp: int64;
		values: float array;
	}

	type t = {
		header: meta;
		data: update list;
	}

	(* XML converting operations *)

	let from_xml (input: Xmlm.input) =
		(* function taken from Rrd.from_xml *)
		let tree =
			let data d = D d in
			let el ((_prefix,tag_name),_attr) children = El (tag_name, children) in
			match Xmlm.peek input with
			| `Dtd _ -> snd (Xmlm.input_doc_tree ~data ~el input)
			| _ -> Xmlm.input_tree ~data ~el input
		in

		let kvs (elts: xml_tree list) = Listext.List.filter_map
			(function | El (key, [D value]) -> Some (key,value) | El _ | D _ -> None) elts in

		let find_elt (key: string) (elts: xml_tree list) =
			match List.find (function | El(k,_elts) -> k=key | _ -> false) elts with
				| El (_k, elts) -> elts
				| D _ -> raise (Parse_error "find_elt: the element found doesn't contain any elements")
		in

		let process_legend (elts: xml_tree list) =
			List.fold_left (function acc -> function
				| El ("entry", [D value]) -> Ds_selector.of_string value :: acc
				| El _ | D _ -> raise (Parse_error "process_legend")
			) [] (List.rev elts)
		in

		let process_meta (elts: xml_tree list) =
			let kvs = kvs elts in {
				time_start = Int64.of_string (List.assoc "start" kvs);
				time_step = Int64.of_string (List.assoc "step" kvs);
				time_end = Int64.of_string (List.assoc "end" kvs);
				entries = process_legend (find_elt "legend" elts);
			}
		in

		let process_row (elts: xml_tree list) =
			let ts = match (find_elt "t" elts) with
				| [D value] -> Int64.of_string value
				| _ -> raise (Parse_error "process_row/ts")
			in
			let values = List.fold_left (function acc -> function
				| El ("v", [D value]) -> float_of_string value :: acc
				| El ("t", _) -> acc
				| El _ | D _ -> raise (Parse_error "process_row/values")
			) [] (List.rev elts) in
			{ timestamp = ts; values = Array.of_list values; }
		in

		let process_data (elts: xml_tree list) =
			List.fold_left (function acc -> function
				| El ("row", elts) -> process_row elts :: acc
				| El _ | D _ -> raise (Parse_error "process_data")
			) [] (List.rev elts)
		in

		let process_xport (elts: xml_tree list) =
			{
				header = process_meta (find_elt "meta" elts);
				data = process_data (find_elt "data" elts);
			}
		in

		let process_root = function
			| El ("xport", elts) -> process_xport elts
			| El _ | D _ -> raise (Parse_error "process_root")
		in

		try process_root tree
		with Parse_error msg -> failwith ("Parse_error: " ^ msg)


	(* Convert the stream from a file descriptor in a Xport.t value *)
	let of_fd (fd: Unix.file_descr) =
		let ic = Unix.in_channel_of_descr fd in
		let input = Xmlm.make_input (`Channel ic) in
		from_xml input

	(* Data sources filtering *)
	let filter_sources filter (update: t) =
		Ds_selector.filter filter update.header.entries

	(* CSV converting operations *)

	let to_csv_headers (update: t) =
		String.concat ", " (List.map Ds_selector.to_string update.header.entries)

	let to_csv (update: t) =
		let last_update = List.hd update.data in
		Xstringext.String.sub_to_end (Array.fold_left (fun acc v -> 
			let strv = Stdout.string_of_float v in acc ^ ", " ^ strv) 
							   "" last_update.values) 2

	(* Association list operations *)

	module Assoc_list = struct

		type values_list = (Ds_selector.t * float) list

		(* Returns a CSV list of values corresponding to given data_sources
			 R2.6.2. Floating-point numbers must be quoted with a dot (".") for the 
			 radix point
		 *)
		let to_csv ?data_sources (last_values: values_list) =
			let assoc_value ds = Stdout.string_of_float 
				(try List.assoc ds last_values with Not_found -> nan) in
			let filtered_values = match data_sources with
				| None -> List.map (fun (_, v) -> Stdout.string_of_float v) last_values
				| Some dss -> (List.map assoc_value dss) in
			(* Display "N/A" instead of "nan" when DS not available *)
			let filtered_values = List.map (fun str -> 
				if str = "nan" then "N/A" else str) filtered_values in
			String.concat ", " filtered_values

		(* Create an association list (Ds_selector.t -> float) associating a data 
			 source to its last known value *)
		let of_xport (update: t) =
			let last_update = (List.hd update.data) in
			List.mapi (fun i ds -> (ds, last_update.values.(i))) update.header.entries

	end

	(* XPORT updates retrieving function
		 R2.6.3. The data shall be obtained from the RRD daemon via the HTTP interface
	 *)
	let get_update session_id =
		let gen_query ?(host=false) ?(backstep=10.) ?cf ?vm session =
			let now = Unix.gettimeofday () in
			let qstring =
				Printf.sprintf "/rrd_updates?session_id=%s&start=%.0f" 
					(Ref.string_of session) (now -. backstep) in
			let qstring = Opt.fold_left 
				(fun acc vm_uuid -> acc ^ "&vm_uuid=" ^ vm_uuid) qstring vm in
			let qstring = Opt.fold_left
				(fun acc cf -> acc ^ "&cf=" ^ cf) qstring cf in
			let qstring = if host then qstring ^ "&host=true" else qstring in
			Stdout.debug "HTTP Query: %s" qstring;
			qstring
		in
		let transport = (Xmlrpc_client.Unix (Filename.concat "/var/lib/xcp" "xapi")) in
		let request_url = gen_query ~host:true session_id in
		let get_update socket =
			let http_request = Http.Request.make ~user_agent:"rrd2csv" Http.Get request_url in
			Http_client.rpc socket http_request (fun _ fd -> of_fd fd)
		in
		(Xmlrpc_client.with_transport transport get_update)

	(* Accessors *)

	let get_last_timestamp (update: t) =
		(List.hd update.data).timestamp

end

(* let _ = *)
(*	let disabled_user_filters = Ds_selector.filter disabled_dss user_filters in *)
(*	if disabled_user_filters <> [] then *)
(*		Stdout.warn ~oc:stderr (Printf.sprintf "The following metrics cannot be displayed because they are inactive in RRDD: %s" *)
(*									 (String.concat "," (List.map Ds_selector.to_string disabled_user_filters))) *)

(* let filtered_dss = Ds_selector.filter user_filters enabled_dss *)
(* =============================================================================== *)

(* R2.5. Output comma-separated header row of data-source names on stdout *)
let print_header data_sources show_name show_uuid =
	List.map (fun ds ->
		if show_name then
			(if show_uuid then
				Ds_selector.to_string_both ~escaped:true ds
			else
				Ds_selector.to_string_name_label ~escaped:true ds)
		else Ds_selector.to_string_uuid ~escaped:true ds) data_sources
		|> String.concat ", "
		|> Printf.sprintf "timestamp, %s"
		|> Stdout.print

(* R2.6. Output the latest known value for each available metric on stdout in 
	 comma-separated row on stdout *)

let print_last session_id data_sources =
	let last_update = Xport.get_update session_id in
	let assoc_list_of_update = Xport.Assoc_list.of_xport last_update in
	let csv_of_assoc_list = Xport.Assoc_list.to_csv ~data_sources assoc_list_of_update in
	(* R2.6.1. Each row must start with the timestamp of the time at which the 
	   data sources were sampled *)
	Stdout.print (Printf.sprintf "%s, %s" (Stdout.time_of_float (Int64.to_float
	(Xport.get_last_timestamp last_update))) csv_of_assoc_list)
			
let	filter_ds_that_starts_with_name dss name =
	List.fold_left (fun acc ds -> 
		if Xstringext.String.startswith name ds.Ds_selector.metric 
		then ds::acc else acc ) [] dss

open Xport

let main session_id user_filters sampling_period show_name show_uuid =
	let init_update = Xport.get_update session_id in
	let filtered_dss = Xport.filter_sources user_filters init_update in
	let time_step = Int64.to_int init_update.header.time_step in
	(* R2.2.1. Warn if any of the metrics mentioned are disabled *)
	Opt.iter (fun sp -> if time_step > sp then
			Stdout.warn
				(Printf.sprintf
					 "Requested sampling period (%ds) is lower than the period of datasources (%ds)"
					 sp time_step)) sampling_period;
	(* R2.1.2. Warn user if he specifies inactive metrics *)
	List.iter (fun ds -> 
		if filter_ds_that_starts_with_name init_update.header.entries 
			ds.Ds_selector.metric = []
		then Stdout.warn (Printf.sprintf "Requested metric %s is disabled or non-existant" 
							  (Ds_selector.to_string ds))
	) user_filters;
	print_header filtered_dss show_name show_uuid;
	begin
		while true do (* Wake up every sampling period *)
			print_last session_id filtered_dss;
			Unix.sleep (Opt.default time_step sampling_period);
		done
	end

(* Entry point *)
let _ =
	let command = Filename.basename Sys.argv.(0) in
	
	(* R2.1. Ability to specify subset of data-sources of interest on the 
	   command-line
	   R2.2. Ability to specify period of sampling on the command-line (in seconds)
	*)
	let user_filters, sampling_period, show_name, show_uuid =
		let open Xapi_stdext_pervasives.Pervasiveext in
		(* R2.1.1. If none are specified, assume that all enabled data-sources are of 
		   interest *)
		let ds = ref []
		and s = ref None
		and n = ref false
		and u = ref false in
		let usage = Printf.sprintf "%s [-s sampling-period] [datasource_selector]*" command in
		let specs = Arg.align [
			("-s", Arg.Int (fun i -> s := Some i), " period of sampling on the command-line (in seconds)");
			("-v", Arg.Unit (fun () -> Printf.printf "rrd2csv version %s\n(C) Citrix 2012\n" version; exit 0)," output version information and exit");
			("-n", Arg.Unit (fun () -> n := true), " show name labels");
			("-u", Arg.Unit (fun () -> u := true), " show uuids with name labels");
			("--version", Arg.Unit (fun () -> Printf.printf "rrd2csv version %s\n(C) Citrix 2012\n" version; exit 0)," output version information and exit");
			("-help", Arg.Unit (fun () -> ignore_int (Sys.command "man -M /opt/xensource/man rrd2csv"); exit 0), " display help");
			("--help", Arg.Unit (fun () -> ignore_int (Sys.command "man -M /opt/xensource/man rrd2csv"); exit 0), " display help")
		] in
		let ano d = ds := (Ds_selector.of_string d) :: !ds in
		Arg.parse specs ano usage;
		!ds, !s, !n, (if !n then !u else true) in (* show UUIDs by default;
		disable them if name labels are shown, unless explicitly requested *)
		XAPI.retry_with_session (fun session_id () -> main session_id
		user_filters sampling_period show_name show_uuid) ()
