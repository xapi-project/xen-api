module D = Debug.Debugger(struct let name="audit_log" end)
open D

open Http
open Stringext

let audit_log_whitelist_prefix = "/var/log/audit.log"

let line_timestamp_length = 21 (* the timestamp length at the debug line *)
let line_before_timestamp_length = 1 (* [ at the beginning of the line *)

let write_line line fd since =
	if String.length line > 
		(line_timestamp_length + line_before_timestamp_length)
	then
	let line_timestamp =
		String.sub line line_before_timestamp_length line_timestamp_length
	in
	if since="" or ((String.compare line_timestamp since) >= 0)
	then
	let len = String.length line in
	ignore(Unix.write fd line 0 len)

let transfer_audit_file _path fd_out since =
	let path = Unixext.resolve_dot_and_dotdot _path in
	if (String.startswith audit_log_whitelist_prefix path)
		&& (Unixext.file_exists path)
	then
	try
		Unixext.readfile_line
			(fun line -> write_line (line^"\n") fd_out since)
			path
	with e -> begin
		debug "error reading audit log file %s: %s" path (ExnHelper.string_of_exn e)
	end

let transfer_all_audit_files fd_out since =
	(* go through audit.log.n->0 first, ascending order of time *)
	for i=100 downto 0 do
		transfer_audit_file
			(audit_log_whitelist_prefix^"."^(string_of_int i))
			fd_out
			since
	done;
	(* finally transfer /var/log/audit.log (the latest one in time) *)
	transfer_audit_file audit_log_whitelist_prefix fd_out since


(* map the ISO8601 timestamp format into the one in our logs *)
let log_timestamp_of_iso8601 iso8601_timestamp =
	let step0 = iso8601_timestamp in
	let step1 = Stringext.String.replace "T" " " step0 in
	let step2 = Stringext.String.replace "-" "" step1 in
	let step3 = Stringext.String.replace "Z" "" step2 in
	step3

(*
 Assume that RBAC access for the session_id already verified by xapi_http.ml
 
 GET /audit_log?session_id=<session>&task_id=<task>&
                [since=<timestamp in ISO 8601 / log.gettimestring() format>]

 eg. /audit_log?...&since=20090910T11:31:11.417
 eg. /audit_log?...&since=20090910%2011:31:11.417
 eg. /audit_log?...&since=2009-09-10T11:31:11.417Z
 eg. /audit_log?...&since=2009-09-10%2011:31:11.417
 eg. /audit_log?...&since=2009-09-10T11:31
 eg. /audit_log?...&since=2009-09-10
*)
let handler (req: request) (bio: Buf_io.t) =

	let s = Buf_io.fd_of bio in
	Buf_io.assert_buffer_empty bio;
	req.close := true;

	Xapi_http.with_context (* makes sure to signal task-completed to cli *)
		(Printf.sprintf "audit_log_get request")
		req
		s
		(fun __context ->

			 let all = req.cookie @ req.query in
			 let since_iso8601 =
				 if List.mem_assoc "since" all then List.assoc "since" all else ""
			 in
			 let since = log_timestamp_of_iso8601 since_iso8601 in
			 (*debug "since=[%s]" since;*)
			 (* we need to return an http header without content-length *)
			 Http_svr.headers s (http_200_ok() @ [ "Content-Type: text/plain"]);
			 (* then the contents *)
			 transfer_all_audit_files s since
		)

