open Cohttp_lwt_unix
open Lwt
open Protocol
open Protocol_lwt

let basedir = ref "/tmp/link_test"

let rpc_req = { Message.payload = "hello"; correlation_id = 1; reply_to = Some "reply_to" }
let rpc_res = { Message.payload = "hello"; correlation_id = 1; reply_to = None }

let in_frames =
	let open In in [
		"login", Login "hello";
		"create", Create (Some "service");
		"create.anon", Create None;
		"subscribe", Subscribe "service";
		"request", Send("service", rpc_req);
		"reply", Send("service", rpc_res);
		"transfer", Transfer(3L, 5.);
		"ack", Ack 3L;
	]

let out_frames =
	let open Out in [
		"create.reply", Create "service";
		"transfer.reply", Transfer { messages = [
			1L, rpc_req;
			2L, rpc_res;
		] }
	]

let make_file name f =
	lwt fd = Lwt_unix.openfile (Filename.concat !basedir name) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
	let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
	try_lwt
		lwt () = f oc in
		lwt () = Lwt_io.flush oc in
		return ()
	finally
		Lwt_unix.close fd

let main () =
	lwt () =
		Lwt_list.iter_s
			(fun (name, in_frame) ->
				make_file name
					(fun oc ->
						let body, meth, uri = In.to_request in_frame in
						let body = match body with None -> "" | Some x -> x in
						let lines = [
							Printf.sprintf "%s %s HTTP/1.1" (Cohttp.Code.string_of_method meth) (Uri.to_string uri);
							Printf.sprintf "Content-Length: %d" (String.length body);
							"";
							body
						] in
						Lwt_io.write oc (String.concat "\r\n" lines)
					)
			) in_frames in
	lwt () =
		Lwt_list.iter_s
			(fun (name, out_frame) ->
				make_file name
					(fun oc ->
						let code, body = Out.to_response out_frame in
						let lines = [
							Printf.sprintf "HTTP/1.1 %s" (Cohttp.Code.string_of_status code);
							Printf.sprintf "Content-Length: %d" (String.length body);
							"";
							body
						] in
						Lwt_io.write oc (String.concat "\r\n" lines)
					)
			) out_frames in
	return ()

let _ =
	Arg.parse [
		"-dir", Arg.Set_string basedir, "Directory to place protocol fragments";
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Test the parser/printer for the link-layer protocol";

	Lwt_unix.run (main ()) 
