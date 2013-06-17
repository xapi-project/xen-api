(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

open Cohttp_lwt_unix
open Lwt
open Protocol
open Protocol_lwt

let basedir = ref "/tmp/link_test"

let rpc_req = { Message.payload = "hello"; kind = Message.Request "reply to" }
let rpc_res = { Message.payload = "hello"; kind = Message.Response ("q", 1L) }

let in_frames =
	let open In in [
		"login", Login "hello";
		"persistent", CreatePersistent "service";
		"transient", CreateTransient "client";
		"request", Send("service", rpc_req);
		"reply", Send("service", rpc_res);
		"transfer", Transfer { from = Some "3"; timeout = 5.; queues = ["one"; "two"]};
		"ack", Ack ("q", 3L);
	]

let out_frames =
	let open Out in [
		"create.reply", Create "service";
		"transfer.reply", Transfer { messages = [
			("q", 1L), rpc_req;
			("q2", 2L), rpc_res;
		]; next = "0" }
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
