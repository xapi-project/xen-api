(*
 * HTTP handler for connecting to a VM's VNC console.
 * Handler should be passed a reference to either a VM (in which case the 'default' VNC
 * console will be chosen) or a console object.
 *)

open Http

module D = Debug.Debugger(struct let name="console" end)
open D

exception Failure

let real_proxy __context console s = 
  (* Only works with VNC for now *)
  let vnc_port = Int64.to_int (Db.Console.get_port ~__context ~self:console) in
  (* let vnc_port = 22 in *)
  debug "attempting to connect to port %d" vnc_port;
  begin try
      let vnc_sock = Unixext.open_connection_fd "127.0.0.1" vnc_port in
      (* Unixext.proxy closes fds itself so we must dup here *)
      let s' = Unix.dup s in
      debug "Connected; running proxy (between fds: %d and %d)" 
	(Unixext.int_of_file_descr vnc_sock) (Unixext.int_of_file_descr s');
      Unixext.proxy vnc_sock s';
      debug "Proxy exited"
    with
      exn -> debug "error: %s" (ExnHelper.string_of_exn exn)
  end

let fake_proxy __context console s = 
  Rfb_randomtest.server s

let default_console_of_vm ~__context ~self = 
  try
    let consoles = Db.VM.get_consoles ~__context ~self in
    let protocols = List.map (fun self -> Db.Console.get_protocol ~__context ~self) consoles in
    fst (List.find (fun (_, p) -> p = `rfb) (List.combine consoles protocols))
  with _ ->
    error "Failed to find default VNC console for VM";
    raise Failure

let console_of_request __context req = 
  (* First check the request looks valid *)
  if not(List.mem_assoc "ref" req.query) && not(List.mem_assoc "uuid" req.query) then begin
    error "HTTP request for console forwarding lacked 'ref' or 'uuid' parameter";
    raise Failure
  end;
  let _ref = 
    if List.mem_assoc "uuid" req.query 
    then 
      let uuid = List.assoc "uuid" req.query in
      (try Ref.string_of(Db.VM.get_by_uuid ~__context ~uuid)
       with _ -> Ref.string_of(Db.Console.get_by_uuid ~__context ~uuid))
    else List.assoc "ref" req.query in

  (* The _ref may be either a VM ref in which case we look for a
     default VNC console or it may be a console ref in which case we
     go for that. *)
  let is_vm = try ignore(Db.VM.get_uuid ~__context ~self:(Ref.of_string _ref)); true with _ -> false in
  let is_console = try ignore(Db.Console.get_uuid ~__context ~self:(Ref.of_string _ref)); true with _ -> false in
  if not(is_vm) && not(is_console) then begin
    error "Reference was not for a VM or a console";
    raise Failure
  end;
  if is_vm then default_console_of_vm ~__context ~self:(Ref.of_string _ref) else (Ref.of_string _ref) 
    

let rbac_check_for_control_domain __context (req:request) console_id permission =
	let is_control_domain =
		let vm_id = Db.Console.get_VM ~__context ~self:console_id in
		Db.VM.get_is_control_domain ~__context ~self:vm_id
	in
	if is_control_domain then
		let extra_dmsg = Printf.sprintf "for host console %s" (Ref.string_of console_id) in
		let session_id = Xapi_http.get_session_id req in
		Rbac.check_with_new_task ~extra_dmsg session_id permission ~fn:Rbac.nofn


(* GET /console_uri?ref=.....
   Cookie: <session id> *)
let handler proxy_fn (req: request) s =
  req.close := true;
  debug "handler: fd = %d" (Unixext.int_of_file_descr s);
  Xapi_http.with_context "Connection to VM console" req s
    (fun __context ->
      let console = console_of_request __context req in
      (* only sessions with 'http/connect_console/host_console' permission *)
      (* can access dom0 host consoles *)
      rbac_check_for_control_domain __context req console
        Rbac_static.permission_http_connect_console_host_console.Db_actions.role_name_label;
      Http_svr.headers s (Http.http_200_ok ());
      
      proxy_fn __context console s)
