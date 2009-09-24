open Http
open Pervasiveext
open Forkhelpers

module D = Debug.Debugger(struct let name="xapi" end)
open D

let vncsnapshot = "/usr/bin/vncsnapshot"


let vncsnapshot_handler (req: request) s =
  debug "vncshapshot handler running";
  Xapi_http.with_context "Taking snapshot of VM console" req s
    (fun __context ->
       try
	 let console = Console.console_of_request __context req in
	 Console.rbac_check_for_control_domain __context req console
     Rbac_static.permission_http_get_vncsnapshot_host_console.Db_actions.role_name_label;
	 let tmp = Filename.temp_file "snapshot" "jpg" in
	 finally
	   (fun () ->
	      let vnc_port = Int64.to_int (Db.Console.get_port ~__context ~self:console) in
	      
	    let pid = safe_close_and_exec [ ] [ ]
	      vncsnapshot [ "-quiet"; "-encodings"; "\"raw\"";
			    Printf.sprintf "%s:%d" "127.0.0.1" (vnc_port-5900); tmp ] in
	    waitpid pid;
	    Http_svr.response_file ~mime_content_type:None s tmp
	   )
	   (fun () -> try Unix.unlink tmp with _ -> ())
       with e ->
	 req.close := true;
	 raise e
    )
