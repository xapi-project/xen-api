(* (C) Copyright XenSource 2007 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

let support_url = "ftp://support.xensource.com/uploads/"

let upload_wrapper = "/opt/xensource/libexec/upload-wrapper"

(* URL to which the crashdump/whatever will be uploaded *)
let upload_url name = 
  let uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  Printf.sprintf "%s%s-%s" support_url uuid name

open Forkhelpers

let do_upload label file url options = 
  let proxy = 
    if List.mem_assoc "http_proxy" options
    then List.assoc "http_proxy" options
    else try Unix.getenv "http_proxy" with _ -> "" in

  match with_logfile_fd label
  (fun log_fd ->
     let pid = safe_close_and_exec 
       [ Close(Unix.stdin);
	 Dup2(log_fd, Unix.stdout);
	 Dup2(log_fd,Unix.stderr) ]
       [ Unix.stdin; Unix.stdout; Unix.stderr ] upload_wrapper [file; url; proxy] in
     waitpid pid) with
  | Success _ -> debug "Upload succeeded"
  | Failure (log, exn) ->
      debug "Upload failed, output: %s" log;
      raise exn

