open Http
open Pervasiveext
open Forkhelpers

module D = Debug.Debugger(struct let name="xapi" end)
open D

let logs_download = "/opt/xensource/libexec/logs-download"

let logs_download_handler (req: request) s = 
  debug "running logs-download handler";
  Xapi_http.with_context "Downloading host logs" req s
    (fun __context ->
      Http_svr.headers s (Http.http_200_ok ());
      
      debug "send the http headers";
      let fd = string_of_int (Unixext.int_of_file_descr Unix.stdout) in
      let pid = safe_close_and_exec [ Dup2(s, Unix.stdout) ] 
	[ Unix.stdout ] logs_download [] in
      waitpid pid)
