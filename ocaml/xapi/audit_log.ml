module D = Debug.Debugger(struct let name="audit_log" end)
open D

open Http

(* 
  Assume that RBAC access for the session_id already verified by xapi_http.ml
  
  GET /audit_log?session_id=<session>&task_id=<task>&
                   report=<report name>&<param1>=<value1>&...
*)
let handler (req: request) (bio: Buf_io.t) =
	Http_svr.default_callback req bio
	
