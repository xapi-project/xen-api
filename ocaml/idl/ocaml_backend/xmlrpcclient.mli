(* ------------------------------------------------------------------

   Copyright (c) 2006 Xensource Inc

   Contacts: Dave Scott    <dscott@xensource.com>
             Jon Ludlam    <jludlam@xensource.com>

   Simple HTTP 1.0/1.1 and XMLRPC client functions

   ------------------------------------------------------------------- *)

(** Thrown when an explicit HTTP rejection is received (although note we don't
    parse enough of the response to be sure... but it was non-empty at least) *)
exception Http_request_rejected of string

exception Content_length_required

(** Thrown when ECONNRESET is caught which suggests the remote crashed or restarted *)
exception Connection_reset

(** Thrown when no data is received from the remote HTTP server. This could happen if
    (eg) an stunnel accepted the connection but xapi refused the forward causing stunnel
    to immediately close. *)
exception Empty_response_from_server

(** Thrown when repeated attempts to connect an stunnel to a remote host and check
    the connection works fail. *)
exception Stunnel_connection_failed

val set_stunnelpid_callback : (string -> int -> unit) option ref

val connect_headers : ?session_id:string -> ?task_id:string -> ?subtask_of:string -> string -> string -> string list
val xmlrpc_headers : ?task_id:string -> ?subtask_of:string -> version:string -> string -> string -> int -> string list
val http_rpc_fd : Unix.file_descr -> string list -> string -> int * string option
val http_rpc_fd_big_query : Unix.file_descr -> string list -> Bigbuffer.t -> int * string option
val read_xml_rpc_response : int -> 'a -> Unix.file_descr -> Xml.xml

val do_secure_http_rpc : ?use_external_fd_wrapper : bool -> ?use_stunnel_cache: bool -> ?verify_cert : bool -> ?task_id:string -> host:string -> port:int -> ?unixsock : Unix.file_descr option -> headers:string list -> body:string -> (int -> string option -> Unix.file_descr -> 'a) -> 'a
val do_http_rpc : string -> int -> ?unixsock:string option -> string list -> string -> (int -> string option -> Unix.file_descr -> 'a) -> 'a

val do_xml_rpc : ?task_id:string -> ?subtask_of:string -> version:string -> host:string -> port:int -> path:string -> Xml.xml -> Xml.xml
val do_secure_xml_rpc : ?task_id:string -> ?subtask_of:string -> ?use_external_fd_wrapper : bool -> ?use_stunnel_cache : bool -> version:string -> host:string -> port:int -> path:string -> Xml.xml -> Xml.xml
val do_xml_rpc_unix : ?task_id:string -> ?subtask_of:string -> version:string -> filename:string -> path:string -> Xml.xml -> Xml.xml

val do_xml_rpc_persistent : host:string -> path:string -> Unix.file_descr -> Xml.xml -> Xml.xml 

val check_reusable : Unix.file_descr -> bool

val get_reusable_stunnel :   ?use_external_fd_wrapper:bool ->
  ?write_to_log:(string -> unit) -> string -> int -> Stunnel.t

val write_to_log : string -> unit
