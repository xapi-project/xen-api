val schema_major_vsn : int
val schema_minor_vsn : int

val last_release_schema_major_vsn : int
val last_release_schema_minor_vsn : int


(* Some of the string names are special: *)
val _session : string
val _task : string
val _event : string
val _vm : string
val _self : string
val _network : string
val _vif : string
val _vdi : string
val _vbd : string
val _sr : string
val _host : string
val _user : string
val _host_metrics : string
val _host_crashdump : string
val _hostcpu : string
val _vm_metrics : string
val _vm_guest_metrics : string
val _vif_metrics : string
val _pif : string
val _pif_metrics :string
val _pbd : string
val _vbd_metrics : string
val _vtpm : string
val _console : string
val _crashdump : string
val _message : string

val all_api : Dm_api.api
val errors : (string, Datamodel_types.error) Hashtbl.t

val whitelist : (Datamodel_types.obj * Datamodel_types.message) list
val emergency_calls : (Datamodel_types.obj * Datamodel_types.message) list
val no_async_messages_for : string list
val expose_get_all_messages_for : string list
val no_task_id_for : string list
val current_operations_for : string list

type action_arg =
   String_query_arg of string |
   Int64_query_arg of string |
   Bool_query_arg of string |
   Varargs_query_arg
type http_meth = Get | Put | Post | Connect
val http_actions : (string * (http_meth * string * bool * action_arg list)) list
