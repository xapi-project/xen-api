type keylogger = {
  mutable debug : string list;
  mutable info : string list;
  mutable warn : string list;
  mutable error : string list;
  no_default : bool;
}
val __all_loggers : (string, Log.t) Hashtbl.t
val __default_logger : keylogger
val __log_mapping : (string, keylogger) Hashtbl.t
val get_or_open : string -> Log.t
val add : string -> string list -> unit
val get_by_level : keylogger -> Log.level -> string list
val set_by_level : keylogger -> Log.level -> string list -> unit
val set : string -> Log.level -> string list -> unit
val set_default : Log.level -> string list -> unit
val append : string -> Log.level -> string -> unit
val append_default : Log.level -> string -> unit
val reopen : unit -> unit
val reclaim : unit -> unit
val clear : string -> Log.level -> unit
val clear_default : Log.level -> unit
val reset_all : string list -> unit
val log_common :
  string ->
  Log.level ->
  ?extra:string ->
  ret_fn1:(string -> 'a) ->
  ret_fn2:(Log.t list -> 'b) -> ('b, unit, string, 'a) format4 -> 'b
val log :
  string ->
  Log.level -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a
val log_and_return :
  string ->
  Log.level ->
  ?raw:bool -> syslog_time:bool -> ?extra:string -> ('a, unit, string, string) format4 -> 'a
val debug : string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a
val info : string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a
val warn : string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a
val error : string -> ?extra:string -> ('a, unit, string, unit) format4 -> 'a
val audit :
  string ->
  ?raw:bool -> ?extra:string -> ('a, unit, string, string) format4 -> 'a
