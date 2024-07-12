val add : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val list : Common.t -> unit

val stat_vm : 'a -> string -> [> `Ok of unit]

val diagnostics : 'a -> [> `Ok of unit]

val remove : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val export :
     'a
  -> bool
  -> bool
  -> string option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val import :
  'a -> bool -> string option -> [> `Error of bool * string | `Ok of unit]

val shutdown :
     'a
  -> float option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val pause : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val unpause : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val reboot :
     'a
  -> float option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val suspend :
     'a
  -> string option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val resume :
     'a
  -> string option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val console_connect :
  'a -> string option -> [> `Error of bool * string | `Ok of unit]

val start :
     'a
  -> bool
  -> bool
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val create :
  'a -> string option -> bool -> [> `Error of bool * string | `Ok of unit]

val cd_eject : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val cd_insert : string -> string -> string

val events : 'a -> 'b

val task_list : 'a -> [> `Ok of unit]

val task_cancel :
  'a -> string option -> [> `Error of bool * string | `Ok of unit]
