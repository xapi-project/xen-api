type variant = {
    version: string
  ; hw_present: bool
  ; priority: float
  ; dev_status: string
}

type driver = {
    ty: string
  ; name: string
  ; descr: string
  ; info: string
  ; selected: string option
  ; active: string option
  ; variants: (string * variant) list
}

type t = {protocol: string; operation: string; drivers: (string * driver) list}

val parse : string -> (string * driver) list

val read : string -> (string * driver) list

module Mock : sig
  val install : unit -> unit
end
