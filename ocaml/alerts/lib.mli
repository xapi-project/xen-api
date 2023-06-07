val seconds_per_day : float

val days_until_expiry : float -> float -> int

val generate_alert :
     ('a -> Xapi_stdext_date.Date.t)
  -> ((string -> string) -> int -> 'a -> 'b)
  -> float
  -> 'a
  -> 'b

module type AlertSource = sig
  type t

  val get_expiry : t -> Xapi_stdext_date.Date.t

  val get_attrs_body_and_name :
    (string -> string) -> int -> t -> t * (string * (string * int64)) option

  val filter_possible_messages : [`message] Ref.t * API.message_t -> bool

  val get_item : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> t list

  val get_cls_and_uuid :
       rpc:(Rpc.call -> Rpc.response)
    -> session_id:[< `session] Ref.t
    -> t
    -> [> `Certificate
       | `Host
       | `PVS_proxy
       | `Pool
       | `SR
       | `VDI
       | `VM
       | `VMPP
       | `VMSS ]
       * string
end

module type Alerter = sig
  val alert : (Rpc.call -> Rpc.response) -> [< `session] Ref.t -> unit
end

module Make : functor (Source : AlertSource) -> Alerter
