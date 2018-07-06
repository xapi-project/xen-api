module Protocol: sig
  type t = Vt100
  val to_string: t -> string
end

module Output: sig
  type t = Pty
  val to_string: t -> string
end

module RingInfo: sig
  type t = {
      ref: int32;
      event_channel: int;
    }
  val to_string: t -> string
  val keys: string list
  val to_assoc_list: t -> (string * string) list
  val of_assoc_list: (string * string) list -> (t, string) result
end

module State: sig
  type t = Initialising | InitWait | Initialised | Connected | Closing | Closed
  val to_string: t -> string
  val of_string: string -> t option
  val _state: string
  val keys: string list
  val to_assoc_list: t -> (string * string) list
  val of_assoc_list: (string * string) list -> (t, string) result
end

module Connection: sig
  type t = {
      virtual_device: int;
      backend_path: string;
      backend_domid: int;
      frontend_path: string;
      frontend_domid: int;
      protocol: Protocol.t;
      name: string option; (** protocol extension to name consoles *)
    }
  val to_assoc_list: t -> (Xs_protocol.ACL.t * (string * string)) list
  val make:
    ?protocol:Protocol.t ->
    ?backend_domid:int ->
    ?name:string -> frontend_domid:int -> virtual_device:int -> unit -> t
end
