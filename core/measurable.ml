module Description = struct
  type t = {
    description: string;
    units: string;
  }
end

module Measurement = struct
  type v =
  | Int of int
  | Bool of bool
  | Float of float

  type t = {
    v: v;
    ns: int64; (** time in ns *)
  }
end

module type S = sig
  val list_available: unit -> (string * Description.t) list

  val measure: string -> Measurement.t option
end


