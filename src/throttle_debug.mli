(** Debugging for {!Throttle}. *)

module Debug (Throttle : module type of Throttle) : module type of Throttle
