(* start a service with systemctl *)
val start : ?timeout:float -> wait_until_success:bool -> string -> unit

(* stop a service with systemctl *)
val stop : ?timeout:float -> wait_until_success:bool -> string -> unit
