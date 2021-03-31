val update :
  __context:Context.t -> (unit, [> `Msg of string * 'a list]) Result.result
(** inspect the host's certificate in /etx/xensource/xapi_ssl.pem
(default) and update it's database entry in case it has changed. In
effect this creates a new entry and removes the stale entry. *)
