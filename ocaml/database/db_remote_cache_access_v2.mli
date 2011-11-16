(** HTTP handler for v2 of the remote DB access protocol *)
val handler: Http.Request.t -> Buf_io.t -> 'a -> unit
