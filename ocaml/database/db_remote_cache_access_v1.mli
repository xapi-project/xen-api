(** HTTP handler for v1 of the remote DB access protocol *)
val handler: Http.Request.t -> Buf_io.t -> 'a -> unit
