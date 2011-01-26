(** HTTP handler for v1 of the remote DB access protocol *)
val handler: Http.request -> Buf_io.t -> unit
