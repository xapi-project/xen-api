(** HTTP handler for v2 of the remote DB access protocol *)
val handler: Http.request -> Buf_io.t -> unit
