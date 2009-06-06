
type handler = {
  name: string;                                  (** used for naming the thread *)
  body: Unix.sockaddr -> Unix.file_descr -> unit (** function called in a thread for each connection*)
}

type server = { 
  shutdown : unit -> unit                        (** clean shutdown, blocks until thread has gone *)
}

(** Creates a server given a bound socket and a handler *)
val server : handler -> Unix.file_descr -> server

