(** Unix file descriptor and metadata *)
type raw

(** A Unix file descriptor wrapped with protection against double-close and
    resource leak *)
type t = raw Safe.t

val borrow_exn : t -> Unix.file_descr
(** [borrow_exn t] gives temporary access to the underlying Unix file
    descriptor. See {!val:Safe.borrow_exn} for details. It is important to keep
    file descriptors in their [Unixfd.t] form for as long as possible and borrow
    them as close as possible to calling low-level [Unix] functions to maximize
    the benefit of double-close prevention and leak tracking. *)

val ( ! ) : t -> Unix.file_descr
(** (!t) is a shorthand for {!borrow_exn} *)

val with_pipe : unit -> loc:string -> (t -> t -> 'b) -> 'b
(** [with_pipe ~loc:__LOC__ () f] creates a pipe and executes [f] with the 2
    endpoints. The 1st file descriptor is the read end, the 2nd the write end.
    [loc] should be set to [__LOC__] to aid in debugging any leaks. When [f]
    finishes (whether successfully or by throwing an exception) the pipe is
    always closed. *)

val with_socketpair :
     Unix.socket_domain
  -> Unix.socket_type
  -> int
  -> loc:string
  -> (t -> t -> 'b)
  -> 'b
(** [with_socketpair domain type port ~loc:__LOC__ f] is like {!with_pipe} but
    for socket pairs. *)

val with_open_connection : Unix.sockaddr -> loc:string -> (t -> 'b) -> 'b
(** [with_open_connection addr ~loc:__LOC__ f] opens a connection to the address
    [addr], executes [f] and closes the connection always *)

val with_channels : t -> (in_channel * out_channel -> 'a) -> 'a
(** [with_channels t f] opens a pair of input/output channels associated with
    [t]. The original [t] cannot be used anymore, I/O can only be performed on
    the channels, see {!Safe.move_exn}. Each channels is operating on a [dup] of
    the original FD, so they can be closed independently and are not subject to
    double close bugs. When [f] finishes both channels are closed. *)

val safe_close : t -> unit
(** [safe_close t] is a convenient way of writing {!Safe.safe_release} [t].
    Calling this is idem-potent, there is no risk of double close. *)
