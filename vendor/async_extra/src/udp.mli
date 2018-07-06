(** A grab-bag of performance-oriented, UDP-oriented network tools.  These provide some
    convenience, but they are more complex than basic applications require.

    Defaults are chosen for typical UDP applications.  Buffering is via [Iobuf]
    conventions, where a typical packet-handling loop iteration is
    read -> [flip_lo] -> process -> [reset].

    While these functions are oriented toward UDP, they work with any files that satisfy
    [Fd.supports_nonblock].

    For zero-copy [Bigstring.t] transfers, we must ensure no buffering between the receive
    loop and caller.  So an interface like [Tcp.connect], with something like
    [(Bigstring.t * Socket.Address.Inet.t) Pipe.Reader.t], won't work. Instead, we use
    synchronous callbacks. *)

open! Core
open! Import

type write_buffer = (read_write, Iobuf.seek) Iobuf.t

(** The default buffer capacity for UDP-oriented buffers is 1472, determined as the
    typical Ethernet MTU (1500 octets) less the typical UDP header length (28).  Using
    buffers of this size, one avoids accidentally creating messages that will be dropped
    on send because they exceed the MTU, and can receive the largest corresponding UDP
    message.

    While this number is merely typical and not guaranteed to work in all cases, defining
    it in one place makes it easy to share and change.  For example, another MTU in common
    use is 9000 for Jumbo frames, so the value of [default_capacity] might change to 8972
    in the future. *)
val default_capacity : int

(** A typical receive loop implicitly calls [Iobuf.flip_lo] before calling its callback to
    prepare a packet buffer for reading by the callback and [Iobuf.reset] afterward to
    prepare for the next iteration.

    It's often convenient to use the same interface for UDP, TCP, and file variants of the
    same protocol.

    [stop] terminates a typical loop as soon as possible, when it becomes determined.

    [max_ready] limits the number of receive loop iterations within an [Fd.every_ready_to]
    iteration, to prevent starvation of other Async jobs and to allow bounded
    busy-waiting, as on busy network sockets. *)
module Config : sig
  type t =
    { capacity  : int
    ; init      : write_buffer
    ; stop      : unit Deferred.t
    ; max_ready : int
    }
  [@@deriving fields]

  val create
    :  ?capacity  : int                 (** default is [default_capacity] *)
    -> ?init      : write_buffer        (** default is [Iobuf.create ~len:capacity] *)
    -> ?stop      : (unit Deferred.t)   (** default is [Deferred.never] *)
    -> ?max_ready : int                 (** default is [12] *)
    -> unit
    -> t
end

(** [sendto_sync sock buf addr] does not try again if [sock] is not ready to write.
    Instead, it returns [EWOULDBLOCK] immediately.

    Short writes are distinguished by [buf] not being empty afterward.

    See also {!Iobuf.sendto_nonblocking_no_sigpipe} and
    {!Bigstring.sendto_nonblocking_no_sigpipe}.

    @raise Failure on internal errors but return [Unix.error] via
    [Unix.Syscall_result.Unit.t] rather than raising [Unix_error]. *)
val sendto_sync
  :  unit
  -> (Fd.t
      -> ([> read ], Iobuf.seek) Iobuf.t
      -> Socket.Address.Inet.t
      -> Unix.Syscall_result.Unit.t
     ) Or_error.t

(** [send_sync sock buf] has identical semantics to [sendto_sync], but is intended for
    connected UDP sockets (and therefore does not require a "to" address).

    See also
    {!Iobuf.send_nonblocking_no_sigpipe} and
    {!Bigstring.send_nonblocking_no_sigpipe}.

    @raise Failure on internal errors but return [Unix.error] via
    [Unix.Syscall_result.Unit.t] rather than raising [Unix_error]. *)
val send_sync
  :  unit
  -> (Fd.t -> ([> read ], Iobuf.seek) Iobuf.t -> Unix.Syscall_result.Unit.t) Or_error.t


(** [sendto sock buf addr] retries if [sock] is not ready to write.

    @raise Unix_error in the case of Unix output errors and [Failure] on internal
    errors. *)
val sendto
  :  unit
  -> (Fd.t
      -> ([> read ], Iobuf.seek) Iobuf.t
      -> Socket.Address.Inet.t
      -> unit Deferred.t
     ) Or_error.t

(** [send sock buf] retries if [sock] is not ready to write.

    @raise Unix_error in the case of Unix output errors and [Failure] on internal
    errors. *)
val send
  :  unit
  -> (Fd.t
      -> ([> read ], Iobuf.seek) Iobuf.t
      -> unit Deferred.t
     ) Or_error.t


(** [bind address] creates a socket bound to address, and, if [address] is a
    multicast address, joins the multicast group. *)
val bind
  :  ?ifname : string
  -> Socket.Address.Inet.t
  -> ([ `Bound ], Socket.Address.Inet.t) Socket.t

val bind_any : unit -> ([ `Bound ], Socket.Address.Inet.t) Socket.t

module Loop_result : sig
  type t = Closed | Stopped [@@deriving sexp_of, compare]

  (** As the name implies, this is useful for other modules that want to compatibly
      convert the result of {!Fd.interruptible_ready_to} to a loop result or exception. *)
  val of_fd_interruptible_every_ready_to_result_exn
    :  (_, _) Iobuf.t option
    -> string
    -> 'a
    -> ('a -> Sexp.t)
    -> [ `Bad_fd | `Closed | `Unsupported | `Interrupted ]
    -> t
end

(** Loops, including [recvfrom_loop], terminate normally when the socket is closed. *)
val recvfrom_loop
  :  ?config : Config.t
  -> Fd.t
  -> (write_buffer -> Socket.Address.Inet.t -> unit)
  -> Loop_result.t Deferred.t

(** [recvfrom_loop_with_buffer_replacement callback] calls [callback] synchronously on
    each message received.  [callback] returns the packet buffer for subsequent
    iterations, so it can replace the initial packet buffer when necessary.  This enables
    immediate buffer reuse in the common case and fallback to allocation if we want to
    save the packet buffer for asynchronous processing. *)
val recvfrom_loop_with_buffer_replacement
  :  ?config : Config.t
  -> Fd.t
  -> (write_buffer -> Socket.Address.Inet.t -> write_buffer)
  -> Loop_result.t Deferred.t

val read_loop
  :  ?config : Config.t
  -> Fd.t
  -> (write_buffer -> unit)
  -> Loop_result.t Deferred.t
val read_loop_with_buffer_replacement
  :  ?config : Config.t
  -> Fd.t
  -> (write_buffer -> write_buffer)
  -> Loop_result.t Deferred.t

(** [recvmmsg_loop ~socket callback] iteratively receives up to [max_count] packets at a
    time on [socket] and passes them to [callback].  Each packet is up to [Iobuf.capacity]
    bytes.

    [callback bufs ~count] processes [count] packets synchronously.

    [Config.init config] is used as a prototype for [bufs] and as one of the elements. *)
val recvmmsg_loop
  : (?config           : Config.t       (** default is [Config.create ()] *)
     -> ?max_count     : int            (** default is [default_recvmmsg_loop_max_count],
                                            which is 32 now *)
     -> ?on_wouldblock : (unit -> unit) (** callback if [recvmmsg] would block *)
     -> Fd.t
     -> (write_buffer array -> count : int -> unit)
     -> Loop_result.t Deferred.t)
      Or_error.t
val default_recvmmsg_loop_max_count : int
