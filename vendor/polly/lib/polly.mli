(**
  Module Polly provides access to the Linux epoll system call for
  monitoring a set of file descriptors for events that the client is
  interested in.
 *)

type t

val create : unit -> t
(** [create ()] returns an epoll(2) file descriptor which is passed
 * later to [wait], [add], [upd], and [del]. It must be passed to
 * [close] when no longer needed.
 *)

val close: t -> unit
(** [close t] closes the file descriptor underlying [t] *)

module Events : sig
  (** a set of events *)
  type t

  val empty : t
  (** empty set *)

  (* The values below define singleton sets containing exactly one
   * event like [inp] (input) or [hup]. See epoll_ctl(2) for the events
   * available. Sets can be combined using [land] (intersection) and
   * [lor] (join) and compared using [(=)].
   *)

  val inp : t
  (** called "in" in Linux; "in" is an OCaml keyword  *)

  val pri : t

  val out : t

  val rdnorm : t

  val rdband : t

  val wrnorm : t

  val wrband : t

  val msg : t

  val err : t

  val hup : t

  val rdhup : t

  val wakeup : t

  val oneshot : t

  val et : t

  (* val exclusive : t *)

  val ( lor ) : t -> t -> t
  (** join sets *)

  val ( land ) : t -> t -> t
  (** intersect sets *)

  val lnot : t -> t
  (** set complement *)

  val test : t -> t -> bool
  (** [test x y] returns true, if and only if the intersection of the
   * two sets is not empty. The common case is [test set x] where [set]
   * is unknown and [x] is a singleton to check that [x] is contained in
   * [set].
   *)

  val to_string : t -> string
  (** [to_string t] return a string representation of [t] for debugging *)
end

val add : t -> Unix.file_descr -> Events.t -> unit
(** [add epoll fd events] registers [fd] with [epoll] to monitor for
 * [events]
 *)

val upd : t -> Unix.file_descr -> Events.t -> unit
(** [upd epoll fd events] updates the events set of [fd] where [fd] has
 * been previously been registered. [upd] is called [mod] in the Linux
 * documentation but [mod] is already an infix operator in OCaml. *)

val del : t -> Unix.file_descr -> unit
(** [del epoll fd] unregister [fd] fro [epoll] *)

(** [wait epoll max timeout f] waits for events on the fds registered
   * with [epoll] to happen or to return after [timeout]. When fds are
   * found to be ready, [wait] iterates over them by calling
   * [f epoll fd events]. [f] receives [epoll], the [fd] being
   * monitored, and the [events]. At most [max] fds are being iterated
   * over by a call to [wait]. Note that still more than [max] fds could
   * be ready to be processed - they would be handled by the next call
   * to [wait].
   *
   * It is important to address the events that trigger an fd to be
   * handled as otherwise the same fd will be handled again at the next
   * call to [wait], leading to a tight loop. This is worth checking
   * using strace(1).
   *
   * See the epoll_wait(2) manual page for the details of the system
   * call.
   *)
val wait :
     t (** epoll *)
  -> int (** max fds to handle *)
  -> int (** timeout in milliseconds: -1 = wait forever *)
  -> (t -> Unix.file_descr -> Events.t -> unit)
  -> int
(** number of fds ready, 0 = timeout *)
