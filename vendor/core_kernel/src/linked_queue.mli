(** This module extends the {{!Base.Queue}[Base.Queue]} module with bin_io support.  As a
    reminder, the [Base.Queue] module is a wrapper around OCaml's standard [Queue] module
    that follows Base idioms and adds some functions.

    See also {!Core_kernel.Queue}, which has different performance characteristics. *)

type 'a t = 'a Base.Linked_queue.t [@@deriving bin_io]

include module type of struct include Base.Linked_queue end
  with type 'a t := 'a t (** @open *)
