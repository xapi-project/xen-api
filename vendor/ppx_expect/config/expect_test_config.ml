module Upon_unreleasable_issue = struct
  type t =
    [ `CR     (** Leaves a CR, so that features cannot be released. *)
    | `Warning_for_collector_testing  (** Only for ppx_expect testing; do not use. *)
    ]

  let equal t1 t2 = t1 = t2

  let comment_prefix = function
    | `CR                            -> "CR "
    | `Warning_for_collector_testing -> ""
  ;;
end

module type S = sig
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end
  val flush : unit -> unit IO.t
  val run : (unit -> unit IO.t) -> unit
  val flushed : unit -> bool
  val upon_unreleasable_issue : [ `CR | `Warning_for_collector_testing ]
end

module IO = struct
  type 'a t = 'a
  let return x = x
  let bind t ~f = f t
end

let flush () = () (* the runtime already flushes [stdout] *)
let run f = f ()
let flushed () = true (* the runtime flushed [stdout] before calling this function *)

let upon_unreleasable_issue = `CR
