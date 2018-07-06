(** This module extends {{!Base.Sequence}[Base.Sequence]} with bin_io. *)

module Merge_with_duplicates_element : sig
  type ('a, 'b) t = ('a, 'b) Base.Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving bin_io]

  include module type of struct include Base.Sequence.Merge_with_duplicates_element end
  with type ('a, 'b) t := ('a, 'b) t
end

include module type of struct include Base.Sequence end
  with module Merge_with_duplicates_element := Merge_with_duplicates_element (** @open *)

(** Merges elements from sequences that are assumed to be sorted by [compare] to produce a
    sequence also sorted by [compare]. If any of the inputs are not sorted, the order of
    the output is not guaranteed to be sorted.

    This includes duplicate elements in the output (whether they occur within
    one input sequence, or across different input sequences).
*)
val merge_all : 'a t list -> compare:('a -> 'a -> int) -> 'a t
