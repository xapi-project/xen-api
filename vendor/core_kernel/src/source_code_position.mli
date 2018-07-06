(** This module extends {{!Base.source_code_position}[Base.source_code_position]}. *)

include module type of struct include Base.Source_code_position end (** @open *)

include Comparable.S
  with type t := t
   and type comparator_witness := comparator_witness

include Hashable.S with type t := t

module Stable : sig
  module V1 : Stable_module_types.S0 with type t = t
end
