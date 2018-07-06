(** This module extends {{!Base.Int32}[Base.Int32]}. *)

(** {2 Interface from Base} *)

include module type of struct include Base.Int32 end
  with module Hex := Base.Int32.Hex (** @open *)

(** {2 Extensions} *)

include Int_intf.Extension
  with type t := t
   and type comparator_witness := comparator_witness (** @open *)
