(** This module extends {{!Base.Int64}[Base.Int64]}. *)

(** {2 Interface from Base} *)

include module type of struct include Base.Int64 end
  with module Hex := Base.Int64.Hex (** @open *)

(** {2 Extensions} *)

include Int_intf.Extension
  with type t := t
   and type comparator_witness := comparator_witness (** @open *)
