open! Import
open! Std_internal

include Time_intf.S with module Time := Time_float0

module Stable : sig
  include module type of struct include Time_float0.Stable end

  module With_utc_sexp : sig
    module V2 : Stable_without_comparator with type t = Time_float0.t
  end

  module Zone : Zone_intf.S_stable with type t := Zone.t
end
