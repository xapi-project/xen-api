open! Import

include Time0_intf.S with type underlying = float

module Stable : sig
  module Span  : Span_intf.S_stable  with type t := Span.t
  module Ofday : Ofday_intf.S_stable with type t := Ofday.t
end
