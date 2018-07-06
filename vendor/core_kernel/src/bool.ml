open! Import

type t = bool [@@deriving bin_io, typerep]

include (Base.Bool : module type of struct include Base.Bool end with type t := t)

include Hashable.Make (Base.Bool)
include Comparable.Make_using_comparator (Base.Bool)

let gen      = Quickcheck.Generator.bool
let obs      = Quickcheck.Observer.bool
let shrinker = Quickcheck.Shrinker.bool
