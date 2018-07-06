open! Import

type t = unit [@@deriving typerep]

include Identifiable.Extend(Base.Unit)(struct
    type t = unit [@@deriving bin_io]
  end)

include (Base.Unit
         : module type of struct include Base.Unit end with type t := t)

let gen      = Quickcheck.Generator.singleton ()
let obs      = Quickcheck.Observer.singleton  ()
let shrinker = Quickcheck.Shrinker.empty      ()

module type S = sig end

type m = (module S)
