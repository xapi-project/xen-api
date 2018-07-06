open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      type t = int [@@deriving hash, bin_io, sexp]
      include (Base.Int : Base.Comparable.S
               with type t := t
               with type comparator_witness = Base.Int.comparator_witness)
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end

type t = int [@@deriving typerep]

module Z =
  Identifiable.Extend (Base.Int) (struct
    type t = int [@@deriving bin_io]
  end)

include (Z : module type of struct include Z end
         with module Replace_polymorphic_compare := Z.Replace_polymorphic_compare)

module Replace_polymorphic_compare = Base.Int

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int.Hex
           : module type of struct include Base.Int.Hex end with type t := t)
end

include (Base.Int
         : (module type of struct include Base.Int end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    include Base.Int
    let splittable_random = Splittable_random.int
  end)
