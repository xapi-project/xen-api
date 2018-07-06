open! Import

type t = nativeint [@@deriving typerep]

include Identifiable.Extend (Base.Nativeint) (struct
    type t = nativeint [@@deriving bin_io]
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Nativeint.Hex
           : module type of struct include Base.Nativeint.Hex end with type t := t)
end

include (Base.Nativeint
         : (module type of struct include Base.Nativeint end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    include Base.Nativeint
    let splittable_random = Splittable_random.nativeint
  end)
