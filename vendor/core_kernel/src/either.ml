module Stable = struct
  module V1 = struct
    type ('f, 's) t = ('f, 's) Base.Either.t =
      | First  of 'f
      | Second of 's
    [@@deriving bin_io, compare, hash, sexp, typerep]

    let map x ~f1 ~f2 =
      match x with
      | First  x1 -> First  (f1 x1)
      | Second x2 -> Second (f2 x2)
  end
end

include Stable.V1

include (Base.Either
         : module type of struct include Base.Either end
         with type ('f, 's) t := ('f, 's) t)

include Comparator.Derived2(struct
    type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of, compare]
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let to_poly = function
    | First  a -> `A a
    | Second b -> `B b

  let of_poly = function
    | `A a -> First a
    | `B b -> Second b

  let gen a b =
    Generator.union
      [ a >>| first
      ; b >>| second
      ]

  let obs a b =
    Observer.unmap (Observer.variant2 a b)
      ~f:to_poly

  let shrinker a b =
    Shrinker.map
      (Shrinker.variant2 a b)
      ~f:of_poly
      ~f_inverse:to_poly

end

let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker
