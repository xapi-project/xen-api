open! Import

module Bin : Binable0.S with type t := Base.Int63.t = struct
  module Bin_emulated = struct
    type t = Base.Not_exposed_properly.Int63_emul.t
    include Binable0.Of_binable(Int64)(struct
        type nonrec t = t
        let of_binable = Base.Not_exposed_properly.Int63_emul.W.wrap_exn
        let to_binable = Base.Not_exposed_properly.Int63_emul.W.unwrap
      end)
  end
  type 'a binable = (module Binable0.S with type t = 'a)
  let binable_of_repr : type a b. (a, b) Base.Int63.Private.Repr.t -> b binable = function
    | Base.Int63.Private.Repr.Int   -> (module Int)
    | Base.Int63.Private.Repr.Int64 -> (module Bin_emulated)
  include (val (binable_of_repr Base.Int63.Private.repr : Base.Int63.t binable))
  let bin_shape_t = Bin_prot.Shape.bin_shape_int63
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t = Base.Int63.t [@@deriving hash, sexp]
      include Bin
      include (Base.Int63 : Base.Comparable.S
               with type t := t
               with type comparator_witness = Base.Int63.comparator_witness)
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end

module type Typerepable = sig
  type t [@@deriving typerep]
end
type 'a typerepable = (module Typerepable with type t = 'a)
let typerep_of_repr : type a b. (a, b) Base.Int63.Private.Repr.t -> a typerepable
  = function
    | Base.Int63.Private.Repr.Int   -> (module Int)
    | Base.Int63.Private.Repr.Int64 -> (module Int64)
include (val (typerep_of_repr Base.Int63.Private.repr : Base.Int63.t typerepable))

include Identifiable.Extend (Base.Int63) (struct
    type nonrec t = t
    include Bin
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int63.Hex : module type of struct include Base.Int63.Hex end
           with type t := t)
end

include (Base.Int63
         : (module type of struct include Base.Int63 end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    include Base.Int63
    let splittable_random = Splittable_random.int63
  end)

let%expect_test _ =
  print_string [%bin_digest: t];
  [%expect{| 2b528f4b22f08e28876ffe0239315ac2 |}]
