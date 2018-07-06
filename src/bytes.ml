open! Import

module Stable = struct
  module V1 = struct
    type t = bytes [@@deriving bin_io, typerep]
    include (Base.Bytes : module type of struct include Base.Bytes end
             with type t := t)
  end
end

include Stable.V1

include Hexdump.Of_indexable (struct
    type t = bytes
    let length = length
    let get    = get
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Let_syntax

  let default_length =
    let%bind size = Generator.size in
    (* Generating the empty byte sequence for every size 0 case is far more tests
       of the empty bytes than we need. Instead, at size 0 we generate byte
       sequences of length 0 and length 1. At size N>0 we generate byte sequences
       of length N+1. *)
    if Int.equal size 0
    then Generator.weighted_union [ 1., return 0; 10., return 1 ]
    else return (size + 1)

  let gen_with_length len char_gen =
    let%bind chars = List.gen_with_length len char_gen in
    return (of_char_list chars)

  let gen' char_gen =
    let%bind len = default_length in
    gen_with_length len char_gen

  let gen = gen' Char.gen

  let obs =
    Observer.unmap (List.obs Char.obs) ~f:to_list

  let shrinker =
    Shrinker.map (List.shrinker Char.shrinker) ~f:of_char_list ~f_inverse:to_list

end

let gen_with_length = For_quickcheck.gen_with_length
let gen'            = For_quickcheck.gen'
let gen             = For_quickcheck.gen
let obs             = For_quickcheck.obs
let shrinker        = For_quickcheck.shrinker
