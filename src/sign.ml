open! Import

module Sign = Base.Sign

module Stable = struct
  module V1 = struct
    type t = Sign.t = Neg | Zero | Pos
    [@@deriving sexp, bin_io, compare, hash, typerep, enumerate]
  end
end

include Stable.V1

include (Sign : module type of struct include Sign end with type t := t)

include Identifiable.Extend (Sign) (Stable.V1)

let ( < ) = Pervasives.(<)
let ( = ) = Pervasives.(=)
let%test _ = compare Neg Zero < 0 && compare Zero Pos < 0
let%test _ = List.for_all all ~f:(fun t -> t = (t |> to_int   |> of_int  ))
let%test _ = List.for_all [ -1; 0; 1 ] ~f:(fun i -> i = (i |> of_int   |> to_int  ))
