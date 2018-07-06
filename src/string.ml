open! Import

(* These two are needed because [include Identifiable.Extend] (present later in the file)
   binds new [Map] and [Set] modules. *)
module Core_map = Map
module Core_set = Set

module Stable = struct
  module V1 = struct
    module T = struct
      type t = string [@@deriving bin_io]
      include (Base.String : module type of struct include Base.String end
               with type t := t)
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end

module Caseless = struct
  module T = struct
    type t = string [@@deriving bin_io]
    include (Base.String.Caseless : module type of struct include Base.String.Caseless end
             with type t := t)
  end
  include T
  include Comparable.Make_binable_using_comparator(T)
  include Hashable.Make_binable(T)
end

let%test_module "Caseless Comparable" =
  (module struct
    let%test _ =
      Int.equal (Core_map.find_exn (Caseless.Map.of_alist_exn [("a", 4); ("b", 5)]) "A") 4

    let%test _ = Core_set.mem (Caseless.Set.of_list ["hello"; "world"]) "heLLO"
    let%test _ = Int.equal (Core_set.length (Caseless.Set.of_list ["a"; "A"])) 1

  end)

let%test_module "Caseless Hash" =
  (module struct
    let%test _ = Int.equal (Caseless.hash "Hello") (Caseless.hash "HELLO")
  end)

type t = string [@@deriving typerep]

include (Base.String
         : module type of struct include Base.String end
         with type t := t
         with module Caseless := Base.String.Caseless)

include Identifiable.Extend (Base.String) (struct
    type t = string [@@deriving bin_io]
  end)

include Hexdump.Of_indexable (struct
    type t = string
    let length = length
    let get    = get
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Let_syntax

  let default_length =
    (* Generating the empty string for every size 0 case is far more tests of the empty
       string than we need. Instead, at any size we generate strings of length up to
       size+1. *)
    let%bind size          = Generator.size                         in
    let      upper_bound   = size + 1                               in
    let%bind weighted_low  = Int.gen_log_uniform_incl 0 upper_bound in
    let      weighted_high = upper_bound - weighted_low             in
    return weighted_high

  let gen_with_length len char_gen =
    let%bind chars = List.gen_with_length len char_gen in
    return (of_char_list chars)

  let gen' char_gen =
    let%bind len = default_length in
    gen_with_length len char_gen

  let gen = gen' Char.gen

  let obs =
    Observer.unmap (List.obs Char.obs)
      ~f:to_list

  let shrinker =
    Shrinker.map (List.shrinker Char.shrinker) ~f:of_char_list ~f_inverse:to_list

end

let gen_with_length = For_quickcheck.gen_with_length
let gen'            = For_quickcheck.gen'
let gen             = For_quickcheck.gen
let obs             = For_quickcheck.obs
let shrinker        = For_quickcheck.shrinker
;;

let take_while t ~f =
  match lfindi t ~f:(fun _ elt -> not (f elt)) with
  | None -> t
  | Some i -> sub t ~pos:0 ~len:i
;;

let rtake_while t ~f =
  match rfindi t ~f:(fun _ elt -> not (f elt)) with
  | None -> t
  | Some i -> sub t ~pos:(i + 1) ~len:(length t - i - 1)
;;

let%test_module "take_while" =
  (module struct
    let f = Char.is_digit

    let%test_unit _ = [%test_result: t] ( take_while "123abc456" ~f) ~expect:"123"
    let%test_unit _ = [%test_result: t] (rtake_while "123abc456" ~f) ~expect:"456"
    let%test_unit _ = [%test_result: t] ( take_while "123456"    ~f) ~expect:"123456"
    let%test_unit _ = [%test_result: t] (rtake_while "123456"    ~f) ~expect:"123456"
  end)
;;

(** See {!Array.normalize} for the following 4 functions. *)
let normalize t i =
  Ordered_collection_common.normalize ~length_fun:length t i

let slice t start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    t start stop

let nget x i =
  let module String = Base.String in
  x.[normalize x i]
