open! Import
module List = List0

open Set_intf

module Merge_to_sequence_element = Merge_to_sequence_element
module Named = Named

module type Elt_plain   = Elt_plain
module type Elt         = Elt
module type Elt_binable = Elt_binable

let to_comparator (type k cmp) ((module M) : (k, cmp) Set.comparator) = M.comparator

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker = Quickcheck.Shrinker
  module Set = Set.Using_comparator

  open Generator.Monad_infix

  let gen_list elt_gen =
    List.gen elt_gen

  let gen ~comparator elt_gen =
    gen_list elt_gen
    >>| Set.of_list ~comparator

  let gen_tree ~comparator elt_gen =
    gen_list elt_gen
    >>| Tree.of_list ~comparator

  let obs elt_obs =
    Observer.unmap (List.obs elt_obs)
      ~f:Set.to_list

  let obs_tree elt_obs =
    Observer.unmap (List.obs elt_obs)
      ~f:Tree.to_list

  let shrink elt_shr t =
    let list = Set.to_list t in
    let drop_elts =
      Sequence.map (Sequence.of_list list) ~f:(fun elt ->
        Set.remove t elt)
    in
    let shrink_elts =
      Sequence.round_robin (List.map list ~f:(fun elt ->
        Sequence.map (Shrinker.shrink elt_shr elt) ~f:(fun elt' ->
          Set.add (Set.remove t elt) elt')))
    in
    Sequence.round_robin [ drop_elts ; shrink_elts ]

  let shrinker elt_shr =
    Shrinker.create (fun t ->
      shrink elt_shr t)

  let shr_tree ~comparator elt_shr =
    Shrinker.create (fun tree ->
      Set.of_tree ~comparator tree
      |> shrink elt_shr
      |> Sequence.map ~f:Set.to_tree)

end

let gen m elt_gen = For_quickcheck.gen ~comparator:(to_comparator m) elt_gen
let obs = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker

module Tree = struct
  include Tree

  let to_map ~comparator t ~f =
    Map.Using_comparator.of_sorted_array_unchecked ~comparator
      (Array.map (to_array t) ~f:(fun key -> (key, f key)))
  ;;

  let of_map_keys m =
    of_sorted_array_unchecked
      ~comparator:(Map.comparator m)
      (List.to_array (Map.keys m))
  ;;

  let of_hash_set ~comparator hset =
    Hash_set.fold hset ~init:(empty ~comparator) ~f:(fun t x -> add t x ~comparator)
  ;;

  let of_hashtbl_keys ~comparator hashtbl =
    Hashtbl.fold hashtbl ~init:(empty ~comparator)
      ~f:(fun ~key:x ~data:_ t -> add t x ~comparator)
  ;;

  let gen = For_quickcheck.gen_tree
  let obs = For_quickcheck.obs_tree
  let shrinker = For_quickcheck.shr_tree
end

module Accessors = struct
  include (Set.Using_comparator : Set.Accessors2
           with type ('a, 'b) t    := ('a, 'b) Set.t
           with type ('a, 'b) tree := ('a, 'b) Tree.t
           with type ('a, 'b) named := ('a, 'b) Set.Named.t)

  let to_map t ~f = Tree.to_map ~comparator:(Set.Using_comparator.comparator t) (to_tree t) ~f
  let obs = obs
  let shrinker = shrinker
end

type 'a cmp = 'a
type 'a elt = 'a


include (struct
  include Set

  let of_tree m = Set.Using_comparator.of_tree ~comparator:(to_comparator m)
  let to_tree = Set.Using_comparator.to_tree

  let sexp_of_t = Set.Using_comparator.sexp_of_t

  module Empty_without_value_restriction = Set.Using_comparator.Empty_without_value_restriction
end : sig
           type ('a, 'b) t = ('a, 'b) Set.t [@@deriving sexp_of]

           include Set.Creators_generic
             with type ('a, 'b, 'c) options := ('a, 'b, 'c) Set.With_first_class_module.t
             with type ('a, 'b) t := ('a, 'b) t
             with type ('a, 'b) set := ('a, 'b) t
             with type ('a, 'b) tree := ('a, 'b) Tree.t
             with type 'a cmp := 'a cmp
             with type 'a elt := 'a elt

           include Set.Accessors2
             with type ('a, 'b) t := ('a, 'b) t
             with type ('a, 'b) tree := ('a, 'b) Tree.t
             with type ('a, 'b) named := ('a, 'b) Set.Named.t
             with module Named := Named
         end)

type ('k, 'cmp) comparator =
  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)

let compare _ _ t1 t2 = compare_direct t1 t2

module Using_comparator = struct
  include (Set.Using_comparator :
             module type of struct include Set.Using_comparator end
           with module Tree := Tree)
  include For_quickcheck

  let of_map_keys m = of_tree ~comparator:(Map.comparator m) (Tree.of_map_keys m)
  let of_hash_set ~comparator hset = of_tree ~comparator (Tree.of_hash_set hset ~comparator)
  let of_hashtbl_keys ~comparator hashtbl = of_tree ~comparator (Tree.of_hashtbl_keys hashtbl ~comparator)
end

let to_map = Accessors.to_map
let of_map_keys = Using_comparator.of_map_keys
let hash_fold_direct = Using_comparator.hash_fold_direct
let comparator = Using_comparator.comparator
let of_hash_set m hset = Using_comparator.of_hash_set ~comparator:(to_comparator m) hset
let of_hashtbl_keys m hashtbl =
  Using_comparator.of_hashtbl_keys ~comparator:(to_comparator m) hashtbl

module Creators (Elt : Comparator.S1) : sig

  type nonrec ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator_witness) t
  type ('a, 'b) tree = ('a, Elt.comparator_witness) Tree.t
  type 'a elt_ = 'a Elt.t
  type 'a cmp_ = Elt.comparator_witness

  val t_of_sexp : (Base.Sexp.t -> 'a Elt.t) -> Base.Sexp.t -> ('a, 'comparator) t_

  include Creators_generic
    with type ('a, 'b) t           := ('a, 'b) t_
    with type ('a, 'b) set         := ('a, 'b) t
    with type ('a, 'b) tree        := ('a, 'b) tree
    with type 'a elt               := 'a elt_
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) Without_comparator.t
    with type 'a cmp               := 'a cmp_

end = struct

  open Using_comparator

  type nonrec ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator_witness) t

  type ('a, 'b) tree = ('a, Elt.comparator_witness) Tree.t

  type 'a elt_ = 'a Elt.t

  type 'cmp cmp_ = Elt.comparator_witness

  let comparator = Elt.comparator

  let of_tree tree = of_tree ~comparator tree

  let of_sorted_array_unchecked array = of_sorted_array_unchecked ~comparator array

  let of_increasing_iterator_unchecked ~len ~f =
    of_increasing_iterator_unchecked ~comparator ~len ~f

  let of_sorted_array array = of_sorted_array ~comparator array

  module M_empty = Empty_without_value_restriction(Elt)
  let empty = M_empty.empty

  let singleton e = singleton ~comparator e

  let union_list l = union_list ~comparator l

  let of_list l = of_list ~comparator l

  let of_hash_set h = of_hash_set ~comparator h

  let of_hashtbl_keys h = of_hashtbl_keys ~comparator h

  let of_array a = of_array ~comparator a

  let stable_dedup_list xs = stable_dedup_list ~comparator xs

  let map t ~f = map ~comparator t ~f

  let filter_map t ~f = filter_map ~comparator t ~f

  let t_of_sexp a_of_sexp sexp = of_tree (Tree.t_of_sexp_direct a_of_sexp sexp ~comparator)

  let of_map_keys = of_map_keys

  let gen elt = gen ~comparator elt

end

module Make_tree (Elt : Comparator.S1) = struct
  let comparator = Elt.comparator

  let compare_elt = comparator.Comparator.compare

  let empty = Tree.empty_without_value_restriction
  let singleton e = Tree.singleton ~comparator e

  let invariants  t = Tree.invariants  t ~comparator
  let length      t = Tree.length      t
  let is_empty    t = Tree.is_empty    t
  let elements    t = Tree.elements    t
  let min_elt     t = Tree.min_elt     t
  let min_elt_exn t = Tree.min_elt_exn t
  let max_elt     t = Tree.max_elt     t
  let max_elt_exn t = Tree.max_elt_exn t
  let choose      t = Tree.choose      t
  let choose_exn  t = Tree.choose_exn  t
  let to_list     t = Tree.to_list     t
  let to_array    t = Tree.to_array    t

  let iter     t ~f = Tree.iter     t ~f
  let iter2  a b ~f = Tree.iter2  a b ~f ~comparator
  let exists   t ~f = Tree.exists   t ~f
  let for_all  t ~f = Tree.for_all  t ~f
  let count    t ~f = Tree.count    t ~f
  let sum    m t ~f = Tree.sum    m t ~f
  let find     t ~f = Tree.find     t ~f
  let find_exn t ~f = Tree.find_exn t ~f
  let find_map t ~f = Tree.find_map t ~f

  let fold       t ~init ~f = Tree.fold       t ~init ~f
  let fold_until t ~init ~f = Tree.fold_until t ~init ~f
  let fold_right t ~init ~f = Tree.fold_right t ~init ~f
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t

  let map          t ~f = Tree.map          t ~f ~comparator
  let filter       t ~f = Tree.filter       t ~f ~comparator
  let filter_map   t ~f = Tree.filter_map   t ~f ~comparator
  let partition_tf t ~f = Tree.partition_tf t ~f ~comparator

  let mem    t a = Tree.mem    t a ~comparator
  let add    t a = Tree.add    t a ~comparator
  let remove t a = Tree.remove t a ~comparator

  let union   t1 t2 = Tree.union   t1 t2 ~comparator
  let inter   t1 t2 = Tree.inter   t1 t2 ~comparator
  let diff    t1 t2 = Tree.diff    t1 t2 ~comparator
  let symmetric_diff t1 t2 =
    Tree.symmetric_diff t1 t2 ~comparator
  let compare_direct t1 t2 = Tree.compare_direct ~comparator t1 t2
  let equal   t1 t2 = Tree.equal   t1 t2 ~comparator

  let is_subset t ~of_ = Tree.is_subset t ~of_ ~comparator
  let subset t1 t2 = is_subset t1 ~of_:t2

  let of_list  l = Tree.of_list  l ~comparator
  let of_hash_set h = Tree.of_hash_set h ~comparator
  let of_hashtbl_keys h = Tree.of_hashtbl_keys h ~comparator
  let of_array a = Tree.of_array a ~comparator
  let of_sorted_array_unchecked a = Tree.of_sorted_array_unchecked a ~comparator
  let of_increasing_iterator_unchecked ~len ~f =
    Tree.of_increasing_iterator_unchecked ~len ~f ~comparator
  let of_sorted_array a = Tree.of_sorted_array a ~comparator

  let union_list l         = Tree.union_list l ~comparator
  let stable_dedup_list xs = Tree.stable_dedup_list xs ~comparator
  let group_by t ~equiv    = Tree.group_by t ~equiv ~comparator
  let split        t a = Tree.split        t a ~comparator
  let nth   t i = Tree.nth   t i
  let find_index = nth
  let remove_index t i = Tree.remove_index t i ~comparator

  let to_tree t = t
  let of_tree t = t

  let to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t =
    Tree.to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t

  let merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t t' =
    Tree.merge_to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t t'

  let of_map_keys = Tree.of_map_keys
  let to_map t ~f = Tree.to_map t ~f ~comparator

  module Named = struct
    let is_subset t ~of_ = Tree.Named.is_subset t ~of_ ~comparator
    let equal t1 t2 = Tree.Named.equal t1 t2 ~comparator
  end

  let gen elt = For_quickcheck.gen_tree elt ~comparator
  let obs elt = For_quickcheck.obs_tree elt
  let shrinker elt = For_quickcheck.shr_tree elt ~comparator
end

(* Don't use [of_sorted_array] to avoid the allocation of an intermediate array *)
let init_for_bin_prot ~len ~f ~comparator =
  let set = Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f in
  if invariants set
  then set
  else
    Using_comparator.of_tree ~comparator
      (fold set ~init:(Tree.empty ~comparator) ~f:(fun acc elt ->
         if Tree.mem acc elt ~comparator
         then failwith "Set.bin_read_t: duplicate element in map"
         else Tree.add acc elt ~comparator))
;;

module Poly = struct
  module Elt = Comparator.Poly

  include Creators (Elt)

  type nonrec 'a t = ('a, Elt.comparator_witness) t

  type 'a named = ('a, Elt.comparator_witness) Named.t

  include Accessors

  let compare _ t1 t2 = compare_direct t1 t2

  let sexp_of_t sexp_of_k t = sexp_of_t sexp_of_k [%sexp_of: _] t

  include Bin_prot.Utils.Make_iterable_binable1 (struct
      type nonrec 'a t = 'a t
      type 'a el = 'a [@@deriving bin_io]
      let _ = bin_el

      let caller_identity = Bin_prot.Shape.Uuid.of_string "88bcc478-4992-11e6-a95d-ff4831acf410"
      let module_name = Some "Core_kernel.Set"
      let length = length
      let iter t ~f = iter ~f:(fun key -> f key) t
      let init ~len ~next =
        init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Comparator.Poly.comparator

    end)

  module Tree = struct
    include Make_tree (Comparator.Poly)

    type 'elt t = ('elt, Comparator.Poly.comparator_witness) tree

    type 'a named = ('a, Elt.comparator_witness) Tree.Named.t

    let sexp_of_t sexp_of_elt t = Tree.sexp_of_t sexp_of_elt [%sexp_of: _] t

    let t_of_sexp elt_of_sexp sexp =
      Tree.t_of_sexp_direct elt_of_sexp sexp
        ~comparator:Comparator.Poly.comparator
    ;;
  end

  let%test_module _ = (module struct
    let (=) = Pervasives.(=)
    let%test _ = stable_dedup_list [] = []
    let%test _ = stable_dedup_list [5;5;5;5;5] = [5]
    let%test _ = stable_dedup_list [5;9;3;5;2;2] = [5;9;3;2]
  end)
end

module type S_plain = S_plain
module type S = S
module type S_binable = S_binable

module Make_plain_using_comparator (Elt : sig
    type t [@@deriving sexp_of]
    include Comparator.S with type t := t
  end) = struct

  module Elt = Elt
  module Elt_S1 = Comparator.S_to_S1 (Elt)

  include Creators (Elt_S1)

  type ('a, 'b) set = ('a, 'b) t
  type t = (Elt.t, Elt.comparator_witness) set
  type named = (Elt.t, Elt.comparator_witness) Named.t

  include Accessors

  let compare t1 t2 = compare_direct t1 t2

  let sexp_of_t t = sexp_of_t Elt.sexp_of_t [%sexp_of: _] t

  module Provide_of_sexp (Elt : sig type t [@@deriving of_sexp] end with type t := Elt.t) =
  struct
    let t_of_sexp sexp = t_of_sexp Elt.t_of_sexp sexp
  end

  module Provide_hash (Elt : Hasher.S with type t := Elt.t) =
  struct
    let hash_fold_t state t = Using_comparator.hash_fold_direct Elt.hash_fold_t state t
    let hash t =
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) t)
  end

  module Provide_bin_io (Elt' : sig type t [@@deriving bin_io] end with type t := Elt.t) =
    Bin_prot.Utils.Make_iterable_binable (struct
      module Elt = struct include Elt include Elt' end
      type nonrec t = t
      type el = Elt.t [@@deriving bin_io]
      let _ = bin_el

      let caller_identity = Bin_prot.Shape.Uuid.of_string "8989278e-4992-11e6-8f4a-6b89776b1e53"
      let module_name = Some "Core_kernel.Set"
      let length = length
      let iter t ~f = iter ~f:(fun key -> f key) t
      let init ~len ~next =
        init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Elt.comparator
    end)


  module Tree = struct
    include Make_tree (Elt_S1)

    type t = (Elt.t, Elt.comparator_witness) tree
    type named = (Elt.t, Elt.comparator_witness) Tree.Named.t

    let compare t1 t2 = compare_direct t1 t2

    let sexp_of_t t = Tree.sexp_of_t Elt.sexp_of_t [%sexp_of: _] t

    module Provide_of_sexp (X : sig type t [@@deriving of_sexp] end with type t := Elt.t) =
    struct
      let t_of_sexp sexp =
        Tree.t_of_sexp_direct X.t_of_sexp sexp ~comparator:Elt_S1.comparator
      ;;
    end
  end
end

module Make_plain (Elt : Elt_plain) =
  Make_plain_using_comparator (struct
    include Elt
    include Comparator.Make (Elt)
  end)

module Make_using_comparator (Elt : sig
    type t [@@deriving sexp]
    include Comparator.S with type t := t
  end) = struct
  module Elt = Elt
  module M1 = Make_plain_using_comparator(Elt)
  include (M1 : module type of M1
           with module Tree := M1.Tree
           with module Elt := Elt)
  include Provide_of_sexp (Elt)
  module Tree = struct
    include M1.Tree
    include Provide_of_sexp (Elt)
  end
end

module Make (Elt : Elt) =
  Make_using_comparator (struct
    include Elt
    include Comparator.Make (Elt)
  end)

module Make_binable_using_comparator (Elt : sig
    type t [@@deriving bin_io, sexp]
    include Comparator.S with type t := t
  end) = struct
  module Elt = Elt
  module M2 = Make_using_comparator (Elt)
  include (M2 : module type of M2 with module Elt := Elt)
  include Provide_bin_io (Elt)
end

module Make_binable (Elt : Elt_binable) =
  Make_binable_using_comparator (struct
    include Elt
    include Comparator.Make (Elt)
  end)

module M = Set.M

module Stable = struct
  module V1 = struct
    type nonrec ('a, 'cmp) t = ('a, 'cmp) t

    module type S = sig
      type elt
      type elt_comparator_witness
      type nonrec t = (elt, elt_comparator_witness) t
      include Stable_module_types.S0_without_comparator with type t := t
    end

    module Make (Elt : Stable_module_types.S0) =
      Make_binable_using_comparator (Elt)
  end
end
