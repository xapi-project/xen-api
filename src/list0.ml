open! Import
open! Typerep_lib.Std

module Array = Base.Array
module List  = Base.List

type 'a t = 'a list [@@deriving bin_io, typerep]

module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list [@@deriving bin_io]

  let compare (type a) (type b) compare_a compare_b = [%compare: (a * b) list]
  [@@deprecated
    "[since 2016-06] This does not respect the equivalence class promised by List.Assoc. \
     Use List.compare directly if that's what you want."]

  include (List.Assoc : module type of struct include List.Assoc end
           with type ('a, 'b) t := ('a, 'b) t)
end

include (List : module type of struct include List end
         with type 'a t := 'a t
         with module Assoc := Assoc)

let to_string ~f t =
  Sexplib.Sexp.to_string
    (sexp_of_t (fun x -> Sexplib.Sexp.Atom x) (List.map t ~f))
;;

include Comparator.Derived(struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
  end)

module For_quickcheck = struct
  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let gen             = Generator.list
  let gen_non_empty   = Generator.list_non_empty
  let gen_with_length = Generator.list_with_length

  let gen_permutations list =
    match list with
    | [] -> Generator.singleton []
    | _ :: _ ->
      let len = List.length list in
      let index_generator =
        init (len - 1) ~f:(fun i ->
          (* choose uniformly among indices to create uniform choice among permutations *)
          Quickcheck.For_int.gen_uniform_incl i (len - 1))
        |> Quickcheck.Generator.all
      in
      index_generator
      >>| fun indices ->
      let arr = Array.of_list list in
      List.iteri indices ~f:(fun i j -> Array.swap arr i j);
      Array.to_list arr
  ;;

  let obs elem_obs =
    Observer.fixed_point (fun t_obs ->
      Observer.unmap
        (Observer.variant2
           (Observer.singleton ())
           (Observer.tuple2 elem_obs t_obs))
        ~f:(function
          | []        -> `A ()
          | x :: list -> `B (x, list)))

  let shrinker t_elt =
    Shrinker.fixed_point (fun t_list ->
      Shrinker.create (function
        | []    -> Sequence.empty
        | h::tl ->
          let open Sequence.Monad_infix in
          let dropped     = Sequence.singleton tl in
          let shrunk_head = Shrinker.shrink t_elt   h >>| fun shr_h  -> shr_h::tl in
          let shrunk_tail = Shrinker.shrink t_list tl >>| fun shr_tl -> h::shr_tl in
          Sequence.round_robin [dropped; shrunk_head; shrunk_tail]))

  let%test_module "shrinker" =
    (module struct

      let t0 =
        Shrinker.create (fun v ->
          if Pervasives.(=) 0 v
          then Sequence.empty
          else Sequence.singleton 0)

      let test_list = [1;2;3]
      let expect =
        [[2;3]; [0;2;3]; [1;3]; [1;0;3]; [1;2]; [1;2;0]]
        |> List.sort ~compare:[%compare: int list ]

      let%test_unit "shrinker produces expected outputs" =
        let shrunk =
          Shrinker.shrink (shrinker t0) test_list
          |> Sequence.to_list
          |> List.sort ~compare:[%compare: int list ]
        in
        [%test_result: int list list] ~expect shrunk

      let rec recursive_list = 1::5::recursive_list

      let%test_unit "shrinker on infinite lists produces values" =
        let shrunk = Shrinker.shrink (shrinker t0) recursive_list in
        let result_length = Sequence.take shrunk 5 |> Sequence.to_list |> List.length in
        [%test_result: int] ~expect:5 result_length
    end)

end

let gen              = For_quickcheck.gen
let gen_non_empty    = For_quickcheck.gen_non_empty
let gen_with_length  = For_quickcheck.gen_with_length
let gen_permutations = For_quickcheck.gen_permutations
let obs              = For_quickcheck.obs
let shrinker         = For_quickcheck.shrinker
