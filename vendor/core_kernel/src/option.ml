open! Import

type 'a t = 'a option [@@deriving bin_io, typerep]

include (Base.Option : module type of struct include Base.Option end
         with type 'a t := 'a t)

include Comparator.Derived(struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let gen elt_gen =
    Generator.union
      [ Generator.singleton None
      ; elt_gen >>| return
      ]

  let obs elt_obs =
    Observer.unmap (Observer.variant2 (Observer.singleton ()) elt_obs)
      ~f:(function
        | None   -> `A ()
        | Some x -> `B x)

  let shrinker elt_shr =
    let shrinker = function
      | Some elt ->
        Sequence.append
          (Sequence.singleton None)
          (Sequence.map (Shrinker.shrink elt_shr elt) ~f:(fun v -> Some v))
      | None ->
        Sequence.empty
    in
    Shrinker.create shrinker

  let%test_module "shrinker" =
    (module struct

      let t1 = Shrinker.create (Fn.const (Sequence.singleton 1))

      let%test_unit _ =
        [%test_result: int option list]
          (Sequence.to_list (Shrinker.shrink (shrinker t1) None))
          ~expect:[]

      let%test_unit _ =
        let sort = List.sort ~compare:[%compare: int option ] in
        let expect =
          [ None; Some 1]
          |> sort
        in
        let results =
          Shrinker.shrink (shrinker t1) (Some 5)
          |> Sequence.to_list
          |> sort
        in
        [%test_result: int option list ] ~expect results

    end)

end

let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker
