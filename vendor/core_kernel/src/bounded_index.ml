open! Import
open! Stable_internal

module Stable = struct
  module V1 = struct
    module Make (M : sig val label : string end) = struct
      type t =
        { index     : int
        ; min_index : int
        ; max_index : int
        }
      [@@deriving bin_io, compare, hash]

      let create index ~min ~max =
        if index < min || index > max
        then
          Error.raise_s [%message
            "index out of bounds"
              (index : int)
              (min   : int)
              (max   : int)]
        else
          { index ; min_index = min ; max_index = max }

      module For_sexpable = struct
        type t = string * int * string * int * string * int [@@deriving sexp]
      end

      include Sexpable.Stable.Of_sexpable.V1 (For_sexpable) (struct
          type nonrec t = t

          let to_sexpable t =
            (M.label, t.index, "of", t.min_index, "to", t.max_index)

          let of_sexpable (label, index, of_, min, to_, max) =
            if String.equal label M.label
            && String.equal of_ "of"
            && String.equal to_ "to"
            then create index ~min ~max
            else Error.raise_s [%message "invalid sexp for index" ~label:M.label]
        end)

      include Comparator.Stable.V1.Make (struct
          type nonrec t = t [@@deriving sexp_of, compare]
        end)

      include Comparable.Stable.V1.Make (struct
          type nonrec t = t [@@deriving sexp, compare, bin_io]
          type nonrec comparator_witness = comparator_witness
          let comparator = comparator
        end)
    end
  end

  let%test_module "V1" = (module Stable_unit_test.Make (struct
      module M = V1.Make (struct let label = "index" end)

      type t = M.t [@@deriving sexp, bin_io, compare]

      let equal x y = (compare x y = 0)

      let make index min_index max_index : t =
        { index ; min_index ; max_index }

      let tests =
        [ make 0 0 0, "(index 0 of 0 to 0)", "\000\000\000"
        ; make 0 0 1, "(index 0 of 0 to 1)", "\000\000\001"
        ; make 1 0 1, "(index 1 of 0 to 1)", "\001\000\001"
        ; make 0 0 2, "(index 0 of 0 to 2)", "\000\000\002"
        ; make 1 0 2, "(index 1 of 0 to 2)", "\001\000\002"
        ; make 2 0 2, "(index 2 of 0 to 2)", "\002\000\002"
        ; make 1 1 1, "(index 1 of 1 to 1)", "\001\001\001"
        ; make 1 1 2, "(index 1 of 1 to 2)", "\001\001\002"
        ; make 2 1 2, "(index 2 of 1 to 2)", "\002\001\002"
        ; make 1 1 3, "(index 1 of 1 to 3)", "\001\001\003"
        ; make 2 1 3, "(index 2 of 1 to 3)", "\002\001\003"
        ; make 3 1 3, "(index 3 of 1 to 3)", "\003\001\003"
        ; make 499_999_999 0 999_999_999,
          "(index 499999999 of 0 to 999999999)",
          "\253\255\100\205\029\000\253\255\201\154\059"
        ; make 500_000_000 1 1_000_000_000,
          "(index 500000000 of 1 to 1000000000)",
          "\253\000\101\205\029\001\253\000\202\154\059"
        ]
    end))
end

open! Std_internal

module type S = Bounded_index_intf.S

module Make (M : sig val label : string val module_name : string end) = struct
  module Stable = struct
    module V1 = Stable.V1.Make (M)
  end

  open Stable.V1

  type t = Stable.V1.t [@@deriving bin_io, compare, hash, sexp]
  type comparator_witness = Stable.V1.comparator_witness

  let create = Stable.V1.create

  let create_all ~min ~max =
    Sequence.unfold ~init:min ~f:(fun index ->
      if index < min || index > max
      then None
      else Some (create index ~min ~max, index + 1))
    |> Sequence.to_list

  let index     t = t.index
  let max_index t = t.max_index
  let min_index t = t.min_index

  include Sexpable.To_stringable (struct
      type nonrec t = t [@@deriving sexp]
    end)

  include Identifiable.Make_using_comparator (struct
      type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
      type nonrec comparator_witness = comparator_witness
      let comparator = comparator
      let of_string = of_string
      let to_string = to_string
      let module_name = M.module_name
    end)
end
