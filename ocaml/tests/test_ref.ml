(* determinisitcally generated one, so crowbar generated testcase is
   reproducible, randomly generated one may change sort order for example *)

let uuidm =
  Crowbar.(
    map [bytes_fixed 16] @@ fun b ->
    b |> Uuidm.of_binary_string ~pos:0 |> Option.get
  )

let ref_of_uuidm uuidm =
  Ref.ref_prefix ^ (uuidm |> Uuidm.to_string) |> Ref.of_string

type arg = [`Generic]

type t = arg Ref.t

let ref_real = Crowbar.(map [uuidm] ref_of_uuidm)

let ref_null = Crowbar.const Ref.null

let ref_dummy = Crowbar.(map [bytes] Ref.make_dummy)

let ref_other = Crowbar.(map [bytes] Ref.of_string)

let dump = Fmt.using Ref.really_pretty_and_small Fmt.Dump.string

let ref : t Crowbar.gen =
  Crowbar.choose [ref_null; ref_real; ref_dummy; ref_other]
  |> Crowbar.with_printer dump

let test_deterministic a b = Crowbar.check_eq (Ref.compare a b) (Ref.compare a b)

let test_reflexive a = Crowbar.check_eq (Ref.compare a a) 0

let pp_compare_result ppf r =
  if r = 0 then
    Fmt.char ppf '='
  else if r < 0 then
    Fmt.char ppf '<'
  else if r > 0 then
    Fmt.char ppf '>'

let test_antisymmetric a b =
  let a_b = Ref.compare a b and b_a = Ref.compare b a in
  (* a <= b && b <= a => a = b *)
  if a_b <= 0 && b_a <= 0 && a_b <> 0 then
    Crowbar.failf "a %a b && b %a a. Expected a = b" pp_compare_result a_b
      pp_compare_result b_a

let test_transitive a b c =
  let a_b = Ref.compare a b
  and b_c = Ref.compare b c
  and a_c = Ref.compare a c in
  if a_b <= 0 && b_c <= 0 && a_c > 0 then
    Crowbar.failf "a %a b && b %a c. a %a c, but expected a <= c"
      pp_compare_result a_b pp_compare_result b_c pp_compare_result a_c

let test_strongly_connected a b =
  let a_b = Ref.compare a b and b_a = Ref.compare b a in
  if not (a_b <= 0 || b_a <= 0) then
    Crowbar.failf
      "Strongly connected (total) property failed. Expected a <= b\n\
      \    or b <= a, but got: a %a b, b %a a" pp_compare_result a_b
      pp_compare_result b_a

let test_strict_irreflexive a = Crowbar.check_eq (Ref.compare a a < 0) false

let test_strict_asymmetric a b =
  let a_b = Ref.compare a b and b_a = Ref.compare b a in
  (* a < b => not (b < a) *)
  if a_b < 0 && b_a < 0 then
    Crowbar.failf "a %a b. b %a a. Expected: not (b < a)" pp_compare_result a_b
      pp_compare_result b_a

let test_strict_connected a b =
  let a_b = Ref.compare a b and b_a = Ref.compare b a in
  if a <> b && not (a_b < 0 || b_a < 0) then
    Crowbar.failf "a %a b. b %a a. Not connected, expected (a < b || b < a)"
      pp_compare_result a_b pp_compare_result b_a

let () =
  let open Crowbar in
  add_test ~name:"total order: deterministic" [ref; ref] test_deterministic ;
  add_test ~name:"total order: reflexive" [ref] test_reflexive ;
  add_test ~name:"total order: antisymmetric" [ref; ref] test_antisymmetric ;
  add_test ~name:"total order: strongly connected" [ref; ref]
    test_strongly_connected ;
  add_test ~name:"total order: transitive" [ref; ref; ref] test_transitive ;
  add_test ~name:"strict total order: irreflexive" [ref] test_strict_irreflexive ;
  add_test ~name:"strict total order: asymmetric" [ref; ref]
    test_strict_asymmetric ;
  add_test ~name:"strict total order: connected" [ref; ref]
    test_strict_connected
