
(** Only compares the error code of xapi errors and ignores the parameters *)
let error_code =
  let fmt = Fmt.pair Fmt.string (Fmt.list Fmt.string) in
  let cmp aa bb = fst aa = fst bb in
  Alcotest.testable fmt cmp
