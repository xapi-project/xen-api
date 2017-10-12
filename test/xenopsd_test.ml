open OUnit

let base_suite =
  "base_suite" >:::
  [
  ]

let () =
  ounit2_of_ounit1 base_suite |>  OUnit2.run_test_tt_main
