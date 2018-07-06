open! Core_kernel
open! Import

let%expect_test "Byte_units.to_string_hum" [@tags "64-bits-only"] =
  print_string (Byte_units.to_string_hum (Byte_units.create `Bytes 1000.));
  [%expect {| 1000b |}];
  print_string (Byte_units.to_string_hum (Byte_units.create `Bytes 1500.));
  [%expect {| 1.46484k |}];
  print_string (Byte_units.to_string_hum ~measure:`Gigabytes (Byte_units.create `Bytes 1000.));
  [%expect {| 9.31323e-07g |}];
  print_string (Byte_units.to_string_hum ~measure:`Words (Byte_units.create `Bytes 1000.));
  [%expect {| 125w |}];
;;
