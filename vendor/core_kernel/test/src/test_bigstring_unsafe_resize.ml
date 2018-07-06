open Core_kernel

let%expect_test "[unsafe_resize]" =
  let bigstring = Bigstring.of_string "ABCDEF" in
  let last_index = Bigstring.length bigstring - 1 in
  bigstring.{last_index} <- '.';
  printf "%s" (Bigstring.to_string bigstring);
  [%expect {| ABCDE. |}];

  let bigstring = Bigstring.unsafe_destroy_and_resize bigstring ~len:0 in
  printf "%s" (Bigstring.to_string bigstring);
  [%expect {| |}];

  begin
    try
      bigstring.{1} <- 'F';
    with e ->
      printf !"%{Exn}\n" e
  end;

  [%expect {| (Invalid_argument "index out of bounds") |}];

  let bigstring = Bigstring.unsafe_destroy_and_resize bigstring ~len:3 in
  Bigstring.From_string.blito () ~src:"ABC" ~dst:bigstring;
  printf "%s" (Bigstring.to_string bigstring);
  [%expect {| ABC |}];

  begin
    try
      bigstring.{last_index} <- 'F';
    with e ->
      printf !"%{Exn}\n" e
  end;

  [%expect {| (Invalid_argument "index out of bounds") |}];
;;

let%expect_test "[unsafe_resize], proxy failure" [@tags "no-js"] =
  let bigstring = Bigstring.create 5 in
  printf "%d" (Bigstring.length bigstring);
  [%expect {| 5 |}];
  let bigstring = Bigstring.unsafe_destroy_and_resize bigstring ~len:10 in
  printf "%d" (Bigstring.length bigstring);
  [%expect {| 10 |}];

  let _shared = Bigstring.sub_shared bigstring in
  begin
    try
      let _ = Bigstring.unsafe_destroy_and_resize bigstring ~len:5 in
      ()
    with e ->
      printf !"%{Exn}\n" e
  end;

  [%expect {| (Failure "bigstring_realloc: bigstring has proxy") |}];
;;
