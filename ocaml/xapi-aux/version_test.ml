open Alcotest

let format () =
  check bool __LOC__ true (Version.is_valid "3") ;
  check bool __LOC__ true (Version.is_valid "0") ;
  check bool __LOC__ true (Version.is_valid "3.1") ;
  check bool __LOC__ true (Version.is_valid "3.1.4") ;
  check bool __LOC__ true (Version.is_valid "3.14") ;
  check bool __LOC__ false (Version.is_valid "") ;
  check bool __LOC__ false (Version.is_valid "3a") ;
  check bool __LOC__ false (Version.is_valid "3.1.4.") ;
  check bool __LOC__ false (Version.is_valid "3.1.4.a") ;
  check bool __LOC__ false (Version.is_valid "3.1.4a") ;
  check bool __LOC__ false (Version.is_valid "3.1:4") ;
  check bool __LOC__ false (Version.is_valid "-3.1.4")

let order () =
  check bool __LOC__ true (Version.String.eq "3" "3.0.0") ;
  check bool __LOC__ true (Version.String.le "3" "3.0.1") ;
  check bool __LOC__ true (Version.String.le "3.1" "3.10") ;
  check bool __LOC__ true (Version.String.eq "0" "0.0.0")

let tests = [test_case "format" `Quick format; test_case "order" `Quick order]

let () = run __MODULE__ [(__MODULE__, tests)]
