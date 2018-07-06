open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Date) [
    Date.of_string "1955-11-12";
    Date.of_string "1985-10-26";
    Date.of_string "2015-10-21";
  ];
  [%expect {|
    (Set (1955-11-12 1985-10-26 2015-10-21))
    (Map (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2)))
    (Hash_set (1955-11-12 1985-10-26 2015-10-21))
    (Table (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2))) |}];
;;

let%expect_test "Date.V1" =
  print_and_check_stable_type [%here] (module Date.Stable.V1) [
    Date.create_exn ~y:1066 ~m:Oct ~d:16;
    Date.create_exn ~y:1955 ~m:Nov ~d: 5;
    Date.create_exn ~y:2012 ~m:Apr ~d:19;
  ];
  [%expect {|
    (bin_shape_digest 47681bb034560d96024e1b2eca0d98ca)
    ((sexp   1066-10-16)
     (bin_io "\254*\004\t\016"))
    ((sexp   1955-11-05)
     (bin_io "\254\163\007\n\005"))
    ((sexp   2012-04-19)
     (bin_io "\254\220\007\003\019")) |}];
;;

let%expect_test "Date.V1.Set" =
  print_and_check_stable_type [%here] (module Date.Stable.V1.Set) [
    Date.Set.empty;
    Date.Set.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16);
    Date.Set.of_list [
      Date.create_exn ~y:1955 ~m:Nov ~d: 5;
      Date.create_exn ~y:2012 ~m:Apr ~d:19;
    ];
    Date.Set.of_list [
      Date.create_exn ~y:1066 ~m:Oct ~d:16;
      Date.create_exn ~y:1955 ~m:Nov ~d: 5;
      Date.create_exn ~y:2012 ~m:Apr ~d:19;
    ];
  ];
  [%expect {|
    (bin_shape_digest ccde15fc17afce11a067d80e40cb1e8d)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1066-10-16)) (bin_io "\001\254*\004\t\016"))
    ((sexp (1955-11-05 2012-04-19))
     (bin_io "\002\254\163\007\n\005\254\220\007\003\019"))
    ((sexp (1066-10-16 1955-11-05 2012-04-19))
     (bin_io "\003\254*\004\t\016\254\163\007\n\005\254\220\007\003\019")) |}];
;;

let%expect_test "Date.V1.Map" =
  let module T = struct
    type t = string Date.Stable.V1.Map.t
    [@@deriving bin_io, compare, sexp]
  end in
  print_and_check_stable_type [%here] (module T) [
    Date.Map.empty;
    Date.Map.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16)
      "not the Battle of Hastings";
    Date.Map.of_alist_exn [
      Date.create_exn ~y:1955 ~m:Nov ~d: 5, "flux capacitor";
      Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday";
    ];
    Date.Map.of_alist_exn [
      Date.create_exn ~y:1066 ~m:Oct ~d:16, "not the Battle of Hastings";
      Date.create_exn ~y:1955 ~m:Nov ~d: 5, "flux capacitor";
      Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday";
    ];
  ];
  [%expect {|
    (bin_shape_digest a0aa3c6d1173d784fbd03980ac5d0be5)
    ((sexp ()) (bin_io "\000"))
    ((sexp ((1066-10-16 "not the Battle of Hastings")))
     (bin_io "\001\254*\004\t\016\026not the Battle of Hastings"))
    ((sexp (
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\002\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday"))
    ((sexp (
       (1066-10-16 "not the Battle of Hastings")
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\003\254*\004\t\016\026not the Battle of Hastings\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday")) |}];
;;

let%test_unit "create_exn doesn't allocate" =
  let allocation_before = Gc.major_plus_minor_words () in
  ignore (Date.create_exn ~y:1999 ~m:Dec ~d:31 : Date.t);
  let allocation_after = Gc.major_plus_minor_words () in
  [%test_eq: int] allocation_before allocation_after;
;;

let%test_unit "creation and destruction" =
  let test y m d =
    let t = Date.create_exn ~y ~m ~d in
    [%test_result: int] ~expect:y (Date.year  t);
    [%test_result: Month.t] ~expect:m (Date.month t);
    [%test_result: int] ~expect:d (Date.day   t);
  in
  test 2014 Month.Sep 24;
  test 9999 Month.Dec 31
;;

let%expect_test "add_years" =
  let test string =
    let date = Date.of_string string in
    for years = -4 to 4 do
      let date_plus_years = Date.add_years date years in
      printf !"%{Date} + %2d years = %{Date}\n" date years date_plus_years;
    done
  in
  (* non-leap day *)
  test "2013-10-07";
  [%expect {|
    2013-10-07 + -4 years = 2009-10-07
    2013-10-07 + -3 years = 2010-10-07
    2013-10-07 + -2 years = 2011-10-07
    2013-10-07 + -1 years = 2012-10-07
    2013-10-07 +  0 years = 2013-10-07
    2013-10-07 +  1 years = 2014-10-07
    2013-10-07 +  2 years = 2015-10-07
    2013-10-07 +  3 years = 2016-10-07
    2013-10-07 +  4 years = 2017-10-07 |}];
  (* leap day maps to Feb 28 on non-leap years (and 400-year century behaves properly) *)
  test "2004-02-29";
  [%expect {|
    2004-02-29 + -4 years = 2000-02-29
    2004-02-29 + -3 years = 2001-02-28
    2004-02-29 + -2 years = 2002-02-28
    2004-02-29 + -1 years = 2003-02-28
    2004-02-29 +  0 years = 2004-02-29
    2004-02-29 +  1 years = 2005-02-28
    2004-02-29 +  2 years = 2006-02-28
    2004-02-29 +  3 years = 2007-02-28
    2004-02-29 +  4 years = 2008-02-29 |}];
  (* non-leap year century behaves properly *)
  test "1904-02-29";
  [%expect {|
    1904-02-29 + -4 years = 1900-02-28
    1904-02-29 + -3 years = 1901-02-28
    1904-02-29 + -2 years = 1902-02-28
    1904-02-29 + -1 years = 1903-02-28
    1904-02-29 +  0 years = 1904-02-29
    1904-02-29 +  1 years = 1905-02-28
    1904-02-29 +  2 years = 1906-02-28
    1904-02-29 +  3 years = 1907-02-28
    1904-02-29 +  4 years = 1908-02-29 |}];
;;
