open! Core_kernel
open! Import
open! Timing_wheel_ns

let show t = print_s [%sexp (t : _ t)]

(* giga-nanosecond *)
let gibi = 2. **  30.

let gibi_nanos float = float *. gibi |> Time_ns.Span.of_ns

module Alarm_precision = struct
  include Alarm_precision

  let sexp_of_t t =
    [%message
      ""
        ~_:(t : t)
        ~_:(String.concat [ t
                            |> to_span
                            |> Time_ns.Span.to_int63_ns
                            |> Int63.to_string_hum
                          ; "ns" ])];
  ;;

  let print t = print_s [%sexp (t : t)]

  let%expect_test "constants" =
    print about_one_day;
    [%expect {|
      (19.546873382684446h 70_368_744_177_664ns) |}];
    print about_one_second;
    [%expect {|
      (1.073741824s 1_073_741_824ns) |}];
    print about_one_microsecond;
    [%expect {|
      (1.024us 1_024ns) |}];
    print about_one_millisecond;
    [%expect {|
      (1.048576ms 1_048_576ns) |}];
    print one_nanosecond;
    [%expect {|
     (1ns 1ns) |}];
  ;;

  let%expect_test "[div]" =
    for pow2 = -3 to 3 do
      print (div about_one_second ~pow2);
    done;
    [%expect {|
      (8.589934592s 8_589_934_592ns)
      (4.294967296s 4_294_967_296ns)
      (2.147483648s 2_147_483_648ns)
      (1.073741824s 1_073_741_824ns)
      (536.870912ms 536_870_912ns)
      (268.435456ms 268_435_456ns)
      (134.217728ms 134_217_728ns) |}];
  ;;

  let%expect_test "[mul]" =
    for pow2 = -3 to 3 do
      print (mul about_one_second ~pow2);
    done;
    [%expect {|
      (134.217728ms 134_217_728ns)
      (268.435456ms 268_435_456ns)
      (536.870912ms 536_870_912ns)
      (1.073741824s 1_073_741_824ns)
      (2.147483648s 2_147_483_648ns)
      (4.294967296s 4_294_967_296ns)
      (8.589934592s 8_589_934_592ns) |}];
  ;;

  let%expect_test "[of_span_floor_pow2_ns]" [@tags "64-bits-only"] =
    List.iter
      [ about_one_day
      ; about_one_second
      ; about_one_millisecond
      ; about_one_microsecond
      ; one_nanosecond ]
      ~f:(fun t ->
        require [%here] (equal t (t |> to_span |> of_span_floor_pow2_ns));
        if Time_ns.Span.( > ) (t |> to_span) Time_ns.Span.nanosecond
        then (
          require [%here] (equal t
                             (Time_ns.Span.( + ) (t |> to_span) Time_ns.Span.nanosecond
                              |> of_span_floor_pow2_ns))));
    List.iter
      [ 1.
      ; 1E-3
      ; 1E-6 ]
      ~f:(fun span ->
        let span = Time_ns.Span.of_sec span in
        print_s [%message
          ""
            (span : Time_ns.Span.Alternate_sexp.t)
            ~alarm_precision:(span |> of_span_floor_pow2_ns : t)]);
    [%expect {|
      ((span 1s) (alarm_precision (536.870912ms 536_870_912ns)))
      ((span 1ms) (alarm_precision (524.288us 524_288ns)))
      ((span 1us) (alarm_precision (512ns 512ns))) |}];
  ;;
end

let%expect_test "[Config.microsecond_precision]" =
  print_s [%sexp (Config.microsecond_precision () : Config.t)];
  [%expect {|
    ((alarm_precision 1.024us) (level_bits (10 10 6 6 5))) |}];
  print_s [%sexp (Config.durations (Config.microsecond_precision ())
                  : Time_ns.Span.Alternate_sexp.t list)];
  [%expect {|
    (1.048576ms
     1.073741824s
     1.1453246122666667m
     1.2216795864177779h
     1.6289061152237037d) |}];
;;

let%expect_test _ =
  require [%here] (Level_bits.max_num_bits = Int64.num_bits - 3);
;;

let%expect_test "invalid level bits" =
  let test level_bits =
    require_does_raise [%here] (fun () -> Level_bits.create_exn level_bits);
    require_does_raise [%here] ~hide_positions:true (fun () ->
      [%of_sexp: Level_bits.t] ([%sexp_of: int list] level_bits)) in
  test [];
  [%expect {|
    (Failure "Level_bits.create_exn requires a nonempty list")
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
  test [ 0 ];
  [%expect {|
    ("Level_bits.create_exn got nonpositive num bits" (0))
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
  test [ -1 ];
  [%expect {|
    ("Level_bits.create_exn got nonpositive num bits" (-1))
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
  test [ 2; 0; 1 ];
  [%expect {|
    ("Level_bits.create_exn got nonpositive num bits" (2 0 1))
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
  test [ Level_bits.max_num_bits + 1 ];
  [%expect {|
    ("Level_bits.create_exn got too many bits"
      (62)
      (got          62)
      (max_num_bits 61))
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
  test (List.init (Level_bits.max_num_bits + 1) ~f:Fn.id);
  [%expect {|
    ("Level_bits.create_exn got nonpositive num bits"
     (0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
      11
      12
      13
      14
      15
      16
      17
      18
      19
      20
      21
      22
      23
      24
      25
      26
      27
      28
      29
      30
      31
      32
      33
      34
      35
      36
      37
      38
      39
      40
      41
      42
      43
      44
      45
      46
      47
      48
      49
      50
      51
      52
      53
      54
      55
      56
      57
      58
      59
      60
      61))
    "Assert_failure timing_wheel_ns.ml:LINE:COL" |}];
;;

let%expect_test _ =
  Level_bits.invariant Level_bits.default;
;;

let%expect_test "[Level_bits.num_bits]" =
  let num_bits bits =
    let level_bits = Level_bits.create_exn bits in
    print_s [%sexp (Level_bits.num_bits level_bits : int)];
    let sexp = [%sexp (level_bits : Level_bits.t)] in
    require_equal [%here] (module Sexp)
      sexp
      (sexp |> [%of_sexp: Level_bits.t] |> [%sexp_of: Level_bits.t]) in
  num_bits [ 1 ];
  [%expect {|
    1 |}];
  num_bits [ 1; 1 ];
  [%expect {|
    2 |}];
  num_bits [ 1; 2; 3 ];
  [%expect {|
    6 |}];
;;

let create_config ?level_bits ~alarm_precision () =
  Config.create ()
    ~alarm_precision:(alarm_precision |> Alarm_precision.of_span_floor_pow2_ns)
    ?level_bits:(Option.map level_bits ~f:Level_bits.create_exn)
;;

let%expect_test "[Config.create] with negative alarm precision" =
  require_does_raise [%here] (fun () -> create_config ~alarm_precision:(gibi_nanos (-1.)) ());
  [%expect {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s)) |}];
;;

let%expect_test "[Config.create] with zero alarm precision" =
  require_does_raise [%here] (fun () -> create_config ~alarm_precision:(gibi_nanos 0.) ());
  [%expect {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0ns)) |}];
;;

let%expect_test "[Config.create] with one second alarm precision" =
  print_s [%sexp (create_config ~alarm_precision:(gibi_nanos 1.) () : Config.t)];
  [%expect {|
    ((alarm_precision 1.073741824s)) |}];
;;

let%expect_test "[Config.durations]" =
  let durations level_bits =
    print_s [%sexp
      (Config.durations (create_config
                           ~alarm_precision:(gibi_nanos 1.)
                           ~level_bits
                           ())
       : Time.Span.t list)] in
  durations [ 1 ];
  [%expect {|
    (2.147483648s) |}];
  durations [ 2; 1 ];
  [%expect {|
    (4.294967296s 8.589934592s) |}];
;;

module Priority_queue = struct

  open Priority_queue

  let show t = print_s [%sexp (t : _ t)]

  let create_unit ~level_bits =
    create ~level_bits:(Level_bits.create_exn level_bits) ()
  ;;

  let%expect_test "[min_allowed_key], [max_allowed_key]" =
    let test level_bits =
      let t = create_unit ~level_bits in
      require_equal [%here] (module Key) (min_allowed_key t) Key.zero;
      print_s [%message
        ""
          ~min_allowed_key:(min_allowed_key t : Key.t)
          ~max_allowed_key:(max_allowed_key t : Key.t)] in
    test [ 1 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 1)) |}];
    test [ 1; 1 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 5)) |}];
    test [ 1; 1; 1 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 11)) |}];
    test [ 2 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 3)) |}];
    test [ 3 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 7)) |}];
    test [ 3; 1 ];
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 23)) |}];
  ;;

  let%expect_test "[is_empty], [key], [length], [mem]" [@tags "64-bits-only"] =
    let t = create_unit ~level_bits:[ 1 ] in
    require [%here] (is_empty t);
    require [%here] (length t = 0);
    let e1 = add t ~key:Key.zero () in
    let e2 = add t ~key:Key.zero () in
    let show () =
      print_s [%message
        ""
          ~length:(length t : int)
          ~is_empty:(is_empty t : bool)
          ~key1:(Or_error.try_with (fun () -> Elt.key t e1) : Key.t Or_error.t)
          ~key2:(Or_error.try_with (fun () -> Elt.key t e2) : Key.t Or_error.t)
          ~mem1:(mem t e1 : bool)
          ~mem2:(mem t e2 : bool)] in
    show ();
    [%expect {|
      ((length   2)
       (is_empty false)
       (key1 (Ok 0))
       (key2 (Ok 0))
       (mem1 true)
       (mem2 true)) |}];
    remove t e1;
    show ();
    [%expect {|
      ((length   1)
       (is_empty false)
       (key1 (
         Error (
           "Timing_wheel.Priority_queue got invalid elt"
           (elt "<Obj_array.Pointer.t: 0x00000001>"))))
       (key2 (Ok 0))
       (mem1 false)
       (mem2 true)) |}];
    change_key t e2 ~key:(Key.of_int 1);
    show ();
    [%expect {|
      ((length   1)
       (is_empty false)
       (key1 (
         Error (
           "Timing_wheel.Priority_queue got invalid elt"
           (elt "<Obj_array.Pointer.t: 0x00000001>"))))
       (key2 (Ok 1))
       (mem1 false)
       (mem2 true)) |}];
    require_does_raise [%here] (fun () -> change_key t e1 ~key:(Key.of_int 1));
    [%expect {|
      ("Timing_wheel.Priority_queue got invalid elt"
       (elt "<Obj_array.Pointer.t: 0x00000001>")) |}];
    remove t e2;
    show ();
    [%expect {|
      ((length   0)
       (is_empty true)
       (key1 (
         Error (
           "Timing_wheel.Priority_queue got invalid elt"
           (elt "<Obj_array.Pointer.t: 0x00000001>"))))
       (key2 (
         Error (
           "Timing_wheel.Priority_queue got invalid elt"
           (elt "<Obj_array.Pointer.t: 0x40000008>"))))
       (mem1 false)
       (mem2 false)) |}];
  ;;

  let%expect_test "[add] failures" =
    let t = create_unit ~level_bits:[ 1 ] in
    let add ~key = ignore (add t ~key () : _ Elt.t) in
    for key = Key.to_int_exn (min_allowed_key t)
      to      Key.to_int_exn (max_allowed_key t)
    do
      add ~key:(Key.of_int key);
    done;
    let check_adds_fail () =
      List.iter
        [ Key.min_value
        ; Key.pred (min_allowed_key t)
        ; Key.succ (max_allowed_key t)
        ; Key.succ Key.max_representable
        ; Key.max_value
        ]
        ~f:(fun key ->
          require_does_raise [%here] (fun () -> add ~key))
    in
    check_adds_fail ();
    [%expect {|
      ("Timing_wheel.Priority_queue got invalid key"
       (key -4_611_686_018_427_387_904)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key -1)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 4_611_686_018_427_387_903)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _))))))) |}];
    increase_min_allowed_key t ~key:Key.one ~handle_removed:ignore;
    check_adds_fail ();
    [%expect {|
      ("Timing_wheel.Priority_queue got invalid key"
       (key -4_611_686_018_427_387_904)
       (timing_wheel (
         (min_allowed_key 1)
         (max_allowed_key 2)
         (elts ((
           (key   1)
           (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 0)
       (timing_wheel (
         (min_allowed_key 1)
         (max_allowed_key 2)
         (elts ((
           (key   1)
           (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 3)
       (timing_wheel (
         (min_allowed_key 1)
         (max_allowed_key 2)
         (elts ((
           (key   1)
           (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 1)
         (max_allowed_key 2)
         (elts ((
           (key   1)
           (value _)))))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 4_611_686_018_427_387_903)
       (timing_wheel (
         (min_allowed_key 1)
         (max_allowed_key 2)
         (elts ((
           (key   1)
           (value _))))))) |}];
    increase_min_allowed_key t ~key:(max_allowed_key t) ~handle_removed:ignore;
    check_adds_fail ();
    [%expect {|
      ("Timing_wheel.Priority_queue got invalid key"
       (key -4_611_686_018_427_387_904)
       (timing_wheel (
         (min_allowed_key 2)
         (max_allowed_key 3)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 1)
       (timing_wheel (
         (min_allowed_key 2)
         (max_allowed_key 3)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 4)
       (timing_wheel (
         (min_allowed_key 2)
         (max_allowed_key 3)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 2)
         (max_allowed_key 3)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 4_611_686_018_427_387_903)
       (timing_wheel (
         (min_allowed_key 2)
         (max_allowed_key 3)
         (elts ())))) |}];
    increase_min_allowed_key t ~key:Key.max_representable ~handle_removed:ignore;
    check_adds_fail ();
    [%expect {|
      ("Timing_wheel.Priority_queue got invalid key"
       (key -4_611_686_018_427_387_904)
       (timing_wheel (
         (min_allowed_key 2_305_843_009_213_693_951)
         (max_allowed_key 2_305_843_009_213_693_951)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_950)
       (timing_wheel (
         (min_allowed_key 2_305_843_009_213_693_951)
         (max_allowed_key 2_305_843_009_213_693_951)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 2_305_843_009_213_693_951)
         (max_allowed_key 2_305_843_009_213_693_951)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 2_305_843_009_213_693_951)
         (max_allowed_key 2_305_843_009_213_693_951)
         (elts ()))))
      ("Timing_wheel.Priority_queue got invalid key"
       (key 4_611_686_018_427_387_903)
       (timing_wheel (
         (min_allowed_key 2_305_843_009_213_693_951)
         (max_allowed_key 2_305_843_009_213_693_951)
         (elts ())))) |}];
  ;;

  let%expect_test "[clear]" =
    let t = create_unit ~level_bits:[ 1; 1 ] in
    clear t;
    let _e1 = add t ~key:Key.zero       () in
    let _e2 = add t ~key:(Key.of_int 2) () in
    show t;
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 5)
       (elts (
         ((key 0) (value _))
         ((key 2) (value _))))) |}];
    clear t;
    show t;
    [%expect {|
      ((min_allowed_key 0)
       (max_allowed_key 5)
       (elts ())) |}];
  ;;

  let increase_min_allowed_key_return_removed_keys t ~key =
    let r = ref [] in
    let handle_removed elt = r := Elt.key t elt :: !r in
    increase_min_allowed_key t ~key ~handle_removed;
    !r
  ;;

  let%expect_test "[increase_min_allowed_key] and [Key.max_representable]" =
    let t = create_unit ~level_bits:[ 1 ] in
    let add ~key = ignore (add t ~key () : _ Elt.t) in
    add ~key:Key.zero;
    add ~key:Key.one;
    require_does_raise [%here] (fun () ->
      increase_min_allowed_key t ~key:Key.( succ max_representable )
        ~handle_removed:ignore);
    [%expect {|
      ("Timing_wheel.increase_min_allowed_key got invalid key"
       (key 2_305_843_009_213_693_952)
       (timing_wheel (
         (min_allowed_key 0)
         (max_allowed_key 1)
         (elts (
           ((key 0) (value _))
           ((key 1) (value _))))))) |}];
    increase_min_allowed_key t ~key:Key.max_representable ~handle_removed:ignore;
    show t;
    [%expect {|
      ((min_allowed_key 2_305_843_009_213_693_951)
       (max_allowed_key 2_305_843_009_213_693_951)
       (elts ())) |}];
    add ~key:Key.max_representable;
    show t;
    [%expect {|
      ((min_allowed_key 2_305_843_009_213_693_951)
       (max_allowed_key 2_305_843_009_213_693_951)
       (elts ((
         (key   2_305_843_009_213_693_951)
         (value _))))) |}];
  ;;

  let%expect_test "[increase_min_allowed_key]" =
    let num_tests = ref 0 in
    (* [all_sums n] returns all combinations of nonnegative ints that sum to [n]. *)
    let all_sums n =
      let results = Array.create ~len:(n + 1) [] in
      results.( 0 ) <- [[]];
      for i = 1 to n do
        results.( i ) <-
          List.concat
            (List.init i ~f:(fun j ->
               let first = j + 1 in
               List.map results.( i - first ) ~f:(fun rest -> first :: rest)));
      done;
      results.( n )
    in
    let test ~num_bits:_ ~level_bits ~initial_min_allowed_key ~step =
      incr num_tests;
      let t = create_unit ~level_bits in
      try
        increase_min_allowed_key t ~key:initial_min_allowed_key ~handle_removed:ignore;
        require_equal [%here] (module Key) (min_allowed_key t) initial_min_allowed_key;
        require [%here]
          (Key.( >= ) (max_allowed_key t)
             (Key.add (min_allowed_key t)
                (Key.Span.of_int63 Int63.( shift_left one num_bits - one ))));
        let keys =
          List.init (Key.Span.to_int_exn
                       (Key.diff (max_allowed_key t) (min_allowed_key t)))
            ~f:(fun i -> Key.add (min_allowed_key t) (Key.Span.of_int i))
        in
        let n = ref 0 in
        List.iter keys ~f:(fun key ->
          ignore (add t ~key () : _ Elt.t);
          incr n;
          require [%here] (length t = !n));
        let removed = ref [] in
        while length t > 0 do
          let keys_removed =
            increase_min_allowed_key_return_removed_keys t
              ~key:(Key.min Key.max_representable
                      (Key.add (min_allowed_key t) step))
          in
          removed := keys_removed @ !removed;
          List.iter keys_removed ~f:(fun key ->
            require [%here] (Key.( < ) key (min_allowed_key t)))
        done;
        let keys_removed = List.sort !removed ~compare:Key.compare in
        require [%here] (Poly.equal keys_removed keys);
      with exn ->
        failwiths "failure" (exn, t) [%sexp_of: exn * _ t]
    in
    let num_bits = 6 in
    let all_sums = all_sums num_bits in
    List.iter
      [ Key.zero
      ; Key.sub Key.max_representable
          (Key.Span.of_int63 (Int63.shift_left Int63.one num_bits))]
      ~f:(fun initial_min_allowed_key ->
        for step = 1 to 1 lsl num_bits do
          List.iter all_sums ~f:(fun level_bits ->
            test ~num_bits ~level_bits ~initial_min_allowed_key
              ~step:(Key.Span.of_int step))
        done);
    print_s [%message (num_tests : int ref)];
    [%expect {|
      (num_tests 4_096) |}];
  ;;

  module Key_option = struct
    type t = Key.t option [@@deriving compare, sexp_of]
    let equal = [%compare.equal: t]
  end

  let%expect_test "[increase_min_allowed_key]" =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    require [%here] (is_none (min_key t));
    let _elt = add t ~key:Key.zero () in
    require_equal [%here] (module Key_option) (min_key t) (Some Key.zero);
    let max_key = 10 in
    for key = 1 to max_key; do
      let key = Key.of_int key in
      require_does_not_raise [%here] (fun () -> ignore (add t ~key () : _ Elt.t));
      require_equal [%here] (module Key_option) (min_key t) (Some Key.zero);
    done;
    for key = 1 to max_key + 1; do
      let key = Key.of_int key in
      begin match increase_min_allowed_key_return_removed_keys t ~key with
      | [ key' ] -> require_equal [%here] (module Key) key' (Key.pred key)
      | _ -> require [%here] false
      end;
      require_equal [%here] (module Key_option)
        (min_key t)
        (if Key.( <= ) key (Key.of_int max_key) then Some key else None);
    done;
  ;;

  let%expect_test "[min_key]" =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    let max_key = Key.of_int 10 in
    let elts =
      List.init (Key.to_int_exn max_key + 1)
        ~f:(fun key -> add t ~key:(Key.of_int key) ())
    in
    List.iter elts ~f:(fun elt ->
      let key = Elt.key t elt in
      remove t elt;
      require_equal [%here] (module Key_option)
        (min_key t)
        (if Key.( < ) key max_key then Some (Key.succ key) else None))
  ;;

  let%expect_test "[iter]" =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    let count () =
      let r = ref 0 in
      iter t ~f:(fun _ -> incr r);
      !r
    in
    let show_count () = print_s [%sexp (count () : int)] in
    show_count ();
    [%expect {|
      0 |}];
    let num_elts = 10 in
    for key = 0 to num_elts - 1; do
      ignore (add t ~key:(Key.of_int key) () : _ Elt.t);
    done;
    show_count ();
    [%expect {|
      10 |}];
    increase_min_allowed_key t ~key:Key.one ~handle_removed:ignore;
    show_count ();
    [%expect {|
      9 |}];
    increase_min_allowed_key t ~key:(Key.of_int num_elts) ~handle_removed:ignore;
    show_count ();
    [%expect {|
      0 |}];
  ;;

  let%expect_test "[iter]" =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    let elts = ref [] in
    for key = 0 to Key.to_int_exn (max_allowed_key t) do
      elts := add t ~key:(Key.of_int key) () :: !elts
    done;
    let elts' = ref [] in
    iter t ~f:(fun elt -> elts' := elt :: !elts');
    let sort elts =
      List.sort elts ~compare:(fun elt1 elt2 ->
        Key.compare (Elt.key t elt1) (Elt.key t elt2))
    in
    require [%here] (List.equal ~equal:phys_equal (sort !elts) (sort !elts'))
  ;;
end

let create_unit ?level_bits ?(start = Time.epoch) ?(alarm_precision = gibi_nanos 1.) () =
  create
    ~config:(create_config ?level_bits () ~alarm_precision)
    ~start
;;

let%expect_test "start after epoch" =
  let t = create_unit ~start:(Time.add Time.epoch (gibi_nanos 1.)) () in
  invariant ignore t;
;;

let%expect_test "invalid alarm precision" =
  let test alarm_precision =
    require_does_raise [%here] (fun () -> create_unit ~alarm_precision ()) in
  test (gibi_nanos (-1.));
  [%expect {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s)) |}];
  test (gibi_nanos 0.);
  [%expect {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0ns)) |}];
;;

let%expect_test "[interval_num_start], [interval_start]" =
  let t = create_unit () in
  require [%here] (not (mem t (Alarm.null ())));
  let start = start t in
  let test after =
    let time = Time.add start (gibi_nanos after) in
    let interval_num = interval_num t time in
    let interval_num_start = interval_num_start t interval_num in
    let interval_start = interval_start t time in
    print_s [%message
      ""
        (interval_num : Interval_num.t)
        (interval_num_start : Time.t)
        (interval_start : Time.t)];
    require [%here] (Time.equal interval_num_start interval_start) in
  test 0.;
  [%expect {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 0.1;
  [%expect {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 0.99;
  [%expect {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 1.;
  [%expect {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 1.5;
  [%expect {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 1.99;
  [%expect {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 2.;
  [%expect {|
    ((interval_num       2)
     (interval_num_start "1970-01-01 00:00:02.147483648Z")
     (interval_start     "1970-01-01 00:00:02.147483648Z")) |}];
;;

let%expect_test "[advance_clock]" =
  let t = create_unit () in
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s)))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:00Z")
     (alarms ())) |}];
  let to_ = Time.add (now t) (gibi_nanos 1.) in
  advance_clock t ~to_ ~handle_fired:ignore;
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s)))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:01.073741824Z")
     (alarms ())) |}];
;;

let%expect_test "[is_empty], [length]" =
  let t = create_unit () in
  let show () =
    print_s [%message
      ""
        ~is_empty:(is_empty t : bool)
        ~length:(length t : int)] in
  show ();
  [%expect {|
    ((is_empty true)
     (length   0)) |}];
  let alarm = add t ~at:(now t) () in
  show ();
  [%expect {|
    ((is_empty false)
     (length   1)) |}];
  remove t alarm;
  show ();
  [%expect {|
    ((is_empty true)
     (length   0)) |}];
;;

let%expect_test "[iter]" =
  let t = create_unit () in
  iter t ~f:(fun _ -> require [%here] false);
  let alarm1 = add t ~at:(now t) () in
  iter t ~f:(fun alarm -> require [%here] (phys_equal alarm alarm1));
  let alarm2 = add t ~at:(now t) () in
  let r = ref 0 in
  iter t ~f:(fun alarm ->
    require [%here] (phys_equal alarm alarm1 || phys_equal alarm alarm2);
    incr r);
  print_s [%message (r : int ref)];
  [%expect {|
    (r 2) |}];
  remove t alarm1;
  remove t alarm2;
  iter t ~f:(fun _ -> require [%here] false);
;;

let%expect_test "access to a removed alarm doesn't segfault" =
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ())
      ~start:Time.epoch
  in
  let alarm = add t ~at:(Time.add (now t) (gibi_nanos 5.)) (ref 1) in
  let show_mem () = print_s [%sexp (mem t alarm : bool)] in
  show_mem ();
  [%expect {|
    true |}];
  remove t alarm;
  show_mem ();
  [%expect {|
    false |}];
  require_does_raise [%here] (fun _ -> Alarm.interval_num t alarm);
  [%expect {|
    ("Timing_wheel.Priority_queue got invalid elt"
     (elt "<Obj_array.Pointer.t: 0x00000001>")) |}];
  require_does_raise [%here] (fun _ -> Alarm.at t alarm);
  [%expect {|
    ("Timing_wheel.Priority_queue got invalid elt"
     (elt "<Obj_array.Pointer.t: 0x00000001>")) |}];
  require_does_raise [%here] (fun _ -> Alarm.value t alarm);
  [%expect {|
    ("Timing_wheel.Priority_queue got invalid elt"
     (elt "<Obj_array.Pointer.t: 0x00000001>")) |}]
;;

(* Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing
   wheel but reschedule its scheduled time. *)
let test_reschedule reschedule =
  let epoch_plus n_seconds = Time.add Time.epoch (gibi_nanos n_seconds) in
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ())
      ~start:(epoch_plus 0.)
  in
  (* add alarm1 before alarm2, test initial conditions *)
  let alarm1 = add t ~at:(epoch_plus 5.)  () in
  let alarm2 = add t ~at:(epoch_plus 10.) () in
  let show () =
    let alarm_at alarm = if mem t alarm then Some (Alarm.at t alarm) else None in
    print_s [%message
      ""
        ~now:(now t : Time.t)
        ~next_alarm_fires_at:(next_alarm_fires_at t : Time.t option)
        ~alarm1_at:(alarm_at alarm1 : Time.t option)
        ~alarm2_at:(alarm_at alarm2 : Time.t option)]
  in
  show ();
  print_endline "Reschedule alarm1 after alarm2; alarm2 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 15.);
  show ();
  print_endline "Advance time past alarm1's original time; nothing fires.";
  advance_clock t ~to_:(epoch_plus 7.) ~handle_fired:(fun _ -> require [%here] false);
  show ();
  print_endline "Reschedule alarm1 before alarm2 again; alarm1 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 8.);
  show ();
  print_endline "Advance time past alarm1, alarm1 fires but alarm2 does not.";
  advance_clock t ~to_:(epoch_plus 9.) ~handle_fired:ignore;
  show ();
  print_endline "Cannot reschedule the already-fired alarm1.";
  require_does_raise [%here] (fun _ -> reschedule t alarm1 ~at:(epoch_plus 20.));
  show ();
  print_endline "Cannot reschedule before current time.";
  require_does_raise [%here] (fun _ -> reschedule t alarm2 ~at:(epoch_plus 8.));
  show ();
  print_endline "Cannot reschedule arbitrarily far in the future.";
  require_does_raise [%here] (fun _ ->
    reschedule t alarm2 ~at:(Time.add (alarm_upper_bound t) (gibi_nanos 1.)));
  print_endline "Fire alarm2.";
  advance_clock t ~to_:(epoch_plus 11.) ~handle_fired:ignore;
  show ();
;;

let%expect_test "[reschedule]" =
  test_reschedule (fun t alarm ~at -> reschedule t alarm ~at);
  [%expect {|
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
     (alarm1_at           ("1970-01-01 00:00:05.36870912Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 after alarm2; alarm2 becomes next.
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at           ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1's original time; nothing fires.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at           ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 before alarm2 again; alarm1 becomes next.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
     (alarm1_at           ("1970-01-01 00:00:08.589934592Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1, alarm1 fires but alarm2 does not.
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule the already-fired alarm1.
    (Failure "Timing_wheel_ns cannot reschedule alarm not in timing wheel")
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule before current time.
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1970-01-01 00:00:08.589934592Z")
     (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule arbitrarily far in the future.
    ("Timing_wheel cannot schedule alarm that far in the future"
     (at                "2104-11-29 00:00:00.789250048Z")
     (alarm_upper_bound "2104-11-28 23:59:59.715508224Z"))
    Fire alarm2.
    ((now "1970-01-01 00:00:11.811160064Z")
     (next_alarm_fires_at ())
     (alarm1_at           ())
     (alarm2_at           ())) |}];
;;

let%expect_test "[reschedule_at_interval_num]" =
  test_reschedule (fun t alarm ~at ->
    reschedule_at_interval_num t alarm ~at:(interval_num t at));
  [%expect {|
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
     (alarm1_at           ("1970-01-01 00:00:05.36870912Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 after alarm2; alarm2 becomes next.
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at           ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1's original time; nothing fires.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at           ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 before alarm2 again; alarm1 becomes next.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
     (alarm1_at           ("1970-01-01 00:00:08.589934592Z"))
     (alarm2_at           ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1, alarm1 fires but alarm2 does not.
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule the already-fired alarm1.
    (Failure "Timing_wheel_ns cannot reschedule alarm not in timing wheel")
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule before current time.
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1970-01-01 00:00:08.589934592Z")
     (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule arbitrarily far in the future.
    ("Timing_wheel.interval_num got time too far in the future"
     (time "2104-11-29 00:00:00.789250048Z"))
    Fire alarm2.
    ((now "1970-01-01 00:00:11.811160064Z")
     (next_alarm_fires_at ())
     (alarm1_at           ())
     (alarm2_at           ())) |}];
;;

let%expect_test "[advance_clock] fires alarms at the right time" =
  let test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
    let t =
      create ~config:(create_config ~alarm_precision ()) ~start:Time.epoch
    in
    for i = 1 to num_alarms do
      let at = Time.add (now t) (Time.Span.scale alarm_separation (Float.of_int i)) in
      ignore (add t ~at (fun () -> require [%here] (Time.( <= ) at (now t)))
              : _ Alarm.t);
    done;
    while not (is_empty t) do
      let to_ = Time.add (now t) advance_by in
      advance_clock t ~to_ ~handle_fired:(fun alarm -> Alarm.value t alarm ());
      require_equal [%here] (module Interval_num)
        (now_interval_num t) (interval_num t to_);
    done;
  in
  List.iter
    [ add
    ; (fun t ~at a -> add_at_interval_num t ~at:(interval_num t at) a)
    ]
    ~f:(fun add ->
      List.iter [ 100 ] ~f:(fun num_alarms ->
        List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
          let alarm_precision = gibi_nanos s in
          List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
            let alarm_separation = gibi_nanos s in
            List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
              let advance_by = gibi_nanos s in
              test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by)))))
;;

let%expect_test "[add] and [advance_clock]" =
  let t =
    create
      ~config:(create_config
                 ~alarm_precision:(gibi_nanos 1.)
                 ~level_bits:[ 10 ]
                 ())
      ~start:Time.epoch
  in
  let add ~after f = ignore (add t ~at:(Time.add (now t) after) f : _ Alarm.t) in
  let advance_clock by =
    advance_clock t ~to_:(Time.add (now t) by)
      ~handle_fired:(fun alarm -> Alarm.value t alarm ())
  in
  require_does_raise [%here] (fun () -> add ~after:(gibi_nanos (-1.)) ignore);
  [%expect {|
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1969-12-31 23:59:58.926258176Z")
     (now_interval_num_start "1970-01-01 00:00:00Z")) |}];

  require_equal [%here] (module Time) (alarm_upper_bound t) (Time.add (now t) (gibi_nanos 1024.));
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:00Z")
     (alarms ())) |}];
  add ~after:(gibi_nanos 30.) ignore;
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:00Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _))))) |}];
  advance_clock (gibi_nanos 30.);
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:32.21225472Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _))))) |}];
  advance_clock (gibi_nanos 1.);
  show t;
  [%expect {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 3_964_975_476)
     (now              "1970-01-01 00:00:33.285996544Z")
     (alarms ())) |}];
;;

let%expect_test "[next_alarm_fires_at]" =
  let t = create_unit ~level_bits:[ 10 ] () in
  let next_alarm_fires_after () =
    print_s [%message
      ""
        ~next_alarm_fires_after:(
          Option.map (next_alarm_fires_at t)
            ~f:(fun time -> Time.diff time Time.epoch)
          : Time.Span.t option)]
  in
  let add_at at =
    ignore (add t ~at:(Time.add Time.epoch at) () : _ Alarm.t);
    next_alarm_fires_after ();
  in
  let advance_clock span =
    advance_clock t ~to_:(Time.add Time.epoch span) ~handle_fired:ignore;
    next_alarm_fires_after ();
  in
  add_at (gibi_nanos 2.);
  [%expect {|
    (next_alarm_fires_after (3.221225472s)) |}];
  add_at (gibi_nanos 1.5);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 1.0);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 0.5);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  add_at (gibi_nanos 0.1);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 0.5);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 1.);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 1.5);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 2.);
  [%expect {|
    (next_alarm_fires_after (3.221225472s)) |}];
  advance_clock (gibi_nanos 3.);
  [%expect {|
    (next_alarm_fires_after ()) |}];
;;

let%expect_test "\
[fire_past_alarms] - all possible subsets of alarms in the first bucket that fire" =
  let start = Time.epoch in
  let at sec = Time.add start (gibi_nanos sec) in
  let at1 = at 1. in
  let at2 = at 2. in
  let num_tests = ref 0 in
  for num_elts = 0 to 5 do
    let rec loop i ats =
      incr num_tests;
      if i > 0
      then (
        loop (i - 1) (at1 :: ats);
        loop (i - 1) (at2 :: ats))
      else (
        let t =
          create ~start
            ~config:(create_config ~alarm_precision:(gibi_nanos 60.) ())
        in
        let num_fired = ref 0 in
        List.iter ats ~f:(fun at ->
          let alarm = add t ~at () in
          require_equal [%here] (module Interval_num)
            (Alarm.interval_num t alarm)
            Interval_num.zero);
        advance_clock t ~to_:at1 ~handle_fired:(fun _ -> require [%here] false);
        fire_past_alarms t ~handle_fired:(fun alarm ->
          if Time.equal (Alarm.at t alarm) at1
          then incr num_fired
          else require [%here] false);
        require_equal [%here] (module Int)
          !num_fired
          (List.count ats ~f:(Time.equal at1)));
    in
    loop num_elts []
  done;
  print_s [%message (num_tests : int ref)];
  [%expect {|
    (num_tests 120) |}];
;;

let%expect_test "alarm buckets" =
  let start = Time.epoch in
  let t : bool ref t =
    create ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ()) ~start
  in
  let handle_fired (a : bool ref Alarm.t) : unit =
    let r = Alarm.value t a in
    require [%here] (not !r);
    r := true
  in
  let precision = alarm_precision t in
  let precision_0_2 = Time.Span.scale precision 0.2 in
  let _ = add t ~at:(Time.add start precision) (ref false) in
  let base = next_alarm_fires_at t |> Option.value_exn in
  let step0 = Time.add base  precision_0_2 in
  let step1 = Time.add step0 precision_0_2 in
  let step2 = Time.add step1 precision_0_2 in
  let step3 = Time.add step2 precision in
  (* Check all alarm will be in the same bucket but step3 *)
  let interval_num0  = interval_num t step0 in
  let interval_num1  = interval_num t step1 in
  let interval_num2  = interval_num t step2 in
  let interval_num3  = interval_num t step3 in
  print_s [%message
    ""
      (interval_num0 : Interval_num.t)
      (interval_num1 : Interval_num.t)
      (interval_num2 : Interval_num.t)
      (interval_num3 : Interval_num.t)];
  [%expect {|
    ((interval_num0 2)
     (interval_num1 2)
     (interval_num2 2)
     (interval_num3 3)) |}];
  let step1_fired = ref false in
  let step2_fired = ref false in
  let step3_fired = ref false in
  let _ = add t ~at:step1 step1_fired in
  let _ = add t ~at:step2 step2_fired in
  let _ = add t ~at:step3 step3_fired in
  let show () =
    print_s [%message
      ""
        (step1_fired : bool ref)
        (step2_fired : bool ref)
        (step3_fired : bool ref)]
  in
  (* Nothing should be triggered before *)
  advance_clock t ~to_:step0 ~handle_fired;
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  advance_clock t ~to_:step1 ~handle_fired;
  show ();
  [%expect {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  show ();
  [%expect {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false)) |}];
  advance_clock t ~to_:step2 ~handle_fired;
  show ();
  [%expect {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false)) |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect {|
    ((step1_fired true)
     (step2_fired true)
     (step3_fired false)) |}];
;;

let%expect_test "[max_alarm_time_in_min_interval]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  let max_alarm_time_in_min_interval () =
    print_s [%sexp (max_alarm_time_in_min_interval t : Time.t option)] in
  max_alarm_time_in_min_interval ();
  [%expect {|
    () |}];
  let add_after span = add t ~at:(Time.add (now t) span) ignore in
  let a = add_after (gibi_nanos 0.5) in
  max_alarm_time_in_min_interval ();
  [%expect {|
    ("1970-01-01 00:00:00.536870912Z") |}];
  remove t a;
  max_alarm_time_in_min_interval ();
  [%expect {|
    () |}];
  (* Add two alarms that appear in different intervals, but in the same slot on the
     second level of the timing wheel.  So the correct [max_alarm_time_in_min_interval] is
     2.1, not 3.9. *)
  let _ = add_after (gibi_nanos 2.1) in
  let _ = add_after (gibi_nanos 3.9) in
  max_alarm_time_in_min_interval ();
  [%expect {|
    ("1970-01-01 00:00:02.25485783Z") |}];
;;

let%expect_test "multiple alarms at the same time are fired in insertion order" =
  let t     = create_unit () in
  let delta = Time_ns.Span.of_sec 1. in
  let at    = Time_ns.(add epoch delta) in
  for i = 0 to 5 do
    ignore (add t ~at i);
  done;
  advance_clock t ~to_:(Time_ns.add at delta) ~handle_fired:(fun alarm ->
    print_s [%sexp (Alarm.value t alarm : int)]);
  [%expect {|
    0
    1
    2
    3
    4
    5 |}]
;;
