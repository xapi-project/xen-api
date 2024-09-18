module Timer = Clock.Timer
module Gen = QCheck2.Gen
module Test = QCheck2.Test

let spans =
  Gen.oneofa ([|1; 100; 300|] |> Array.map (fun v -> Mtime.Span.(v * ms)))

let test_timer_remaining =
  let print = Fmt.to_to_string Mtime.Span.pp in
  Test.make ~count:20 ~name:"Timer.remaining" ~print spans @@ fun duration ->
  let timer = Timer.start ~duration in
  let half = Timer.span_to_s duration /. 2. in
  let elapsed = Mtime_clock.counter () in
  Printf.printf "Sleeping for %f seconds...\n" half ;
  Unix.sleepf half ;
  let actual = Mtime_clock.count elapsed in
  (* We expect to have slept [half] seconds, but we could've been woken up later
     by the OS, it'll never be exact. Check that we're not too far off, or the
     Expired / Remaining test below will be wrong.
     The following equation must hold:
     [duration / 2 <= actual < duration]
  *)
  QCheck2.assume (Timer.span_is_shorter actual ~than:duration) ;
  QCheck2.assume
    (not (Timer.span_is_shorter Mtime.Span.(2 * actual) ~than:duration)) ;
  let () =
    match Timer.remaining timer with
    | Expired _ when half < 0.05 ->
        (* OS timer may not be accurate for very short sleeps,
           or the system might be busy.
           Skip the strict test on very short durations, we'll still test this on the 100ms+ ones.
        *)
        ()
    | Expired t ->
        Test.fail_reportf
          "Expected to have spare time, but got excess: %a. Duration: %a, \
           actual: %a, timer: %a"
          Mtime.Span.pp t Mtime.Span.pp duration Mtime.Span.pp actual Timer.pp
          timer
    | Remaining t ->
        if Timer.span_is_longer Mtime.Span.(2 * t) ~than:duration then
          Test.fail_reportf
            "Expected to have less than half spare time, but got: %a. \
             Duration: %a, actual: %a, timer: %a"
            Mtime.Span.pp t Mtime.Span.pp duration Mtime.Span.pp actual Timer.pp
            timer
  in

  (* 3 * half > duration, so we expect Excess to be reported now *)
  Unix.sleepf (2. *. half) ;
  let actual = Mtime_clock.count elapsed in
  QCheck2.assume (Timer.span_is_longer actual ~than:duration) ;
  let () =
    match Timer.remaining timer with
    | Expired _ ->
        ()
    | Remaining t ->
        Test.fail_reportf
          "Expected to have excess time, but got spare: %a. Duration: %a, \
           actual: %a, timer: %a"
          Mtime.Span.pp t Mtime.Span.pp duration Mtime.Span.pp actual Timer.pp
          timer
  in
  if not (Timer.has_expired timer) then
    Test.fail_reportf "Expected Timer to have expired. Duration: %a, timer: %a"
      Mtime.Span.pp duration Timer.pp timer ;
  true

let combinations =
  let pair x y = (x, y) in
  let rec loop acc = function
    | x :: xs ->
        let acc = List.map (pair x) xs :: acc in
        loop acc xs
    | [] ->
        List.(concat (rev acc))
  in
  loop []

let test_span_compare =
  let shortest = Mtime.Span.zero in
  let long = Mtime.Span.of_uint64_ns Int64.max_int in
  let longest = Mtime.Span.of_uint64_ns (-1L) in
  let spec = combinations [shortest; long; longest] in
  let pp_spec () = Fmt.str "%a" (Fmt.Dump.pair Mtime.Span.pp Mtime.Span.pp) in
  let test_shorter (a, b) () =
    let ( < ) a b = Mtime.Span.compare a b < 0 in
    Alcotest.(check bool)
      "is_shorter doesn't match compare" (a < b)
      (Timer.span_is_shorter a ~than:b)
  in
  let tests_shorter =
    List.map
      (fun t ->
        (Printf.sprintf "is_shorter %a" pp_spec t, `Quick, test_shorter t)
      )
      spec
  in
  let test_longer (a, b) () =
    let ( > ) a b = Mtime.Span.compare a b > 0 in
    Alcotest.(check bool)
      "is_longer doesn't match compare" (a > b)
      (Timer.span_is_longer a ~than:b)
  in
  let tests_longer =
    List.map
      (fun t -> (Printf.sprintf "is_longer %a" pp_spec t, `Quick, test_longer t))
      spec
  in
  List.concat [tests_shorter; tests_longer]

let test_conversion_to_s =
  let shortest = Mtime.Span.zero in
  let long = Mtime.Span.(104 * day) in
  let longer = Mtime.Span.(105 * day) in
  let spec = [(shortest, 0.); (long, 8.9856e+06); (longer, 9.072e+06)] in
  let pp_spec () = Fmt.str "%a" Fmt.(Dump.pair Mtime.Span.pp float) in
  let test_span_to_s (input, expected) () =
    let actual = Timer.span_to_s input in
    Alcotest.(check (float Float.epsilon))
      "seconds match span length" expected actual
  in
  List.map
    (fun t ->
      (Printf.sprintf "span_to_s %a" pp_spec t, `Quick, test_span_to_s t)
    )
    spec

let test_conversion_from_s =
  let span = Alcotest.testable Mtime.Span.pp Mtime.Span.equal in
  let shortest = 0. in
  let short_enough = 9_007_199.254_740_991 in
  let too_long = 9_007_199.254_740_992 in
  let neg = -1. in
  let spec =
    let open Mtime.Span in
    [
      (shortest, Some zero)
    ; (short_enough, Some (9_007_199_254_740_991 * ns))
    ; (too_long, None)
    ; (neg, None)
    ]
  in
  let pp_spec () =
    Fmt.str "%a" Fmt.(Dump.pair float (Dump.option Mtime.Span.pp))
  in
  let test_span_to_s (input, expected) () =
    let actual = Timer.s_to_span input in
    Alcotest.(check @@ option span)
      "span length matches seconds" expected actual
  in
  List.map
    (fun t ->
      (Printf.sprintf "span_to_s %a" pp_spec t, `Quick, test_span_to_s t)
    )
    spec

let tests_span =
  List.concat [test_conversion_to_s; test_conversion_from_s; test_span_compare]

let tests = [test_timer_remaining]
