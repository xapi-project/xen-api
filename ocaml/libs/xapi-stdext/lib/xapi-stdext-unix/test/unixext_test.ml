open QCheck2
open Xapi_stdext_unix
open Xapi_fd_test

let expect_string ~expected ~actual =
  if not (String.equal expected actual) then
    Test.fail_reportf "Data sent and observed do not match: %S <> %S" expected
      actual

let expect_amount ~expected observation =
  let open Observations in
  let actual = String.length observation.data in
  if expected <> actual then
    Test.fail_reportf
      "Amount of data available and transferred does not match: %d <> %d;@,%a"
      expected actual pp observation

let skip_blk = function
  | Unix.S_BLK ->
      if Unix.geteuid () <> 0 then
        QCheck2.assume_fail ()
  | _ ->
      ()

let skip_dirlnk = function
  | Unix.S_DIR | Unix.S_LNK ->
      QCheck2.assume_fail ()
  | _ ->
      ()

(*
let pp_pair =
  let open Observations in
  Fmt.(record
    [ field "read" (fun t -> t.read) pp
    ; field "write" (fun t -> t.write) pp
    ; field "elapsed" (fun t -> t.elapsed) Mtime.Span.pp
   ]
  )
*)

let print_timeout = Fmt.to_to_string Mtime.Span.pp

let test_time_limited_write =
  let timeouts = Generate.timeouts in
  let gen = Gen.tup2 Generate.t timeouts
  and print = Print.tup2 Generate.print print_timeout in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout_span) ->
  let timeout = Unixext.Timeout.of_span timeout_span in
  skip_blk behaviour.kind ;
  skip_dirlnk behaviour.kind ;
  try
    let test_elapsed = ref Mtime.Span.zero in
    let test wrapped_fd =
      let len = behaviour.size in
      let buf = String.init len (fun i -> Char.chr (i mod 255)) in
      let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
      Unix.set_nonblock fd ;
      let dt = Mtime_clock.counter () in
      let deadline = Unixext.Timer.start ~timeout in
      let finally () = test_elapsed := Mtime_clock.count dt in
      Fun.protect ~finally (fun () ->
          Unixext.time_limited_write_substring fd len buf deadline
      ) ;
      buf
    in
    (*Printf.eprintf "testing write: %s\n%!" (print (behaviour, timeout)) ;*)
    let observations, result = Generate.run_wo behaviour ~f:test in
    let () =
      let open Observations in
      let elapsed = !test_elapsed in
      let timeout_extra =
        Mtime.Span.(add (timeout :> Mtime.Span.t) @@ (500 * ms))
      in
      if Mtime.Span.compare elapsed timeout_extra > 0 then
        Test.fail_reportf
          "Function duration significantly exceeds timeout: %a > %s; %s"
          Mtime.Span.pp elapsed
          Unixext.Timeout.(to_string timeout)
          (Fmt.to_to_string Fmt.(option pp) observations.Observations.read) ;
      match (observations, result) with
      | {read= Some read; _}, Ok expected ->
          (* expected is the input given to [time_limited_write_substring] *)
          expect_amount ~expected:(String.length expected) read ;
          expect_string ~expected ~actual:read.data
      | {read= Some read; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
          let elapsed = !test_elapsed in
          if Mtime.Span.compare elapsed timeout_span < 0 then
            Test.fail_reportf "Timed out earlier than requested: %a < %a"
              Mtime.Span.pp elapsed Mtime.Span.pp timeout_span ;
          let actual = String.length read.data in
          if actual >= behaviour.size then
            Test.fail_reportf "Timed out, but transferred enough data: %d >= %d"
              actual behaviour.size
      | ( {read= Some read; _}
        , Error (`Exn_trap (Unix.Unix_error (Unix.EPIPE, _, _), _)) ) ->
          if String.length read.data = behaviour.size then
            Test.fail_reportf
              "Transferred exact amount, shouldn't have tried to send more: %d"
              behaviour.size
      | {read= None; _}, _ ->
          ()
      | _, Error (`Exn_trap (e, bt)) ->
          Printexc.raise_with_backtrace e bt
    in
    true
  with e ->
    Format.eprintf "Error: %a@." Fmt.exn_backtrace
      (e, Printexc.get_raw_backtrace ()) ;
    false

let test_time_limited_read =
  let gen = Gen.tup2 Generate.t Generate.timeouts
  and print = Print.tup2 Generate.print print_timeout in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout_span) ->
  let timeout = Unixext.Timeout.of_span timeout_span in
  (* Format.eprintf "Testing %s@." (print (behaviour, timeout)); *)
  skip_blk behaviour.kind ;
  skip_dirlnk behaviour.kind ;
  let test_elapsed = ref Mtime.Span.zero in
  let test wrapped_fd =
    let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
    Unix.set_nonblock fd ;
    let dt = Mtime_clock.counter () in
    let deadline = Unixext.Timer.start ~timeout in
    let finally () = test_elapsed := Mtime_clock.count dt in
    Fun.protect ~finally (fun () ->
        Unixext.time_limited_read fd behaviour.size deadline
    )
  in
  (*Printf.eprintf "testing: %s\n%!" (print (behaviour, timeout)) ;*)
  let observations, result =
    let buf = String.init behaviour.size (fun i -> Char.chr (i mod 255)) in
    Generate.run_ro behaviour buf ~f:test
  in
  let () =
    let open Observations in
    let elapsed = !test_elapsed in
    let timeout_extra =
      Mtime.Span.(add (timeout :> Mtime.Span.t) @@ (500 * ms))
    in
    if Mtime.Span.compare elapsed timeout_extra > 0 then
      Test.fail_reportf
        "Function duration significantly exceeds timeout: %a > %a; %s"
        Mtime.Span.pp elapsed Unixext.Timeout.pp timeout
        (Fmt.to_to_string Fmt.(option pp) observations.Observations.write) ;
    (* Format.eprintf "Result: %a@." (Fmt.option Observations.pp) observations.write;*)
    match (observations, result) with
    | {write= Some write; _}, Ok actual ->
        expect_amount ~expected:(String.length actual) write ;
        expect_string ~expected:write.data ~actual
    | {write= Some _; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
        let elapsed = !test_elapsed in
        if Mtime.Span.compare elapsed timeout_span < 0 then
          Test.fail_reportf "Timed out earlier than requested: %a < %a"
            Mtime.Span.pp elapsed Mtime.Span.pp timeout_span
    | ( {write= Some write; _}
      , Error (`Exn_trap (Unix.Unix_error (Unix.EPIPE, _, _), _)) ) ->
        if String.length write.data = behaviour.size then
          Test.fail_reportf
            "Transferred exact amount, shouldn't have tried to send more: %d"
            behaviour.size
    | {write= None; _}, _ ->
        ()
    | _, Error (`Exn_trap (e, bt)) ->
        Printexc.raise_with_backtrace e bt
  in
  true

let test_proxy =
  let gen = Generate.t and print = Generate.print in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun behaviour ->
  if behaviour.kind <> Unix.S_SOCK then
    QCheck2.assume_fail () ;
  let test wrapped_fd =
    let buf = String.init behaviour.size (fun i -> Char.chr (i mod 255)) in
    let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
    let test2 wrapped_fd2 =
      let fd2 = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd2 in
      Unixext.proxy (Unix.dup fd) (Unix.dup fd2)
    in
    match Generate.run_rw behaviour buf ~f:test2 with
    | _, Error (`Exn_trap (e, bt)) ->
        Printexc.raise_with_backtrace e bt
    | obs, Ok () ->
        obs
  in
  let buf' =
    String.init behaviour.size (fun i -> Char.chr ((30 + i) mod 255))
  in
  match Generate.run_rw behaviour buf' ~f:test with
  | _, Error (`Exn_trap (e, bt)) ->
      Printexc.raise_with_backtrace e bt
  | {read= None; _}, Ok _ ->
      false
  | _, Ok {write= None; _} ->
      false
  | {read= Some write; _}, Ok {write= Some read; _} ->
      expect_string ~expected:write.data ~actual:read.data ;
      true

let timeout_equal (a : Unixext.Timeout.t) (b : Unixext.Timeout.t) =
  Mtime.Span.equal (a :> Mtime.Span.t) (b :> Mtime.Span.t)

let mtime_span_gen =
  let open Gen in
  (* We can't use Mtime.Span.max_span, because that is too big for float,
     only works for int64 conversion, and there is no Mtime.Span.max_span_float.
     The documentation says that 2^53 is the maximum though, so use that.
     Otherwise we'll fail later when converting to string and back goes through float.

     Use microseconds instead of nanoseconds, because nanoseconds have rounding
     errors during conversion.
  *)
  let+ usval = 0 -- (((1 lsl 53) - 1) / 1_000_000) in
  Mtime.Span.(usval * us)

let test_timeout_string_conv =
  let gen = mtime_span_gen and print = Fmt.to_to_string Mtime.Span.pp in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun timeout_span ->
  let timeout = Unixext.Timeout.of_span timeout_span in
  let str = Unixext.Timeout.to_string timeout in
  let timeout' = Unixext.Timeout.of_string str in
  if not (timeout_equal timeout timeout') then
    Test.fail_reportf
      "timeout not equal after round-trip through %S: %Lu (%a) <> %Lu (%a)" str
      (Mtime.Span.to_uint64_ns (timeout :> Mtime.Span.t))
      Unixext.Timeout.pp timeout
      (Mtime.Span.to_uint64_ns (timeout' :> Mtime.Span.t))
      Unixext.Timeout.pp timeout' ;
  true

let spans =
  Gen.oneofa ([|1; 100; 300|] |> Array.map (fun v -> Mtime.Span.(v * ms)))

let seconds_of_timeout (t : Unixext.Timeout.t) =
  1e-9 *. ((t :> Mtime.Span.t) |> Mtime.Span.to_float_ns)

let test_timer_remaining =
  let gen = Gen.map Unixext.Timeout.of_span spans
  and print = Fmt.to_to_string Unixext.Timeout.pp in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun timeout ->
  let timeout_span = (timeout :> Mtime.Span.t) in
  let timer = Unixext.Timer.start ~timeout in
  let half = seconds_of_timeout timeout /. 2. in
  let elapsed = Mtime_clock.counter () in
  Unix.sleepf half ;
  let actual = Mtime_clock.count elapsed in
  (* We expect to have slept [half] seconds, but we could've been woken up later by the OS, it'll never be exact.
     Check that we're not too far off, or the Excess/Spare test below will be wrong.
     The following equation must hold:
     [timeout / 2 <= actual < timeout]
  *)
  QCheck2.assume (Mtime.Span.compare actual timeout_span < 0) ;
  QCheck2.assume (Mtime.Span.compare Mtime.Span.(2 * actual) timeout_span >= 0) ;
  let () =
    match Unixext.Timer.remaining timer with
    | Excess t ->
        Test.fail_reportf
          "Expected to have spare time, but got excess: %a. Timeout: %a, \
           actual: %a, timer: %a"
          Mtime.Span.pp t Unixext.Timeout.pp timeout Mtime.Span.pp actual
          Unixext.Timer.pp timer
    | Spare t ->
        if Mtime.Span.compare Mtime.Span.(2 * t) (timeout :> Mtime.Span.t) > 0
        then
          Test.fail_reportf
            "Expected to have less than half spare time, but got: %a. Timeout: \
             %a, actual: %a, timer: %a"
            Mtime.Span.pp t Unixext.Timeout.pp timeout Mtime.Span.pp actual
            Unixext.Timer.pp timer
  in

  (* 3*half > timeout, so we expect Excess to be reported now *)
  Unix.sleepf (2. *. half) ;
  let actual = Mtime_clock.count elapsed in
  QCheck2.assume (Mtime.Span.compare actual timeout_span > 0) ;
  let () =
    match Unixext.Timer.remaining timer with
    | Excess _ ->
        ()
    | Spare t ->
        Test.fail_reportf
          "Expected to have excess time, but got spare: %a. Timeout: %a, \
           actual: %a, timer: %a"
          Mtime.Span.pp t Unixext.Timeout.pp timeout Mtime.Span.pp actual
          Unixext.Timer.pp timer
  in
  true

let tests =
  [
    test_timeout_string_conv
  ; test_timer_remaining
  ; test_proxy
  ; test_time_limited_write
  ; test_time_limited_read
  ]

let () =
  (* avoid SIGPIPE *)
  let (_ : Sys.signal_behavior) = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Xapi_stdext_unix.Unixext.test_open 1024 ;
  QCheck_base_runner.run_tests_main tests
