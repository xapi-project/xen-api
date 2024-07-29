open QCheck2
open Xapi_fd_test

let print_timeout = string_of_float

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

let test_buf_io =
  let timeouts = Generate.timeouts in
  let gen = Gen.tup2 Generate.t timeouts
  and print = Print.tup2 Generate.print print_timeout in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout) ->
  let every_bytes =
    Int.min
      (Option.map Observations.Delay.every_bytes behaviour.delay_read
      |> Option.value ~default:Int.max_int
      )
      (Option.map Observations.Delay.every_bytes behaviour.delay_write
      |> Option.value ~default:Int.max_int
      )
  in
  let operations = Int.max 1 (behaviour.size / every_bytes) in
  (* Buf_io uses per-operation timeouts, not a timeout for the whole function,
     so if we want a timeout of 0.1s and we insert some delays every 1 byte,
     for 64KiB bytes in total, then we need 0.1/65536 timeout for individual operations.

     timeout_span remains the span for the entire function,
     and timeout the per operation timeout that we'll pass to the function under test.
  *)
  let timeout_span = Mtime.Span.of_float_ns (timeout *. 1e9) |> Option.get in
  let timeout = timeout /. float operations in
  let timeout_operation_span =
    Mtime.Span.of_float_ns (timeout *. 1e9) |> Option.get
  in
  (* timeout < 1us would get truncated to 0 *)
  QCheck2.assume (timeout > 1e-6) ;
  (* Format.eprintf "Testing %s@." (print (behaviour, timeout)); *)
  if behaviour.kind <> Unix.S_SOCK then
    QCheck2.assume_fail () ;
  (* we only support sockets for this function *)
  let test_elapsed = ref Mtime.Span.zero in
  let test wrapped_fd =
    let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
    let bio = Buf_io.of_fd fd in
    let dt = Mtime_clock.counter () in
    let finally () = test_elapsed := Mtime_clock.count dt in
    Fun.protect ~finally (fun () ->
        Buf_io.really_input_buf bio behaviour.size ~timeout
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
      Mtime.Span.(add (timeout_span :> Mtime.Span.t) @@ (500 * ms))
    in
    if Mtime.Span.compare elapsed timeout_extra > 0 then
      Test.fail_reportf
        "Function duration significantly exceeds timeout: %a > %.6f; %s"
        Mtime.Span.pp elapsed timeout
        (Fmt.to_to_string Fmt.(option pp) observations.Observations.write) ;
    (* Format.eprintf "Result: %a@." (Fmt.option Observations.pp) observations.write;*)
    match (observations, result) with
    | {write= Some write; _}, Ok actual ->
        expect_amount ~expected:(String.length actual) write ;
        expect_string ~expected:write.data ~actual
    | {write= Some _; _}, Error (`Exn_trap (Buf_io.Timeout, _)) ->
        let elapsed = !test_elapsed in
        if Mtime.Span.compare elapsed timeout_operation_span < 0 then
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

let () =
  (* avoid SIGPIPE *)
  let (_ : Sys.signal_behavior) = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  QCheck_base_runner.run_tests_main [test_buf_io]
