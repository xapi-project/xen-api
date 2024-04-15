open Xapi_fdcaps
open Operations
open Xapi_fd_test.Observations
open Syntax

let skip_blk = function
  | Unix.S_BLK ->
      if Unix.geteuid () <> 0 then
        Alcotest.skip ()
  | _ ->
      ()

let expected = "string to be written"

(*
let prepare fd_opt =
  let buf = Buffer.create 0 in
  let () =
    fd_opt
    |> Option.iter @@ fun fd ->
       let (_ : int) =
         observe_write buf single_write_substring fd expected 0
           (String.length expected)
       in
       ()
  in
  buf
*)

let test_kind_ro kind () =
  skip_blk kind ;
  let f fd =
    let b = Bytes.make 128 'x' in
    let n = read fd b 0 (Bytes.length b) in
    close fd ; Bytes.sub_string b 0 n
  in
  let observed, res = observe_ro single_write_substring kind expected ~f in
  let actual = unwrap_exn res in
  match observed.write with
  | Some observed_write ->
      Alcotest.(check' string)
        ~msg:"expected string received" ~expected:observed_write.data ~actual
  | None ->
      ()

let test_kind_wo kind () =
  skip_blk kind ;
  let f fd =
    let n = single_write_substring fd expected 0 (String.length expected) in
    close fd ; String.sub expected 0 n
  in
  let observed, res = observe_wo read kind ~f ~size:128 in
  let actual = unwrap_exn res in
  match observed.read with
  | Some observed_read ->
      Alcotest.(check' string)
        ~msg:"expected string received" ~expected:observed_read.data ~actual
  | None ->
      ()

let kinds = Unix.[S_BLK; S_CHR; S_FIFO; S_REG; S_SOCK]

let test_kind_all test =
  kinds
  |> List.map @@ fun kind ->
     Alcotest.test_case (Fmt.to_to_string Safefd.pp_kind kind) `Quick (test kind)

let test_cancellable_sleep () =
  let@ t = CancellableSleep.with_ in
  let sleep_duration = Mtime.Span.(2 * s) in
  let sleeper () =
    let dt = Mtime_clock.counter () in
    let () = CancellableSleep.sleep t sleep_duration in
    Mtime_clock.count dt
  in
  let waker_duration = 0.1 in
  let waker () = Unix.sleepf waker_duration ; CancellableSleep.cancel t in
  let slept, _ = concurrently (sleeper, waker) ((), ()) in
  let slept = unwrap_exn slept in
  if Mtime.Span.compare slept sleep_duration >= 0 then
    Alcotest.failf
      "Sleep wasn't interrupted as expected, total duration = %a; waked at = \
       %fs"
      Mtime.Span.pp slept waker_duration ;
  if Mtime.Span.to_float_ns slept *. 1e-9 < waker_duration then
    Alcotest.failf "Sleep was shorter than expected, total duration = %a < %fs"
      Mtime.Span.pp slept waker_duration

let test_full_sleep () =
  let@ t = CancellableSleep.with_ in
  let sleep_duration = Mtime.Span.(10 * ms) in
  let slept =
    let dt = Mtime_clock.counter () in
    let () = CancellableSleep.sleep t sleep_duration in
    Mtime_clock.count dt
  in
  if Mtime.Span.compare slept sleep_duration < 0 then
    Alcotest.failf "Sleep was shorter than expected, total duration = %a < %a"
      Mtime.Span.pp slept Mtime.Span.pp sleep_duration

let () =
  setup () ;
  (* kill test after 5s, it must've gotten stuck.. *)
  (*  let (_: int) = Unix.alarm 5 in *)
  Alcotest.run ~show_errors:true "xapi_fdcaps"
    [
      ("test_kind_ro", test_kind_all test_kind_ro)
    ; ("test_kind_wo", test_kind_all test_kind_wo)
    ; ( "cancellable sleep"
      , [
          Alcotest.test_case "cancellable" `Quick test_cancellable_sleep
        ; Alcotest.test_case "full" `Quick test_full_sleep
        ]
      )
    ]
