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

let skip_blk_timed behaviour =
  let open Generate in
  (* select/poll on block device returns immediately,
     so we cannot apply any delays on the reads/writes:
     they won't be reflected on the other side yet
  *)
  QCheck2.assume
    (behaviour.kind <> Unix.S_BLK
    || Option.is_none behaviour.delay_write
       && Option.is_none behaviour.delay_read
    )

let test_time_limited_write =
  let gen = Gen.tup2 Generate.t Generate.timeouts
  and print = Print.tup2 Generate.print Print.float in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout) ->
  skip_blk_timed behaviour ;
  skip_dirlnk behaviour.kind ;
  try
    let test_elapsed = ref Mtime.Span.zero in
    let test wrapped_fd =
      let len = behaviour.size in
      let buf = String.init len (fun i -> Char.chr (i mod 255)) in
      let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
      Unix.set_nonblock fd ;
      let dt = Mtime_clock.counter () in
      let deadline = Unix.gettimeofday () +. timeout in
      let finally () = test_elapsed := Mtime_clock.count dt in
      Fun.protect ~finally (fun () ->
          Unixext.time_limited_write_substring fd len buf deadline
      ) ;
      buf
    in
    let observations, result = Generate.run_wo behaviour ~f:test in
    let () =
      let open Observations in
      let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
      if elapsed_s > timeout +. 0.5 then
        Test.fail_reportf
          "Function duration significantly exceeds timeout: %f > %f; %s"
          elapsed_s timeout
          (Fmt.to_to_string Fmt.(option pp) observations.Observations.read) ;
      match (observations, result) with
      | {read= Some read; _}, Ok expected ->
          (* expected is the input given to [time_limited_write_substring] *)
          expect_amount ~expected:(String.length expected) read ;
          expect_string ~expected ~actual:read.data
      | {read= Some read; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
          let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
          if elapsed_s < timeout then
            Test.fail_reportf "Timed out earlier than requested: %f < %f"
              elapsed_s timeout ;
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
  and print = Print.tup2 Generate.print Print.float in
  Test.make ~name:__FUNCTION__ ~print gen @@ fun (behaviour, timeout) ->
  skip_blk behaviour.kind ;
  skip_dirlnk behaviour.kind ;
  skip_blk_timed behaviour ;
  let test_elapsed = ref Mtime.Span.zero in
  let test wrapped_fd =
    let fd = Xapi_fdcaps.Operations.For_test.unsafe_fd_exn wrapped_fd in
    Unix.set_nonblock fd ;
    let dt = Mtime_clock.counter () in
    let deadline = Unix.gettimeofday () +. timeout in
    let finally () = test_elapsed := Mtime_clock.count dt in
    Fun.protect ~finally (fun () ->
        Unixext.time_limited_read fd behaviour.size deadline
    )
  in
  let observations, result =
    let buf = String.init behaviour.size (fun i -> Char.chr (i mod 255)) in
    Generate.run_ro behaviour buf ~f:test
  in
  let () =
    let open Observations in
    let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
    if elapsed_s > timeout +. 0.5 then
      Test.fail_reportf
        "Function duration significantly exceeds timeout: %f > %f; %s" elapsed_s
        timeout
        (Fmt.to_to_string Fmt.(option pp) observations.Observations.write) ;
    match (observations, result) with
    | {write= Some write; _}, Ok actual ->
        expect_amount ~expected:(String.length actual) write ;
        expect_string ~expected:write.data ~actual
    | {write= Some _; _}, Error (`Exn_trap (Unixext.Timeout, _)) ->
        let elapsed_s = Mtime.Span.to_float_ns !test_elapsed *. 1e-9 in
        if elapsed_s < timeout then
          Test.fail_reportf "Timed out earlier than requested: %f < %f"
            elapsed_s timeout
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

let run_select ro wo errs timeout =
  let dt = Mtime_clock.counter () in
  let r = Unixext.select ro wo errs timeout in
  (Mtime_clock.count dt, timeout, r)

(* delays as long as 28.4ms were observed with epoll
   (on an otherwise idle system with a single thread, and no Xen domains)
   Be very conservative here and allow for a large difference
*)
let extra_timeout = Mtime.Span.(250 * ms)

let check_timeout elapsed timeout =
  let timeout_span = Clock.Timer.s_to_span timeout |> Option.get in
  if
    Clock.Timer.span_is_longer elapsed
      ~than:(Mtime.Span.add Mtime.Span.(2 * timeout_span) extra_timeout)
  then
    Test.fail_reportf "Timed out too late: %a > %f" Mtime.Span.pp elapsed
      timeout ;
  timeout_span

module FDSet = Set.Make (struct
  type t = Unix.file_descr

  let compare = Stdlib.compare
end)

let check_set lst =
  let set = FDSet.of_list lst in
  let n = List.length lst and n' = FDSet.cardinal set in
  if n <> n' then
    Test.fail_reportf
      "File descriptor set contains duplicate elements: %d <> %d" n n' ;
  set

let check_sets (s1, s2, s3) = (check_set s1, check_set s2, check_set s3)

let pp_fd = Fmt.using Unixext.int_of_file_descr Fmt.int

let pp_fdset = Fmt.(using FDSet.to_seq @@ Dump.seq pp_fd)

let check_subset msg msg' s1 s1' (r, w, e) (r', w', e') =
  if not (FDSet.subset s1 s1') then
    Test.fail_reportf
      "%s %s: (%d and %d elements): %a and %a. output: %a,%a,%a; available: \
       %a,%a,%a"
      msg msg' (FDSet.cardinal s1) (FDSet.cardinal s1') pp_fdset s1 pp_fdset s1'
      pp_fdset r pp_fdset w pp_fdset e pp_fdset r' pp_fdset w' pp_fdset e'

let check_subsets msg ((s1, s2, s3) as all) ((s1', s2', s3') as all') =
  check_subset msg "read" s1 s1' all all' ;
  check_subset msg "write" s2 s2' all all' ;
  check_subset msg "error" s3 s3' all all'

let test_select =
  let gen, print = Generate.select_input in
  Test.make ~long_factor:10 ~name:__FUNCTION__ ~print gen @@ fun t ->
  (* epoll raised EEXIST, but none of the actual callers in XAPI need this,
     so skip
  *)
  QCheck2.assume (t.rw = [] && t.re = [] && t.we = []) ;
  let ((elapsed, timeout, ready), (elapsed', timeout', ready')), available =
    Observations.with_select_input t run_select
  in
  let timeout_span = check_timeout elapsed timeout in
  let (_ : Mtime.Span.t) = check_timeout elapsed' timeout' in
  let () =
    match ready with
    | [], [], [] ->
        if Clock.Timer.span_is_shorter elapsed ~than:timeout_span then
          Test.fail_reportf "Timed out too early: %a < %f" Mtime.Span.pp elapsed
            timeout
    | _ ->
        let ready = check_sets ready in
        let ready' = check_sets ready' in
        let available = check_set available in
        let available = (available, available, available) in
        check_subsets "1st call subset of 2nd" ready ready' ;
        check_subsets "ready subset of available" ready available ;
        check_subsets "ready' subset of available" ready' available ;
        ()
  in
  true

let tests =
  [test_select; test_proxy; test_time_limited_write; test_time_limited_read]

let () =
  (* avoid SIGPIPE *)
  let (_ : Sys.signal_behavior) = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  (* TODO: reenable once the epoll branch is merged Xapi_stdext_unix.Unixext.test_open 1024 *)
  ()
