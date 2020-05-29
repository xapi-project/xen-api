open Safe_resources

let noleak () =
  Unixfd.with_pipe () ~loc:__LOC__ @@ fun p1 p2 ->
  Safe.safe_release p1 ; Safe.safe_release p2

let gc_live_words s =
  (* this runs finalisers which may allocate again *)
  Gc.full_major () ;
  (* really clean up *)
  Gc.full_major () ;
  (* does not create more live words *)
  s := (Gc.stat ()).live_words

let memory_usage_growth f =
  let s0 = ref 0 and s1 = ref 0 in
  gc_live_words s0 ;
  f () ;
  gc_live_words s1 ;
  Logs.debug (fun m -> m "Memory usage (words): %d -> %d" !s0 !s1) ;
  !s1 - !s0

let test_unixfd_noleak () =
  (* this just ensures we didn't stash something into a global *)
  let delta = memory_usage_growth noleak in
  Alcotest.(check int "memory leak in words" 0 delta)

let count_fds () =
  let d = Unix.opendir "/proc/self/fd" in
  Fun.protect ~finally:(fun () -> Unix.closedir d) @@ fun () ->
  let count = ref 0 in
  try
    while true do
      let s : string = Unix.readdir d in
      prerr_endline s ; incr count
    done ;
    assert false
  with End_of_file -> !count

let run () =
  Unixfd.with_pipe ~loc:__LOC__ () (fun p1 p2 ->
      (* this is intentionally buggy here: it leaks.
       * GC finaliser should detect, warn and clean up *)
      Safe.move_exn p1 |> ignore ;
      Safe.move_exn p2 |> ignore)

let leak_detected (count0, count1, count2) =
  (* do not call alcotest inhere, it gives weird results with memory getting freed,
   * not allocated (flushing?) *)
  count0 := count_fds () ;
  run () ;
  count1 := count_fds () ;
  Gc.full_major () ;
  count2 := count_fds ()

let count0, count1, count2 = (ref 0, ref 0, ref 0)

let test_unixfd_leak_detected () =
  let delta =
    memory_usage_growth (fun () -> leak_detected (count0, count1, count2))
  in
  Alcotest.(check int "memory leak in words" 0 delta) ;
  Alcotest.(
    check int "count of open FDs after pipe 2 higher" (!count0 + 2) !count1) ;
  Alcotest.(check int "count of open FDs same as original" !count0 !count2)

let noleak2 () =
  let moved1, moved2 =
    count0 := count_fds () ;
    Unixfd.with_pipe ~loc:__LOC__ () (fun p1 p2 -> (Safe.move_exn p1, Safe.move_exn p2))
  in
  count1 := count_fds () ;
  Safe.within (Safe.move_exn moved1) @@ ignore;
  Safe.within (Safe.move_exn moved2) @@ ignore;
  count2 := count_fds ()

let test_unixfd_noleak2 () =
  (* this just ensures we didn't stash something into a global *)
  let delta = memory_usage_growth noleak2 in
  Alcotest.(
    check int "count of open FDs after pipe 2 higher" (!count0 + 2) !count1) ;
  Alcotest.(check int "count of open FDs same as original" !count0 !count2) ;
  Alcotest.(check int "memory leak in words" 0 delta)

let tests =
  [
    (__LOC__, `Quick, test_unixfd_noleak)
  ; (__LOC__, `Quick, test_unixfd_noleak2)
  ; (__LOC__, `Quick, test_unixfd_leak_detected)
  ]
