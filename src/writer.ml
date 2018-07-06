open Core
open Import

include Writer0

let of_pipe info pipe_w =
  let%map `Reader reader_fd, `Writer writer_fd = Unix.pipe info in
  let reader = Reader.create reader_fd in
  let writer = create writer_fd in
  if Debug.writer
  then (
    Debug.log "Writer.of_pipe" (pipe_w, reader, writer)
      [%sexp_of: string Pipe.Writer.t * Reader.t * t]);
  (* Shuttle bytes from [reader] to [pipe_w].  If the user calls [close writer],
     then [reader] will see EOF, which will cause [transfer] to complete.  If [pipe_w]
     is closed, then [transfer] will complete. *)
  let closed_and_flushed_downstream =
    let%bind () = Reader.transfer reader pipe_w in
    if raise_when_consumer_leaves writer && not (is_closed writer)
    then (
      Monitor.send_exn (monitor writer)
        (Unix.Unix_error (EPIPE, "Writer.of_pipe", "")));
    let%map (), () = Deferred.both (Reader.close reader) (close writer) in
    if not (Pipe.is_closed pipe_w) then (Pipe.close pipe_w)
  in
  writer, `Closed_and_flushed_downstream closed_and_flushed_downstream
;;

let%test_module _ =
  (module struct

    let test_async f =
      Thread_safe.block_on_async_exn (fun () ->
        match%map Clock.with_timeout (sec 1.) (f ()) with
        | `Result x -> x
        | `Timeout -> raise_s [%message "Timeout."])
    ;;

    let count_errors w =
      let ivar = ref (Ivar.create ()) in
      let module M = struct exception Return end in
      let count = ref 0 in
      Monitor.detach_and_iter_errors (monitor w) ~f:(fun exn ->
        match Monitor.extract_exn exn with
        | M.Return ->
          Ivar.fill !ivar !count;
          count := 0;
          ivar := Ivar.create ()
        | _ -> incr count);
      Staged.stage (fun () ->
        Monitor.send_exn (monitor w) M.Return;
        Ivar.read !ivar)
    ;;

    let%test_unit "check_buffer_age raises once after EPIPE" = test_async (fun () ->
      let fd_r, fd_w = Core.Unix.pipe () in
      let w =
        create (Fd.create Fifo fd_w (Info.of_string "test fd"))
          ~raise_when_consumer_leaves:false
          ~buffer_age_limit:(`At_most (sec 10.))
          ~syscall:`Per_cycle
      in
      let count = Staged.unstage (count_errors w) in
      Core.Unix.close fd_r;
      write w "Hello, world!";
      let check ~now = Check_buffer_age.internal_check_now_for_unit_test ~now in
      check ~now:(Time_ns.now ());
      let%bind () = consumer_left w in
      let now = Time_ns.now () in
      let check ~after = check ~now:(Time_ns.add now (Time_ns.Span.of_int_sec after)) in
      check ~after:20;
      check ~after:30;
      check ~after:40;
      let%bind n = count () in
      [%test_result: int] ~expect:1 n;
      close w)
    ;;

    let%test_unit "check_buffer_age is edge triggered" = test_async (fun () ->
      let fd = Core.Unix.openfile ~mode:[O_WRONLY] "/dev/null" in
      let w =
        create (Fd.create Char fd (Info.of_string "test fd"))
          ~raise_when_consumer_leaves:false
          ~buffer_age_limit:(`At_most (sec 10.))
          ~syscall:`Per_cycle
      in
      let count = Staged.unstage (count_errors w) in
      let now = Time_ns.now () in
      let check ~after =
        Check_buffer_age.internal_check_now_for_unit_test
          ~now:(Time_ns.add now (Time_ns.Span.of_int_sec after))
      in
      check ~after:0;
      w.bytes_received <- Int63.of_int 1;
      check ~after:1;
      check ~after:12;
      check ~after:13;
      check ~after:14;
      let%bind n = count () in
      [%test_result: int] ~expect:1 n;
      w.bytes_received <- Int63.of_int 2;
      check ~after:15;
      check ~after:25;
      let%bind n = count () in
      [%test_result: int] ~expect:0 n;
      w.bytes_written <- Int63.of_int 2;
      check ~after:26;
      let%bind n = count () in
      [%test_result: int] ~expect:0 n;
      w.bytes_received <- Int63.of_int 3;
      check ~after:27;
      check ~after:38;
      let%bind n = count () in
      [%test_result: int] ~expect:1 n;
      close w)
    ;;
  end)
