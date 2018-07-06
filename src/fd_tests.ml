open! Core
open! Async_kernel
open! Import

module Unix = Core.Unix

open Fd

let%test_module "Fd" =
  (module struct
    let wait_until_cell_is_equal_to cell expected =
      let rec loop tries =
        if tries = 0
        then (return false)
        else if String.equal !cell expected
        then (return true)
        else (
          let%bind () = Clock.after (sec 0.1) in
          loop (tries - 1))
      in
      loop 100;
    ;;

    let read_into_cell (fd, cell) =
      Unix.set_nonblock fd;
      try
        while true do
          let buf = Bytes.create 1024 in
          let n = Unix.read fd ~buf ~pos:0 ~len:(Bytes.length buf) in
          assert (n > 0);
          cell := !cell ^ Bytes.To_string.sub buf ~pos:0 ~len:n
        done
      with Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> ()
    ;;

    let block_with_timeout f =
      Thread_safe.block_on_async_exn (fun () -> Clock.with_timeout (sec 5.) (f ()))
    ;;

    let%test_unit _ =
      let read = ref "" in
      block_with_timeout (fun () ->
        let fdr, fdw = Unix.pipe () in
        let fdr_async = create Fifo fdr (Info.of_string "<pipe>") in
        let d = every_ready_to fdr_async `Read read_into_cell (fdr, read) in
        assert (String.equal !read "");
        assert (Unix.write_substring fdw ~buf:"foo" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foo" in
        assert b;
        assert (Unix.write_substring fdw ~buf:"bar" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foobar" in
        assert b;
        let%bind () = close fdr_async in
        d)
      |> function
      | `Result `Closed -> assert (String.equal !read "foobar")
      | `Result _ -> assert false
      | `Timeout -> assert false
    ;;

    let%test_unit _ =
      let read = ref "" in
      block_with_timeout (fun () ->
        let fdr, fdw = Unix.pipe () in
        let fdr_async = create Kind.Fifo fdr (Info.of_string "<pipe>") in
        let stop = Ivar.create () in
        let d =
          interruptible_every_ready_to fdr_async `Read read_into_cell (fdr, read)
            ~interrupt:(Ivar.read stop)
        in
        assert (String.equal !read "");
        assert (Unix.write_substring fdw ~buf:"foo" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foo" in
        assert b;
        assert (Unix.write_substring fdw ~buf:"bar" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foobar" in
        assert b;
        Ivar.fill stop ();
        assert (Unix.write_substring fdw ~buf:"extra" ~pos:0 ~len:5 = 5);
        d)
      |> function
      | `Result `Interrupted -> assert (String.equal !read "foobar")
      | `Result _ -> assert false
      | `Timeout -> assert false
    ;;

    let%test_unit _ =
      let read = ref "" in
      block_with_timeout (fun () ->
        let fdr, fdw = Unix.pipe () in
        let fdr_async = create Fifo fdr (Info.of_string "<pipe>") in
        let stop = Ivar.create () in
        let d =
          interruptible_every_ready_to fdr_async `Read read_into_cell (fdr, read)
            ~interrupt:(Ivar.read stop)
        in
        assert (String.equal !read "");
        assert (Unix.write_substring fdw ~buf:"foo" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foo" in
        assert b;
        assert (Unix.write_substring fdw ~buf:"bar" ~pos:0 ~len:3 = 3);
        let%bind b = wait_until_cell_is_equal_to read "foobar" in
        assert b;
        let%bind () = close fdr_async in
        d)
      |> function
      | `Result `Closed -> assert (String.equal !read "foobar")
      | `Result _ -> assert false
      | `Timeout -> assert false
    ;;

    let%test_unit
      "ready_to reports Ready for writer when reader closes after partial read"
      =
      block_with_timeout (fun () ->
        let r, w =
          let r, w = Core.Unix.pipe () in
          create Fifo r (Info.of_string "<reader>"),
          create Fifo w (Info.of_string "<writer>")
        in
        let read =
          match%bind ready_to r `Read with
          | `Bad_fd -> assert false
          | `Closed -> assert false
          | `Ready  ->
            assert (supports_nonblock r);
            let len = 1 lsl 17 in
            let buf = Bigstring.create len in
            match
              syscall r ~nonblocking:true (fun fd ->
                let res =
                  Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
                    (Bigstring.read_assume_fd_is_nonblocking fd buf ~pos:0 ~len)
                in
                assert (res >= 0 && res <= len))
            with
            | `Already_closed -> assert false
            | `Error exn      -> raise_s [%message "read error" (exn : exn)]
            | `Ok ()          -> close r
        in
        let len = 1_000_000 in
        let iovec = Unix.IOVec.of_bigstring (Bigstring.create len) in
        let write =
          match%bind
            syscall_in_thread w ~name:"writev" (fun fd ->
              Bigstring.writev fd [| iovec |])
          with
          | `Already_closed -> assert false
          | `Error exn      -> raise_s [%message "write error" (exn : exn)]
          | `Ok n           ->
            assert (n >= 0 && n <= len);
            match%map ready_to w `Write with
            | `Bad_fd -> assert false
            | `Closed -> assert false
            | `Ready  -> ()
        in
        let%bind () = read in
        let%bind () = write in
        match%map
          syscall_in_thread w ~name:"writev" (fun fd -> Bigstring.writev fd [| iovec |])
        with
        | `Error (Unix.Unix_error (EPIPE, "writev", "")) -> ()
        | `Error exn      -> raise_s [%message "bad write error" (exn : exn)]
        | `Ok n           -> raise_s [%message "no write error" (n : int)]
        | `Already_closed -> assert false)
      |> function
      | `Result () -> ()
      | `Timeout   -> assert false
    ;;
  end)
;;
