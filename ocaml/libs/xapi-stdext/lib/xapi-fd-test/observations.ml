open Xapi_fdcaps
open Properties
open Operations
open Syntax

let open_ro name = openfile_ro `reg name []

let open_wo name = openfile_wo `reg name []

let open_rw name = openfile_rw `reg name []

let with_kind_ro kind f =
  let with2 t =
    let@ fd1, fd2 = with_fd2 t in
    f fd1 (Some fd2)
  in
  match kind with
  | Unix.S_SOCK ->
      let@ fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      f (as_readonly_socket fd1) (Some fd2)
  | Unix.S_REG ->
      let@ name, out = with_tempfile () in
      let@ fd = with_fd @@ open_ro name in
      f fd (Some out)
  | Unix.S_FIFO ->
      with2 (pipe ())
  | Unix.S_DIR ->
      invalid_arg
        "S_DIR" (* not supported, OCaml has separate dir_handle type *)
  | Unix.S_LNK ->
      invalid_arg "S_LNK" (* O_NOFOLLOW not bound in OCaml *)
  | Unix.S_BLK ->
      let@ name, out = with_tempfile ~size:512L () in
      let@ blkname, _ = with_temp_blk name in
      let@ fd = with_fd @@ open_ro blkname in
      f fd (Some out)
  | Unix.S_CHR ->
      let@ fd = with_fd @@ dev_zero () in
      f fd None

let with_kind_wo kind f =
  let with2 t =
    let@ fd1, fd2 = with_fd2 t in
    f fd2 (Some fd1)
  in
  match kind with
  | Unix.S_REG ->
      let@ name, _out = with_tempfile () in
      let@ fd = with_fd @@ open_wo name in
      let@ fd_ro = with_fd @@ open_ro name in
      f fd (Some fd_ro)
  | Unix.S_FIFO ->
      with2 @@ pipe ()
  | Unix.S_SOCK ->
      let@ fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      f (as_writeonly_socket fd2) (Some fd1)
  | Unix.S_DIR ->
      invalid_arg
        "S_DIR" (* not supported, OCaml has separate dir_handle type *)
  | Unix.S_LNK ->
      invalid_arg "S_LNK" (* O_NOFOLLOW not bound in OCaml *)
  | Unix.S_BLK ->
      let@ name, _out = with_tempfile ~size:512L () in
      let@ blkname, _ = with_temp_blk name in
      let@ fd_ro = with_fd @@ open_ro blkname in
      let@ fd = with_fd @@ open_wo blkname in
      f fd (Some fd_ro)
  | Unix.S_CHR ->
      let@ fd = with_fd @@ dev_null_out () in
      f fd None

let with_kind_rw kind f =
  match kind with
  | Unix.S_SOCK ->
      let@ fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      f fd1 fd2
  | Unix.S_REG ->
      let@ name, _out = with_tempfile () in
      let@ fd = with_fd @@ open_rw name in
      let@ fd' = with_fd @@ open_rw name in
      f fd fd'
  | Unix.S_FIFO | Unix.S_DIR | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR ->
      invalid_arg "with_kind_rw: not a socket or reg"

let observe_read observed op t dest off len =
  let amount = op t dest off len in
  assert (amount >= 0) ;
  Buffer.add_subbytes observed dest off amount ;
  amount

let observe_write observed op t source off len =
  let amount = op t source off len in
  assert (amount >= 0) ;
  Buffer.add_substring observed source off amount ;
  amount

type 'a or_exn = ('a, Rresult.R.exn_trap) result

let unwrap_exn = function
  | Ok ok ->
      ok
  | Error (`Exn_trap (e, bt)) ->
      Printexc.raise_with_backtrace e bt

let concurrently (f, g) (farg, garg) =
  (* only one thread at a time reads or writes, atomic not needed *)
  let thread_result = ref None in
  let thread_fun (tfun, arg) =
    thread_result := Some (Rresult.R.trap_exn tfun arg)
  in
  let t = Thread.create thread_fun (g, garg) in
  let res = Rresult.R.trap_exn f farg in
  Thread.join t ;
  let thread_result =
    match !thread_result with
    | Some r ->
        r
    | None ->
        Rresult.R.trap_exn failwith "Thread not run?"
  in
  (res, thread_result)

type 'a observation = {
    elapsed: Mtime.span
  ; data: string
  ; is_read: [< rdonly | wronly] as 'a
}

let truncated_string ppf s =
  let n = 35 in
  if String.length s < 2 * n then
    Fmt.string ppf s
  else
    Fmt.pf ppf "%S...%S" (String.sub s 0 n)
      (String.sub s (String.length s - n) n)

let pp ppf =
  Fmt.(
    record ~sep:(any ";")
      [
        field "elapsed" (fun t -> t.elapsed) Mtime.Span.pp
      ; field "data" (fun t -> t.data) truncated_string
      ]
  )
    ppf

type ('a, 'b) observations = {read: 'a; write: 'b; elapsed: Mtime.span}

module CancellableSleep = struct
  type nonrec t = {
      wait: (rdonly, sock) make
    ; wake: (wronly, sock) make
    ; buf: bytes
  }

  let with_ f =
    let@ wait, wake = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    f
      {
        wait= as_readonly_socket wait
      ; wake= as_writeonly_socket wake
      ; buf= Bytes.make 1 ' '
      }

  let set_rcvtimeo sock timeo =
    if timeo < 1e-6 then Fmt.invalid_arg "timeout too short: %g" timeo ;
    setsockopt_float sock Unix.SO_RCVTIMEO timeo

  let sleep t dt =
    set_rcvtimeo t.wait (Mtime.Span.to_float_ns dt *. 1e-9) ;
    try
      let (_ : int) = read t.wait t.buf 0 1 in
      ()
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> ()

  let cancel t = shutdown_send t.wake
end

module Delay = struct
  type t = {duration: Mtime.span; every_bytes: int}

  let every_bytes t = t.every_bytes

  let pp =
    Fmt.(
      record ~sep:(any ";")
        [
          field "duration" (fun t -> t.duration) Mtime.Span.pp
        ; field "every_bytes" (fun t -> t.every_bytes) int
        ]
    )

  let v ~duration ~every_bytes = {duration; every_bytes}

  let apply repeat cancel t op =
    let remaining = ref t.every_bytes in
    let sleep () =
      CancellableSleep.sleep cancel t.duration ;
      remaining := t.every_bytes
    in
    let delayed_op fd buf off len =
      (* ensure we'll be able to insert our sleep, limit [len] if needed *)
      let n = op fd buf off (Int.min !remaining len) in
      remaining := !remaining - n ;
      if !remaining <= 0 then sleep () ;
      n
    in
    repeat delayed_op

  let apply_read cancel t op = apply repeat_read cancel t op

  let apply_write cancel t op = apply repeat_write cancel t op
end

let do_op buf is_read repeat observe op arg off length fd =
  fd
  |> Option.map @@ fun rd ->
     let dt = Mtime_clock.counter () in
     let (_ : int) = repeat (observe buf op) rd arg off length in
     let elapsed = Mtime_clock.count dt in
     let data = Buffer.contents buf in
     {is_read; data; elapsed}

let do_read read rd_buf ~size =
  let length = size in
  do_op rd_buf `rdonly repeat_read observe_read read (Bytes.make length 'x') 0
    length

let do_write write buf expected off =
  do_op buf `wronly repeat_write observe_write write expected off
    (String.length expected - off)

let wrap_measure f arg =
  let dt = Mtime_clock.counter () in
  let r = Rresult.R.trap_exn f arg in
  let result = (Mtime_clock.count dt, r) in
  close arg ; result

let observe_ro write ~f kind expected =
  with_kind_ro kind @@ fun ro wo_opt ->
  let written = Buffer.create 0 in
  let prepare fd_opt =
    let () =
      fd_opt
      |> Option.iter @@ fun fd ->
         as_spipe_opt fd |> Option.iter set_nonblock ;
         let (_ : int) =
           repeat_write
             (observe_write written write)
             fd expected 0 (String.length expected)
         in
         clear_nonblock fd
    in
    Buffer.length written
  in
  (* write as much as possible initially, TODO: should be configurable? *)
  let off = prepare wo_opt in
  let g fd_opt =
    fd_opt
    |> Option.fold ~none:None ~some:(fun fd ->
           let r = do_write write written expected off (as_writable_opt fd) in
           close fd ; r
       )
  in
  let res, thread_result = concurrently (wrap_measure f, g) (ro, wo_opt) in
  let elapsed, res = unwrap_exn res in
  let write = unwrap_exn thread_result in
  let write =
    write
    |> Option.map @@ fun write -> {write with data= Buffer.contents written}
  in
  ({read= (); write; elapsed}, res)

let observe_wo read ~f ~size kind =
  with_kind_wo kind @@ fun wo ro_opt ->
  let rd_buf = Buffer.create 0 in
  (* TODO:set block device size *)
  let g fd_opt =
    fd_opt
    |> Option.fold ~none:None ~some:(fun fd ->
           do_read ~size read rd_buf (as_readable_opt fd)
       )
  in
  let res, thread_result = concurrently (wrap_measure f, g) (wo, ro_opt) in
  let elapsed, res = unwrap_exn res in
  let read = unwrap_exn thread_result in
  let (_ : _ option) = g ro_opt in
  let read =
    read |> Option.map @@ fun read -> {read with data= Buffer.contents rd_buf}
  in
  ({write= (); read; elapsed}, res)

let observe_rw read write ~f ~size kind expected =
  with_kind_rw kind @@ fun rw1 rw2 ->
  let written = Buffer.create 0 in
  let rd_buf = Buffer.create 0 in
  let gw fd = do_write write written expected 0 (as_writable_opt fd)
  and gr fd = do_read ~size read rd_buf (as_readable_opt fd) in
  let g fd =
    let r = concurrently (gr, gw) (fd, fd) in
    close fd ; r
  in
  let res, thread_result = concurrently (wrap_measure f, g) (rw1, rw2) in
  let elapsed, res = unwrap_exn res in
  let read, write = unwrap_exn thread_result in
  let read =
    read
    |> unwrap_exn
    |> Option.map @@ fun read -> {read with data= Buffer.contents rd_buf}
  and write =
    write
    |> unwrap_exn
    |> Option.map @@ fun write -> {write with data= Buffer.contents written}
  in
  ({read; write; elapsed}, res)

let rec with_kind_list create aux f = function
  | [] ->
      f (List.rev aux)
  | kind :: tl ->
      create kind @@ fun fd1 fd2 ->
      with_kind_list create ((fd1, fd2) :: aux) f tl

let with_kind_list g lst f = with_kind_list g [] f lst

let with_kinds_ro lst = with_kind_list with_kind_ro lst

let with_kinds_wo lst = with_kind_list with_kind_wo lst

(* compatible with [with_kind_ro] and [with_kind_wo] *)
let with_kind_rw' kind f = with_kind_rw kind @@ fun fd1 fd2 -> f fd1 (Some fd2)

let with_kinds_rw lst = with_kind_list with_kind_rw' lst

type fd_set = Unix.file_descr list

type select_fd_spec = {kind: Unix.file_kind; wait: float}

type select_input = {
    ro: select_fd_spec list
  ; wo: select_fd_spec list
  ; rw: select_fd_spec list
  ; re: select_fd_spec list
  ; we: select_fd_spec list
  ; errors: select_fd_spec list
  ; timeout: float
}

let split_combine gen lst f =
  let fds, waits =
    lst |> List.map (fun {kind; wait} -> (kind, wait)) |> List.split
  in
  gen fds @@ fun fds ->
  let fds1, fds2 = List.split fds in
  f (fds1, List.combine fds2 waits)

let ( let@ ) f x = f x

type 'a fd_safe_set = ('a, kind) make list

let with_fd_inputs f (ro : rdonly fd_safe_set) (wo : wronly fd_safe_set)
    (rw : rdwr fd_safe_set) (re : rdonly fd_safe_set) (we : wronly fd_safe_set)
    (errs : rdwr fd_safe_set) timeout =
  let ro = List.map For_test.unsafe_fd_exn ro
  and wo = List.map For_test.unsafe_fd_exn wo
  and re = List.map For_test.unsafe_fd_exn re
  and we = List.map For_test.unsafe_fd_exn we
  and rw = List.map For_test.unsafe_fd_exn rw
  and errs = List.map For_test.unsafe_fd_exn errs in
  let call timeout = f (ro @ rw @ re) (wo @ rw @ we) (errs @ re @ we) timeout in
  let r1 = call timeout in
  let r2 = call 0. in
  (r1, r2)

let simulate f lst lst' =
  List.combine lst lst'
  |> List.map @@ fun (wrapped, (wrapped', wait)) ->
     (wait, For_test.unsafe_fd_exn wrapped, f wrapped wrapped')

let large = String.make 1_000_000 'x'

let buf = Bytes.make (String.length large) 'x'

let simulate_ro _ro ro' () =
  ro'
  |> Option.iter @@ fun ro' ->
     as_spipe_opt ro' |> Option.iter set_nonblock ;
     let (_ : int) = Operations.single_write_substring ro' "." 0 1 in
     ()

let simulate_wo wo wo' =
  let handle_pipe fd =
    set_nonblock fd ;
    (* fill buffers, to make write unavailable initially, but not on regular files/block devices,
       to avoid ENOSPC errors
    *)
    let (_ : int) =
      Operations.repeat_write Operations.single_write_substring fd large 0
        (String.length large)
    in
    ()
  in
  as_spipe_opt wo |> Option.iter handle_pipe ;
  fun () ->
    wo'
    |> Option.iter @@ fun wo' ->
       as_spipe_opt wo' |> Option.iter set_nonblock ;
       let (_ : int) = Operations.read wo' buf 0 (Bytes.length buf) in
       ()

let simulate_rw rw rw' =
  let f = simulate_ro rw rw' and g = simulate_wo rw rw' in
  fun () -> f () ; g ()

let compare_wait (t1, _, _) (t2, _, _) = Float.compare t1 t2

let run_simulation (stop, actions) =
  (* TODO: measure when we actually sent, also have an atomic to when to stop exactly *)
  List.fold_left
    (fun (prev, fds) (curr, fd, action) ->
      let delta = curr -. prev in
      assert (delta >= 0.) ;
      if not (Atomic.get stop) then
        if delta > 0. then
          Unix.sleepf delta ;
      (* check again, might've been set meanwhile *)
      ( curr
      , if (not (Atomic.get stop)) || curr < Float.epsilon then (
          action () ; fd :: fds
        ) else
          fds
      )
    )
    (0., []) actions
  |> snd

let with_select_input t f =
  let@ re, re' = split_combine with_kinds_ro t.re in
  let@ we, we' = split_combine with_kinds_wo t.we in
  let@ rw, rw' = split_combine with_kinds_rw t.rw in
  let@ ro, ro' = split_combine with_kinds_ro t.ro in
  let@ wo, wo' = split_combine with_kinds_wo t.wo in
  let@ errs, errs' = split_combine with_kinds_rw t.errors in
  let actions =
    List.concat
      [
        simulate simulate_ro (ro @ re) (ro' @ re')
      ; simulate simulate_wo (wo @ we) (wo' @ we')
      ; simulate simulate_rw rw rw' (* TODO: how to simulate errors *)
      ; simulate simulate_rw errs errs'
      ]
    |> List.fast_sort compare_wait
  in
  let stop = Atomic.make false in
  let finally () = Atomic.set stop true in
  let run () = with_fd_inputs f ro wo rw re we errs t.timeout in
  let run () = Fun.protect ~finally run in
  let r1, r2 = concurrently (run, run_simulation) ((), (stop, actions)) in
  (unwrap_exn r1, unwrap_exn r2)
