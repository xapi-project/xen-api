let ( let@ ) f x = f x

let ( let* ) = Lwt.bind

module Tpm = Xapi_guard.Types.Tpm

module TPMs = struct
  let writes_created = Atomic.make 1

  let reads_created = Atomic.make 1

  let request_persist uuid write =
    let __FUN = __FUNCTION__ in

    let key = Tpm.deserialize_key (Random.int 3) |> Result.get_ok in

    let time = Mtime_clock.now () in
    let serial_n = Atomic.fetch_and_add writes_created 1 in
    let contents =
      Printf.sprintf "contents %s" (Mtime.to_uint64_ns time |> Int64.to_string)
    in
    let* () =
      Logs_lwt.app (fun m ->
          m "%s: Write № %i requested: %a/%i/%a" __FUN serial_n Uuidm.pp uuid
            Tpm.(serialize_key key)
            Mtime.pp time
      )
    in
    write (uuid, time, key) contents

  let request_read uuid read =
    let __FUN = __FUNCTION__ in

    let key = Tpm.deserialize_key (Random.int 3) |> Result.get_ok in

    let time = Mtime_clock.now () in
    let serial_n = Atomic.fetch_and_add reads_created 1 in
    let* () =
      Logs_lwt.app (fun m ->
          m "%s: Read № %i requested: %a/%i/%a" __FUN serial_n Uuidm.pp uuid
            Tpm.(serialize_key key)
            Mtime.pp time
      )
    in
    let* () = Lwt_unix.sleep 0.05 in
    read (uuid, time, key)
end

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b ; m
    )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
            Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ ->
            Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over () ; Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result ;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  {Logs.report}

let setup_log level =
  Logs.set_level level ;
  Logs.set_reporter (lwt_reporter ()) ;
  ()

let ok = Lwt_result.ok

let retry_forever fname f =
  let rec loop () =
    let* () =
      Lwt.catch f (function exn ->
          let* () =
            Logs_lwt.app (fun m ->
                m "%s failed with %s, retrying..." fname (Printexc.to_string exn)
            )
          in
          Lwt_unix.sleep 0.5
          )
    in
    (loop [@tailcall]) ()
  in
  loop ()

let max_writes = 128

let max_reads = 500_000

let received_writes = ref 0

let received_reads = ref 0

let throttled_reads = Mtime.Span.(200 * ms)

let failing_writes_period = Mtime.Span.(500 * ms)

let epoch = Mtime_clock.now ()

let should_fail () : bool =
  let rec polarity elapsed =
    if Mtime.Span.compare elapsed failing_writes_period < 0 then
      true
    else
      not (polarity Mtime.Span.(abs_diff elapsed failing_writes_period))
  in
  let elapsed = Mtime.span epoch (Mtime_clock.now ()) in
  polarity elapsed

let log_write (uuid, timestamp, key) content =
  let __FUN = __FUNCTION__ in
  let ( let* ) = Lwt_result.bind in
  let maybe_fail () =
    if should_fail () then
      Lwt_result.fail
        (failwith (Printf.sprintf {|oops, could not write '%s'|} content))
    else
      Lwt_result.return ()
  in
  let* () = maybe_fail () in
  received_writes := !received_writes + 1 ;
  Logs_lwt.app (fun m ->
      m "%s Write № %i detected: %a/%i/%a" __FUN !received_writes Uuidm.pp uuid
        Tpm.(serialize_key key)
        Mtime.pp timestamp
  )
  |> ok

let log_read (uuid, timestamp, key) =
  let __FUN = __FUNCTION__ in
  let ( let* ) = Lwt_result.bind in
  received_reads := !received_reads + 1 ;
  let* () =
    Logs_lwt.app (fun m ->
        m "%s Read to source № %i detected: %a/%i/%a" __FUN !received_reads
          Uuidm.pp uuid
          Tpm.(serialize_key key)
          Mtime.pp timestamp
    )
    |> ok
  in
  Lwt_result.return "yes"

let to_cache with_read_writes =
  let __FUN = __FUNCTION__ in
  let elapsed = Mtime_clock.counter () in
  let persist uuid (_, write_tpm) = TPMs.request_persist uuid write_tpm in
  let read uuid (read_tpm, _) =
    let* contents = TPMs.request_read uuid read_tpm in
    Logs_lwt.app (fun m -> m "%s Read received: '%s'" __FUN contents)
  in
  let rec loop_and_stop f name uuid max sent =
    let sent = sent + 1 in
    let@ read_write = with_read_writes in
    let* () = f uuid read_write in
    if sent >= max then
      Logs_lwt.app (fun m ->
          m "%s: Stopping requests after %i %ss" __FUN sent name
      )
    else if Mtime.Span.compare (Mtime_clock.count elapsed) throttled_reads > 0
    then
      let* () = Lwt_unix.sleep 0.1 in
      loop_and_stop f name uuid max sent
    else
      let* () = Lwt.pause () in
      loop_and_stop f name uuid max sent
  in
  let vms = List.init 4 (fun _ -> Uuidm.(v `V4)) in

  List.concat
    [
      List.map (fun uuid -> loop_and_stop persist "write" uuid max_writes 0) vms
    ; List.map (fun uuid -> loop_and_stop read "read" uuid max_reads 0) vms
    ]

let from_cache with_watcher = retry_forever "watcher" with_watcher

let main () =
  let* with_read_writes, with_watcher =
    Xapi_guard.Disk_cache.(setup Swtpm log_read log_write)
  in
  let reader = from_cache with_watcher in
  let writers = to_cache with_read_writes in
  let* _ = Lwt.all (reader :: writers) in
  Lwt.return_unit

let () =
  Debug.log_to_stdout () ;
  setup_log @@ Some Logs.Debug ;
  Lwt_main.run (main ())
