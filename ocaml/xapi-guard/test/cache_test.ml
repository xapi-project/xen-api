let ( let@ ) f x = f x

let ( let* ) = Lwt.bind

module Tpm = Xapi_guard.Types.Tpm

module TPMs = struct
  let tpms_created = Atomic.make 1

  let request_persist uuid write =
    let __FUN = __FUNCTION__ in

    let key = Tpm.deserialize_key (Random.int 3) in

    let time = Mtime_clock.now () in
    let serial_n = Atomic.fetch_and_add tpms_created 1 in
    let contents =
      Printf.sprintf "contents %s" (Mtime.to_uint64_ns time |> Int64.to_string)
    in
    let* () =
      Logs_lwt.app (fun m ->
          m "%s: Content № %i created: %a/%i/%a" __FUN serial_n Uuidm.pp uuid
            Tpm.(serialize_key key)
            Mtime.pp time
      )
    in
    write (uuid, time, key) contents
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

let max_sent = 128

let received = ref 0

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

let log (uuid, timestamp, key) content : (unit, exn) Result.t Lwt.t =
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
  received := !received + 1 ;
  Logs_lwt.app (fun m ->
      m "%s Content № %i detected: %a/%i/%a" __FUN !received Uuidm.pp uuid
        Tpm.(serialize_key key)
        Mtime.pp timestamp
  )
  |> ok

let to_cache with_writer =
  let __FUN = __FUNCTION__ in
  let elapsed = Mtime_clock.counter () in
  let rec loop_and_stop uuid sent () =
    let sent = sent + 1 in

    let@ write_tpm = with_writer in
    let* () = TPMs.request_persist uuid write_tpm in
    if sent >= max_sent then
      Logs_lwt.app (fun m ->
          m "%s: Stopping requests after %i writes" __FUN sent
      )
    else if Mtime.Span.compare (Mtime_clock.count elapsed) throttled_reads > 0
    then
      let* () = Lwt_unix.sleep 0.1 in
      loop_and_stop uuid sent ()
    else
      let* () = Lwt.pause () in
      loop_and_stop uuid sent ()
  in
  List.init 4 (fun _ -> Uuidm.(v `V4))
  |> List.map (fun uuid -> loop_and_stop uuid 0 ())

let from_cache with_watcher = retry_forever "watcher" with_watcher

let main () =
  let* with_writer, with_watcher = Xapi_guard.Disk_cache.(setup Swtpm log) in
  let reader = from_cache with_watcher in
  let writers = to_cache with_writer in
  let* _ = Lwt.all (reader :: writers) in
  Lwt.return_unit

let () =
  setup_log @@ Some Logs.Debug ;
  Lwt_main.run (main ())
