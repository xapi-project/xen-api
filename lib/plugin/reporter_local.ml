(*
 * Copyright (C) 2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xapi_stdext_threads.Threadext
open Xapi_stdext_unix
open Reporter

let start_local (module D : Debug.DEBUG)
    ~reporter
    ~uid
    ~neg_shift
    ~page_count
    ~protocol
    ~dss_f =
  try
    let path = RRDD.Plugin.get_path uid in
    D.info "Obtained path=%s\n" path;
    let _ = Unixext.mkdir_safe (Filename.dirname path) 0o644 in
    let id = Rrd_file_writer.({
        path;
        shared_page_count = page_count;
      }) in
    let _, writer =
      Rrd_file_writer.create id (choose_protocol protocol)
    in
    let overdue_count = ref 0 in
    let report () =
      overdue_count := wait_until_next_reading
          (module D)
          ~neg_shift
          ~uid
          ~protocol
          ~overdue_count:!overdue_count;
      let payload = Rrd_protocol.({
          timestamp = Utils.now ();
          datasources = dss_f ();
        }) in
      writer.Rrd_writer_functor.write_payload payload;
      Thread.delay 0.003
    in
    let cleanup () =
      RRDD.Plugin.Local.deregister uid;
      writer.Rrd_writer_functor.cleanup ()
    in
    loop (module D : Debug.DEBUG) ~reporter ~report ~cleanup
  with exn ->
  match reporter with
  | Some reporter ->
    Mutex.execute reporter.lock (fun () ->
        reporter.state <- Stopped (`Failed exn))
  | None -> raise exn
