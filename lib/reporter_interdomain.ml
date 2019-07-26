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

let start_interdomain
    (module D : Debug.DEBUG)
    ~reporter
    ~uid
    ~backend_domid
    ~page_count
    ~protocol
    ~dss_f =
  let id = Rrd_page_writer.({
      backend_domid = backend_domid;
      shared_page_count = page_count;
    }) in
  let shared_page_refs, writer =
    Rrd_page_writer.create id (choose_protocol protocol)
  in
  let xs_state = Xs.get_xs_state () in
  Xs.transaction xs_state.Xs.client (fun xs ->
      Xs.write xs
        (Printf.sprintf "%s/%s/grantrefs" xs_state.Xs.root_path uid)
        (List.map string_of_int shared_page_refs |> String.concat ",");
      Xs.write xs
        (Printf.sprintf "%s/%s/protocol" xs_state.Xs.root_path uid)
        (Rrd_interface.string_of_protocol protocol);
      Xs.write xs
        (Printf.sprintf "%s/%s/ready" xs_state.Xs.root_path uid)
        "true");
  let report () =
    let payload = Rrd_protocol.({
        timestamp = Utils.now ();
        datasources = dss_f ();
      }) in
    writer.Rrd_writer_functor.write_payload payload;
    Thread.delay 5.0
  in
  let cleanup () =
    Xs.immediate xs_state.Xs.client (fun xs ->
        Xs.write xs
          (Printf.sprintf "%s/%s/shutdown" xs_state.Xs.root_path uid)
          "true");
    writer.Rrd_writer_functor.cleanup ()
  in
  loop (module D) ~reporter ~report ~cleanup
