(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(*
 * RRD Unix module
 * This module provides Unix tools for dealing with RRDs
 *)
(**
 * @group Performance Monitoring
*)

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let with_out_channel_output fd f =
  let oc = Unix.(out_channel_of_descr (dup fd)) in
  finally
    (fun () ->
      let output = Xmlm.make_output (`Channel oc) in
      f output
    )
    (fun () -> Out_channel.close_noerr oc)

let xml_to_fd rrd fd = with_out_channel_output fd (Rrd.xml_to_output rrd)

let json_to_fd rrd fd =
  let payload = Rrd.json_to_string rrd |> Bytes.unsafe_of_string in
  let len = Bytes.length payload in
  Unix.write fd payload 0 len |> ignore

let to_fd ?(json = false) rrd fd =
  (if json then json_to_fd else xml_to_fd) rrd fd
