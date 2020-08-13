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

module Process = Rrdd_plugin.Process(struct let name = "xcp-rrdd-dummy" end)

let make_cnt start =
  let i = ref (start-1) in
  let f () = incr i; Int64.of_int !i in
  f

let cnt = make_cnt 0

let generate_dummy_dss () =
  [
    Rrd.Host,
    Ds.ds_make ~name:"dummy-metric" ~description:"Dummy data" ~value:(Rrd.VT_Int64 (cnt ())) ~ty:(Rrd.Gauge)
      ~default:true ~units:"Pixies" ()
  ]

let _ =
  let mode = ref (Rrdd_plugin.Reporter.Local 1) in
  Arg.parse
    [("-mode",
      Arg.String (function
          | "local" -> mode := Rrdd_plugin.Reporter.Local 1
          | "interdomain" -> mode := Rrdd_plugin.Reporter.Interdomain (0, 1)
          | x -> invalid_arg x),
      "Switch between local and interdomain mode")]
    (fun _ -> ())
    (Printf.sprintf "Usage: %s -mode [local|interdomain]" Sys.executable_name);

  Process.initialise ();
  Process.main_loop
    ~neg_shift:0.5
    ~target:!mode
    ~protocol:Rrd_interface.V2
    ~dss_f:generate_dummy_dss
