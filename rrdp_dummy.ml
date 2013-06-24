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

open Rrdp_common

module Common = Common(struct let name = "xcp-rrdd-dummy" end)
open Common

let make_cnt start =
	let i = ref (start-1) in
	let f () = incr i; Int64.of_int !i in
	f
	
let cnt = make_cnt 0 		

let generate_dummy_dss () =
	[Ds.ds_make ~name:"dummy-metric" ~description:"Dummy data" ~value:(Rrd.VT_Int64 (cnt ())) ~ty:(Rrd.Gauge)
		~default:true ~units:"Pixies" (), Rrd.Host]

let _ =
	initialise ();
	main_loop ~dss_f:generate_dummy_dss
