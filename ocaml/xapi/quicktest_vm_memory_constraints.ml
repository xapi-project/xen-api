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
open Quicktest_ocamltest
open Ocamltest
open Vm_memory_constraints.Vm_memory_constraints

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

(** Creates a memory constraints record (with values in bytes) from the given
memory constraints tuple (with values in MiB). *)
let create (static_min, dynamic_min, target, dynamic_max, static_max) =
	let scale value = (Int64.of_int value) ** 1024L ** 1024L in
	{ static_min  = scale static_min
	; dynamic_min = scale dynamic_min
	; target      = scale target
	; dynamic_max = scale dynamic_max
	; static_max  = scale static_max
	}

let test_reset_to_safe_defaults = make_test_case
	"reset_to_safe_defaults"
	"Tests the reset_to_safe_defaults function."
	(fun () ->
		List.iter
			(fun (input, output) ->
				let reset constraints = reset_to_safe_defaults ~constraints in
				assert_equal (reset (create input)) (create output))
			[( 256, 512,1024,2048,4096), ( 256,4096,4096,4096,4096)
			;(4096,2048,1024, 512, 256), ( 256, 256, 256, 256, 256)
			;(1024,1024,1024,1024,1024), (1024,1024,1024,1024,1024)
			]
	)

let tests = make_module_test_suite "VM_memory_constraints"
[
	test_reset_to_safe_defaults;
]

let run_from_within_quicktest () = run_from_within_quicktest tests
