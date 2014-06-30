(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit

let test_64_bit_marshall x () =
	List.iter (fun endianness ->
		let s = Io.marshall_int64 ~endianness x in
		let x' = Io.unmarshall_int64 ~endianness s in
		assert_equal ~msg:"Marshalling and unmarshalling changed value" x x'
	) [`big; `little]

let test_random_marshall n () =
	for i = 1 to n do
		let x = Random.int64 Int64.max_int in
		test_64_bit_marshall x ()
	done

let test =
	"test_io" >:::
		[
			"Max int marshall/unmarshall" >:: test_64_bit_marshall (Int64.max_int);
			"Min int marshall/unmarshall" >:: test_64_bit_marshall (Int64.min_int);
			"Zero int marshall/unmarshall" >:: test_64_bit_marshall 0L;
			"100x random int marshall/unmarshall" >:: test_random_marshall 100;
		]
