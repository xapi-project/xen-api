(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open OUnit
open Crc

(* Expected CRCs with an unusual initial value (from LVM) *)
let full_crc_tests_from_lvm = [
	"hello world", Int32.neg 375048304l;
	"LVM uses CRC-32", 1179429198l;
]

let lvm_crc32 x =
	(* LVM2's CRC is different for no good reason *)
	let crc = Int32.logxor 0xf597a6cfl (-1l) in
	let y = Crc32.string ~crc x 0 (String.length x) in
	Int32.logxor y (-1l)

let test_crc_from_lvm data expected_crc =
	let crc = lvm_crc32 data in
	assert_equal ~printer:Int32.to_string expected_crc crc

(* Expected CRCs calculated using python's zlib.crc32. *)
let full_crc_tests = [
	"", 0l;
	"foobar", Int32.neg 1628037227l;
	"as89f7d8f798d7f987f0daf", 822820715l;
	";£*%($)(^$&", Int32.neg 1888319081l;
	"\\\n\\xyz", 355115745l;
]

let test_crc_all_string data expected_crc =
	let crc = Crc32.string data 0 (String.length data) in
	assert_equal ~printer:Int32.to_string expected_crc crc

let test_crc_all_cstruct data expected_crc =
	let cstruct = Cstruct.of_string data in
	let crc = Crc32.cstruct cstruct in
	assert_equal ~printer:Int32.to_string expected_crc crc

let suite_test_crc_all =
	let make_tests tests test_fn test_base_name =
		List.map
			(fun (test_string, expected_crc) ->
				let test_name =
					Printf.sprintf
						"%s: \"%s\"" test_base_name (String.escaped test_string)
				in
				test_name >:: (fun () -> test_fn test_string expected_crc))
			tests
	in
	"suite_test_crc_all" >:::
		((make_tests full_crc_tests test_crc_all_string "test_crc_all_string") @
		(make_tests full_crc_tests test_crc_all_cstruct "test_crc_all_cstruct") @
		(make_tests full_crc_tests_from_lvm test_crc_from_lvm "test_crc_from_lvm"))

let part_crc_tests = [
	"foobarbaz", 5, 0, 0l;
	"abcfoobardef", 3, 6, Int32.neg 1628037227l;
	"lmn\\\n\\xyzopq", 3, 6, 355115745l;
]

let test_crc_part_string data offset length expected_crc =
	let crc = Crc32.string data offset length in
	assert_equal crc expected_crc

let test_crc_part_cstruct data offset length expected_crc =
	let cstruct = Cstruct.of_string data in
	let crc = Crc32.cstruct (Cstruct.sub cstruct offset length) in
	assert_equal crc expected_crc

let suite_test_crc_part =
	let make_tests test_fn test_base_name =
		List.map
			(fun (test_string, offset, length, expected_crc) ->
				let test_name =
					Printf.sprintf
						"%s: \"%s\"" test_base_name (String.escaped test_string)
				in
				test_name >::
					(fun () -> test_fn test_string offset length expected_crc))
			part_crc_tests
	in
	"suite_test_crc_part" >:::
		((make_tests test_crc_part_string "test_crc_part_string") @
		(make_tests test_crc_part_cstruct "test_crc_part_cstruct"))

let update_crc_tests = [
	"", "bar", 1996459178l;
	"bar", "", 1996459178l;
	"bar", "baz", 385868946l;
	"[12pl34.", ",2l3\n\t", 1481911597l;
]

let test_crc_update_string data_first data_second expected_crc =
	let data_all = data_first ^ data_second in
	let crc_all = Crc32.string data_all 0 (String.length data_all) in

	let crc_first = Crc32.string data_first 0 (String.length data_first) in
	let crc_parts =
		Crc32.string
			~crc:crc_first
			data_second 0 (String.length data_second)
	in

	assert_equal crc_all crc_parts

let test_crc_update_cstruct data_first data_second expected_crc =
	let cstruct_all = Cstruct.of_string (data_first ^ data_second) in
	let cstruct_first = Cstruct.of_string data_first in
	let cstruct_second = Cstruct.of_string data_second in

	let crc_all = Crc32.cstruct cstruct_all in
	let crc_first = Crc32.cstruct cstruct_first in
	let crc_parts = Crc32.cstruct ~crc:crc_first cstruct_second in

	assert_equal crc_all crc_parts

let suite_test_crc_update =
	let make_tests test_fn test_base_name =
		List.map
			(fun (test_string_first, test_string_second, expected_crc) ->
				let test_name =
					Printf.sprintf
						"%s: \"%s\",\"%s\""
						test_base_name
						(String.escaped test_string_first)
						(String.escaped test_string_second)
				in
				test_name >::
					(fun () -> test_fn
							test_string_first test_string_second expected_crc))
			update_crc_tests
	in
	"suite_test_crc_update" >:::
		((make_tests test_crc_update_string "test_crc_update_string") @
		(make_tests test_crc_update_cstruct "test_crc_update_cstruct"))

let test_negative_length () =
	assert_raises Crc.Invalid_length
		(fun () -> Crc32.string "foobar" 2 (-5))

let test_negative_offset () =
	assert_raises Crc.Invalid_offset
		(fun () -> Crc32.string "foobar" (-3) 4)

let test_too_large_length () =
	assert_raises Crc.Invalid_length
		(fun () -> Crc32.string "foobar" 3 5)

let test_too_large_offset () =
	assert_raises Crc.Invalid_offset
		(fun () -> Crc32.string "foobar" 7 2)

let suite_test_bounds_checking =
	"suite_test_bounds_checking" >:::
		[
			"test_negative_length" >:: test_negative_length;
			"test_negative_offset" >:: test_negative_offset;
			"test_too_large_length" >:: test_too_large_length;
			"test_too_large_offset" >:: test_too_large_offset;
		]

let base_suite =
	"base_suite" >:::
		[
			suite_test_crc_all;
			suite_test_crc_part;
			suite_test_crc_update;
			suite_test_bounds_checking;
		]

let () = OUnit2.run_test_tt_main (ounit2_of_ounit1 base_suite)
