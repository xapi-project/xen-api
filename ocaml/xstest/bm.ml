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
(*open OUnit*)
open Printf
open Xenstore

let fill_store () =
	let xsh = Xs.daemon_open () in
	List.iter (fun (a,b) -> xsh.Xs.write a b)
		[ "/benchs", "";
		  "/benchs/ooo/x", "1";
		  "/benchs/xxx", "xxx";
		  "/benchs/xxx/qwr", "206";
		  "/benchs/yyy", "yyy";
		  "/benchs/yyy/www", "2901";
		  "/benchs/zzz", "zzz";
		  "/benchs/zzz/xyz", "zzz";
		  "/benchs/1", "";
		  "/benchs/1/www", "abc";
		  "/benchs/2", "";
		  "/benchs/2/qwr", "avds";
		  "/benchs/3", "";
		  "/benchs/3/x", "xxx";
		  "/benchs/4", "";
		  "/benchs/4/xyz", "abc"; ];
	Xs.close xsh

let bench_single () =
	let xsh = Xs.daemon_open () in
	Bench.output stdout "single read" (Bench.multi ~times:1000 (fun () ->
		ignore (xsh.Xs.read "/benchs/xxx")));
	Xs.close xsh

let bench_single_wr () =
	let xsh = Xs.daemon_open () in
	Bench.output stdout "single write" (Bench.multi ~times:1000 (fun () ->
		xsh.Xs.write "/benchs/zzz/123" "abc"));
	Xs.close xsh

let really_maybe_yield () =
	if Random.bool () then Thread.delay 0.00000001

let maybe_yield () =
	(* if Random.bool () then Thread.yield () *)
	if true then Thread.delay 0.00000001

let bench_transaction ?(single_read_path=false) ~test_name ~times tests =
	let doit ~read_path ~write_path name xsh =
		try 
			let i = ref 0 in
			Xs.transaction xsh (fun t ->
				incr i;
				let v =
					try t.Xst.read (read_path ^ "/www")
					with _ ->
						t.Xst.write (read_path ^ "/www") "2901";
						"2901"
					in
				t.Xst.write (write_path ^ "/abc") v;
				t.Xst.write (write_path ^ "/abc2") (v ^ "abc2");
			);
			!i
		with exn ->
			printf "%s raise an exception %s\n%!" name (Printexc.to_string exn);
			raise exn
		in

	let fct fct_transact () =
		let xsh = Xs.daemon_open () in
		let eagain = ref 0 in

		let a1, a2, a3, a4, a5, a6 = Bench.multi ~times (fun () ->
			try
				let i = (fct_transact xsh) in
				if i > 1 then
					eagain := !eagain + i - 1
			with exn ->
				printf "X exception: %s\n%!" (Printexc.to_string exn)) in
		Xs.close xsh;
		a1, a2, a3, a4, !eagain, a6 in

	let generate offset i =
		Array.init i (fun i ->
			let i1 = i + 1 in 
			let write_path = sprintf "/benchs/%d" (offset + i1) in
			let read_path = if single_read_path then "/benchs/0" else write_path in
			let name = sprintf "%d" i1 in
			fct (doit ~read_path ~write_path name)
		)
		in

	let rec aux last_n = function
		| n::t -> Bench.multifork (Printf.sprintf "%s%i" test_name n) (generate last_n n); aux n t
		| [] -> ()
	in
	
	aux 0 tests

let bench_slowfast () =
	let ft1 xsh =
		let i = ref 0 in
		ignore (Xs.transaction xsh (fun t ->
			incr i;
			let v = t.Xst.read "/benchs/1/www" in
			List.iter (fun path ->
				t.Xst.write ("/benchs/1/" ^ path) v;
				really_maybe_yield ())
				[ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ];
			maybe_yield ();
		)); !i
	and ft2 xsh =
		let i = ref 0 in
		ignore (Xs.transaction xsh (fun t ->
			incr i;
			ignore (t.Xst.write "/benchs/2/www" "x");
			maybe_yield ();
			)); !i
	and ft3 xsh =
		let i = ref 0 in
		ignore (Xs.transaction xsh (fun t ->
			incr i;
			ignore (t.Xst.write "/benchs/3/www" "x");
			maybe_yield ();
			)); !i
	and ft4 xsh =
		let i = ref 0 in
		ignore (Xs.transaction xsh (fun t ->
			incr i;
			ignore (t.Xst.write "/benchs/4/www" "x");
			maybe_yield ();
			)); !i
		in
	let fct fct_transact () =
		let xsh = Xs.daemon_open () in
		let eagain = ref 0 in

		let a1, a2, a3, a4, a5, a6 = Bench.multi ~times:10000 (fun () ->
			try
				let i = (fct_transact xsh) in
				if i > 1 then
					eagain := !eagain + i - 1
			with exn ->
				printf "X exception: %s\n%!" (Printexc.to_string exn)) in
		Xs.close xsh;
		a1, a2, a3, a4, !eagain, a6 in

	Bench.multithread "sf2" [| (fct ft1); (fct ft2); |];
	Bench.multithread "sf4" [| (fct ft1); (fct ft2); (fct ft3); (fct ft4); |]

let single = ref false
let single_wr = ref false
let transaction = ref false
let transaction_wr = ref false
let slowfast = ref false

type transaction_test = { times: int; name: string; tests: int list }
let t1 = { times=1000; name="t"; tests=[2;4;8;16;32] }
let t2 = { times=50; name="twr"; tests=[128] }

let classic () = 
	single := true;
	single_wr := true;
	transaction := true

let _ =
  Arg.parse (Arg.align
	[ "--classic", Arg.Unit classic, " perform the 'single', 'single_wr' and 'transaction' tests";
	  "--single", Arg.Set single, " perform the 'single' tests";
	  "--single-wr", Arg.Set single_wr, " perform the 'single_wr' tests";
	  "--transaction", Arg.Set transaction, " perform the 'transaction' tests";
	  "--transaction_wr", Arg.Set transaction_wr, " perform the 'transaction_wr' tests";
	  "--slowfast", Arg.Set slowfast, " perform the 'slowfast' tests"])
	(fun _ -> ())
	("usage:");

	(try fill_store (); with exn -> printf "fill_store exn\n%!"; raise exn);
	if !single then (try bench_single (); with exn -> printf "bench_single exn\n%!"; raise exn);
	if !single_wr then (try bench_single_wr (); with exn -> printf "bench_single_wr exn\n%!"; raise exn);
	if !transaction then (try bench_transaction ~test_name:t1.name ~times:t1.times t1.tests; with exn -> printf "bench_trans exn\n%!"; raise exn);
	if !transaction_wr then (try bench_transaction ~single_read_path:true ~test_name:t2.name ~times:t2.times t2.tests; with exn -> printf "bench_trans exn\n%!"; raise exn);
	if !slowfast then (try bench_slowfast (); with exn -> printf "bench_slowfast exn\n%!"; raise exn);
	()
