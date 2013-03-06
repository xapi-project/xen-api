(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open Fun
open OUnit
open Test_highlevel

type host_license_state = {
	license_params: (string * string) list;
	edition: string;
}

let string_of_string_map map =
	Printf.sprintf "[%s]"
		(List.map (fun (k, v) -> k ^ ": " ^ v) map |> String.concat "; ")

let string_of_host_license_state state =
	Printf.sprintf "{license_params = %s; edition = %s"
		(string_of_string_map state.license_params)
		state.edition

let string_of_date_opt = function
	| None -> "None"
	| Some date -> Printf.sprintf "Some %s" (Date.to_string date)

let f2d = Date.of_float
let f2d2s = Date.to_string ++ Date.of_float

module CompareDates = struct
	module Tests = Generic.Make(struct
		module Io = struct
			type input_t = (Date.iso8601 option * Date.iso8601 option)
			type output_t = int

			let string_of_input_t input =
				Printf.sprintf "(%s, %s)"
					(string_of_date_opt (fst input))
					(string_of_date_opt (snd input))

			let string_of_output_t = string_of_int
		end

			let transform (date1, date2) = Xapi_pool_license.compare_dates date1 date2
	end)

	(* Tuples of ((value 1, value 2), expected result from comparing values) *)
	let compare_date_tests = [
		((None, None), 0);
		((None, Some (f2d 5.0)), 1);
		((Some (f2d 10.0), Some (f2d 5.0)), 1);
		((Some (f2d 15.0), None), -1);
		((Some (f2d 20.0), Some (f2d 30.0)), -1);
		((Some (f2d 150.0), Some (f2d 150.0)), 0);
	]

	(* Compare each pair of date options, and check that the result is what we expect. *)
	let test () =
		Tests.test_equal_multiple ~input_output_pairs:compare_date_tests
end

module PoolExpiryDate = struct
	module Tests = Generic.Make(Generic.EncapsulateState(struct
		module Io = struct
			type input_t = Date.iso8601 option list
			type output_t = Date.iso8601 option

			let string_of_input_t input =
				Printf.sprintf "[%s]"
					(input |> List.map string_of_date_opt |> String.concat "; ")

			let string_of_output_t = string_of_date_opt
		end
		module State = XapiDb

		(* Create a host in the database for each expiry date in the list. *)
		let load_input __context expiry_dates =
			List.iter
				(fun expiry_date ->
					let license_params = match expiry_date with
					| None -> []
					| Some date -> ["expiry", (Date.to_string date)]
					in
					let (_: API.ref_host) = Test_common.make_host ~__context ~license_params () in ())
				expiry_dates

		let extract_output __context =
			let hosts = Db.Host.get_all ~__context in
			Xapi_pool_license.get_earliest_expiry_date ~__context ~hosts
	end))

	(* Tuples of ((host expiry date) list, expected pool expiry date) *)
	let expiry_date_tests = [
		([None; None; Some (f2d 500.0); None], Some (f2d 500.0));
		([None; None; None; None], None);
		([Some (f2d 100.0)], Some (f2d 100.0));
		([Some (f2d 300.0); Some (f2d 150.0); Some (f2d 450.0)], Some (f2d 150.0));
		([None; Some (f2d 650.0); None; Some (f2d 350.0)], Some (f2d 350.0));
	]

	let test () =
		Tests.test_equal_multiple ~input_output_pairs:expiry_date_tests
end

module PoolEdition = struct
	module Tests = Generic.Make(Generic.EncapsulateState(struct
		module Io = struct
			type input_t = string list
			type output_t = string

			let string_of_input_t input =
				Printf.sprintf "[%s]" (String.concat "; " input)
			let string_of_output_t = (fun x -> x)
		end
		module State = XapiDb

		(* Create a host for each edition in the list. *)
		let load_input __context editions =
			List.iter
				(fun edition ->
					let (_: API.ref_host) = Test_common.make_host ~__context ~edition () in ())
				editions

		let extract_output __context =
			let hosts = Db.Host.get_all ~__context in
			Xapi_pool_license.get_lowest_edition ~__context ~hosts
	end))

	(* Tuples of ((host edition) list, expected pool edition) *)
	let pool_edition_tests = [
		(["free"], "free");
		(["free"; "per-socket"; "free"; "per-socket"], "free");
		(["xendesktop"; "xendesktop"; "xendesktop"; "xendesktop"], "xendesktop");
		(["per-socket"; "per-socket"; "per-socket"], "per-socket");
		(["xendesktop"; "xendesktop"; "free"; "free"], "free");
	]

	let test () =
		Tests.test_equal_multiple ~input_output_pairs:pool_edition_tests
end

module PoolLicenseState = struct
	module Tests = Generic.Make(Generic.EncapsulateState(struct
		module Io = struct
			type input_t = host_license_state list
			type output_t = (string * string) list

			let string_of_input_t input =
				Printf.sprintf "[%s]"
					(List.map string_of_host_license_state input |> String.concat "; ")
			let string_of_output_t = string_of_string_map
		end
		module State = XapiDb

		(* For each (license_params, edition) pair, create a host.
		 * Also create a pool object. *)
		let load_input __context hosts =
			List.iter
				(fun host ->
					let (_: API.ref_host) =
						Test_common.make_host ~__context
							~edition:host.edition
							~license_params:host.license_params () in ())
				hosts;
			let (_: API.ref_pool) =
				Test_common.make_pool ~__context
					~master:(List.hd (Db.Host.get_all ~__context)) () in ()

		let extract_output __context =
			let pool = Helpers.get_pool ~__context in
			Xapi_pool.get_license_state ~__context ~self:pool
	end))

	(* Tuples of (host_license_state list, expected pool license state) *)
	let pool_license_tests = [
		(* A pool of free edition hosts, none of which has an expiry date. *)
		([
			{license_params = []; edition = "free"};
			{license_params = []; edition = "free"};
			{license_params = []; edition = "free"};
		],
		["edition", "free"; "expiry", "never"]);
		(* A pool of per-socket edition hosts, of which two have expiry dates. *)
		([
			{license_params = []; edition = "per-socket"};
			{license_params = ["expiry", f2d2s 500.0]; edition = "per-socket"};
			{license_params = ["expiry", f2d2s 350.0]; edition = "per-socket"};
		],
		["edition", "per-socket"; "expiry", f2d2s 350.0]);
		(* A pool of per-socket edition hosts, of which none have expiry dates. *)
		([
			{license_params = []; edition = "per-socket"};
			{license_params = []; edition = "per-socket"};
			{license_params = []; edition = "per-socket"};
		],
		["edition", "per-socket"; "expiry", "never"]);
		(* A pool of xendesktop edition hosts, of which none have expiry dates. *)
		([
			{license_params = []; edition = "xendesktop"};
			{license_params = []; edition = "xendesktop"};
			{license_params = []; edition = "xendesktop"};
		],
		["edition", "xendesktop"; "expiry", "never"]);
		(* A pool of hosts, some per-socket (with different expiry dates) and some free. *)
		([
			{license_params = ["expiry", f2d2s 5000.0]; edition = "per-socket"};
			{license_params = []; edition = "free"};
			{license_params = ["expiry", f2d2s 6000.0]; edition = "per-socket"};
		],
		["edition", "free"; "expiry", "never"]);
	]

	let test () =
		Tests.test_equal_multiple ~input_output_pairs:pool_license_tests
end

let test =
	"pool_license" >:::
		[
			"test_compare_dates" >:: CompareDates.test;
			"test_pool_expiry_date" >:: PoolExpiryDate.test;
			"test_pool_edition" >:: PoolEdition.test;
			"test_pool_license_state" >:: PoolLicenseState.test;
		]
