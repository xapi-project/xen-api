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

open Stdext
open Fun
open OUnit
open Test_highlevel

type host_license_state = {
  license_params: (string * string) list;
  edition: string;
}

let string_of_host_license_state state =
  Printf.sprintf "{license_params = %s; edition = %s}"
    (Test_printers.(assoc_list string string) state.license_params)
    state.edition

let string_of_date_opt = function
  | None -> "None"
  | Some date -> Printf.sprintf "Some %s" (Date.to_string date)

let f2d = Date.of_float
let f2d2s f = f |> Date.of_float |> Date.to_string

let edition_to_int = ["edition1", 1; "edition2", 2; "edition3", 3]

module CompareDates = Generic.Make(struct
    module Io = struct
      type input_t = (Date.iso8601 option * Date.iso8601 option)
      type output_t = int

      let string_of_input_t =
        Test_printers.(assoc_pair (option Date.to_string) (option Date.to_string))

      let string_of_output_t = Test_printers.int
    end

    let transform (date1, date2) = Xapi_pool_license.compare_dates date1 date2

    (* Tuples of ((value 1, value 2), expected result from comparing values) *)
    let tests = [
      ((None, None), 0);
      ((None, Some (f2d 5.0)), 1);
      ((Some (f2d 10.0), Some (f2d 5.0)), 1);
      ((Some (f2d 15.0), None), -1);
      ((Some (f2d 20.0), Some (f2d 30.0)), -1);
      ((Some (f2d 150.0), Some (f2d 150.0)), 0);
    ]
  end)

module PoolExpiryDate = Generic.Make(Generic.EncapsulateState(struct
                                       module Io = struct
                                         type input_t = Date.iso8601 option list
                                         type output_t = Date.iso8601 option

                                         let string_of_input_t =
                                           Test_printers.(list (option Date.to_string))

                                         let string_of_output_t = Test_printers.option Date.to_string
                                       end
                                       module State = Test_state.XapiDb

                                       (* Create a host in the database for each expiry date in the list. *)
                                       let load_input __context expiry_dates =
                                         List.iter
                                           (fun expiry_date ->
                                              let license_params = match expiry_date with
                                                | None -> []
                                                | Some date -> ["expiry", (Date.to_string date)]
                                              in
                                              let (_: API.ref_host) = Test_common.make_host ~__context ~edition:"edition1" ~license_params () in ())
                                           expiry_dates

                                       let extract_output __context _ =
                                         let hosts = Db.Host.get_all ~__context in
                                         snd (Xapi_pool_license.get_lowest_edition_with_expiry ~__context ~hosts ~edition_to_int)

                                       (* Tuples of ((host expiry date) list, expected pool expiry date) *)
                                       let tests = [
                                         ([None; None; Some (f2d 500.0); None], Some (f2d 500.0));
                                         ([None; None; None; None], None);
                                         ([Some (f2d 100.0)], Some (f2d 100.0));
                                         ([Some (f2d 300.0); Some (f2d 150.0); Some (f2d 450.0)], Some (f2d 150.0));
                                         ([None; Some (f2d 650.0); None; Some (f2d 350.0)], Some (f2d 350.0));
                                       ]
                                     end))

module PoolEdition = Generic.Make(Generic.EncapsulateState(struct
                                    module Io = struct
                                      type input_t = string list
                                      type output_t = string

                                      let string_of_input_t = Test_printers.(list string)
                                      let string_of_output_t = Test_printers.string
                                    end
                                    module State = Test_state.XapiDb

                                    (* Create a host for each edition in the list. *)
                                    let load_input __context editions =
                                      List.iter
                                        (fun edition ->
                                           let (_: API.ref_host) = Test_common.make_host ~__context ~edition () in ())
                                        editions

                                    let extract_output __context _ =
                                      let hosts = Db.Host.get_all ~__context in
                                      fst (Xapi_pool_license.get_lowest_edition_with_expiry ~__context ~hosts ~edition_to_int)

                                    (* Tuples of ((host edition) list, expected pool edition) *)
                                    let tests = [
                                      (["edition1"], "edition1");
                                      (["edition1"; "edition2"; "edition1"; "edition3"], "edition1");
                                      (["edition2"; "edition2"; "edition2"; "edition2"], "edition2");
                                      (["edition3"; "edition3"; "edition3"], "edition3");
                                      (["edition2"; "edition2"; "edition1"; "edition1"], "edition1");
                                    ]
                                  end))

module PoolLicenseState = Generic.Make(Generic.EncapsulateState(struct
                                         module Io = struct
                                           type input_t = host_license_state list
                                           type output_t = string * string

                                           let string_of_input_t = Test_printers.(list string_of_host_license_state)
                                           let string_of_output_t = Test_printers.(pair string string)
                                         end
                                         module State = Test_state.XapiDb

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

                                         let extract_output __context _ =
                                           let hosts = Db.Host.get_all ~__context in
                                           let pool_edition, expiry = Xapi_pool_license.get_lowest_edition_with_expiry ~__context ~hosts ~edition_to_int in
                                           let pool_expiry =
                                             match expiry with
                                             | None -> "never"
                                             | Some date -> if date = Date.of_float License_check.never then "never" else Date.to_string date
                                           in
                                           pool_edition, pool_expiry

                                         (* Tuples of (host_license_state list, expected pool license state) *)
                                         let tests = [
                                           (* A pool of edition1 hosts, none of which has an expiry date. *)
                                           [
                                             {license_params = []; edition = "edition1"};
                                             {license_params = []; edition = "edition1"};
                                             {license_params = []; edition = "edition1"};
                                           ],
                                           ("edition1", "never");
                                           (* A pool of edition2 hosts, of which two have expiry dates. *)
                                           [
                                             {license_params = []; edition = "edition2"};
                                             {license_params = ["expiry", f2d2s 500.0]; edition = "edition2"};
                                             {license_params = ["expiry", f2d2s 350.0]; edition = "edition2"};
                                           ],
                                           ("edition2", f2d2s 350.0);
                                           (* A pool of edition2 hosts, of which none have expiry dates. *)
                                           [
                                             {license_params = []; edition = "edition2"};
                                             {license_params = []; edition = "edition2"};
                                             {license_params = []; edition = "edition2"};
                                           ],
                                           ("edition2", "never");
                                           (* A pool of hosts, some edition2 (with different expiry dates) and some edition1 (no expiry). *)
                                           [
                                             {license_params = ["expiry", f2d2s 5000.0]; edition = "edition2"};
                                             {license_params = []; edition = "edition1"};
                                             {license_params = ["expiry", f2d2s 6000.0]; edition = "edition2"};
                                           ],
                                           ("edition1", "never");
                                         ]
                                       end))

let test =
  "pool_license" >:::
  [
    "test_compare_dates" >::: CompareDates.tests;
    "test_pool_expiry_date" >::: PoolExpiryDate.tests;
    "test_pool_edition" >::: PoolEdition.tests;
    "test_pool_license_state" >::: PoolLicenseState.tests;
  ]
