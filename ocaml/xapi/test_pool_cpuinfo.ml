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
module PoolCpuinfo = Generic.Make(Generic.EncapsulateState(struct
                                    module Io = struct
                                      type input_t = (string * string) list list
                                      type output_t = (string * string) list

                                      let string_of_input_t = Test_printers.(list (assoc_list string string))
                                      let string_of_output_t = Test_printers.(assoc_list string string)
                                    end
                                    module State = Test_state.XapiDb

                                    (* Create a host for each edition in the list. *)
                                    let load_input __context inputs =
                                      List.iter
                                        (fun cpu_info ->
                                           let host = Test_common.make_host ~__context () in
                                           Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info)
                                        inputs;
                                      ignore (Test_common.make_pool ~__context
                                                ~master:(List.hd (Db.Host.get_all ~__context)) ());
                                      Create_misc.create_pool_cpuinfo ~__context


                                    let extract_output __context _ =
                                      let pool = Helpers.get_pool ~__context in
                                      List.sort compare (Db.Pool.get_cpu_info ~__context ~self:pool)

                                    let cpu_info ~vendor ~cpu_count ~socket_count ~features_hvm ~features_pv =
                                      let cpu_info =
                                        ["vendor", vendor;
                                         "cpu_count", cpu_count;
                                         "socket_count", socket_count;
                                         "features_hvm", features_pv;
                                         "features_pv", features_hvm] in

                                      (* Sort the associaton list so the test framework's comparisons work *)
                                      List.sort compare cpu_info

                                    let tests = [
                                      ([cpu_info "Abacus" "1" "1" "0000000a" "0000000a"],
                                       cpu_info "Abacus" "1" "1" "0000000a" "0000000a");

                                      ([cpu_info "Abacus" "2" "4" "0000000a" "0000000a";
                                        cpu_info "Abacus" "1" "1" "0000000a" "0000000a"],
                                       cpu_info "Abacus" "3" "5" "0000000a" "0000000a");

                                      ([cpu_info "Abacus" "8" "2" "0000000a" "00000002";
                                        cpu_info "Abacus" "4" "1" "0000000f" "00000001"],
                                       cpu_info "Abacus" "12" "3" "0000000a" "00000000");

                                      ([cpu_info "Abacus" "24" "1" "ffffffff-ffffffff" "ffffffff-ffffffff";
                                        cpu_info "Abacus" "24" "24" "ffffffff-ffffffff" "ffffffff-ffffffff"],
                                       cpu_info "Abacus" "48" "25" "ffffffff-ffffffff" "ffffffff-ffffffff");

                                      ([cpu_info "Abacus" "1" "1" "ffffffff" "ffffffff-ffffffff-ffffffff";
                                        cpu_info "Abacus" "1" "1" "ffffffff-ffffffff" "ffffffff-ffffffff"],
                                       cpu_info "Abacus" "2" "2" "ffffffff-00000000" "ffffffff-ffffffff-00000000");

                                      ([cpu_info "Abacus" "10" "1" "01230123-5a5a5a5a" "00000002";
                                        cpu_info "Abacus" "1" "10" "ffff1111-a5a56666" "00004242"],
                                       cpu_info "Abacus" "11" "11" "01230101-00004242" "00000002");

                                      (* CA-188665: Test a pool containing an old host which doesn't have the new feature keys *)
                                      ([cpu_info "Abacus" "1" "1" "01230123-5a5a5a5a" "00000002";
                                        ["cpu_count", "1"; "features", "ffff1111-a5a56666"; "socket_count", "1"; "vendor", "Abacus"]],
                                       cpu_info "Abacus" "1" "1" "01230123-5a5a5a5a" "00000002");
                                    ]
                                  end))

let test =
  "pool_cpuinfo" >:::
  [
    "test_pool_cpuinfo" >::: PoolCpuinfo.tests;
  ]
