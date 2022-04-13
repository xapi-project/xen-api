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

open Test_highlevel

module PoolCpuinfo = Generic.MakeStateful (struct
  module Io = struct
    type input_t = ((string * string) list * bool) list

    type output_t = (string * string) list

    let string_of_input_t =
      Test_printers.(list (pair (assoc_list string string) bool))

    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  module State = Test_state.XapiDb

  (* Create a host for each edition in the list. *)
  let load_input __context inputs =
    (* we don't want to count localhost, use the host(s) we
     * explicitly created *)
    List.iter
      (fun (cpu_info, hvm_capable) ->
        let host = Test_common.make_host ~__context () in
        Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info ;
        if hvm_capable then
          Db.Host.set_capabilities ~__context ~self:host ~value:["hvm"]
      )
      inputs ;
    ignore
      (Test_common.make_pool ~__context
         ~master:(List.hd (Db.Host.get_all ~__context))
         ()
      ) ;
    Create_misc.create_pool_cpuinfo ~__context

  let extract_output __context _ =
    let pool = Helpers.get_pool ~__context in
    List.sort compare (Db.Pool.get_cpu_info ~__context ~self:pool)

  let cpu_info_common ~vendor ~cpu_count ~socket_count ~features_hvm
      ~features_pv ~features_hvm_host ~features_pv_host =
    let cpu_info =
      [
        ("vendor", vendor)
      ; ("cpu_count", cpu_count)
      ; ("socket_count", socket_count)
      ; ("features_hvm", features_hvm)
      ; ("features_pv", features_pv)
      ; ("features_hvm_host", features_hvm_host)
      ; ("features_pv_host", features_pv_host)
      ]
    in
    (* Sort the associaton list so the test framework's comparisons work *)
    List.sort compare cpu_info

  let cpu_info ~vendor ~cpu_count ~socket_count ~features_hvm ~features_pv =
    cpu_info_common ~vendor ~cpu_count ~socket_count ~features_hvm ~features_pv
      ~features_hvm_host:features_hvm ~features_pv_host:features_pv

  let cpu_pinfo = cpu_info

  let tests =
    `QuickAndAutoDocumented
      [
        ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"0000000a" ~features_pv:"0000000a"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
            ~features_hvm:"0000000a" ~features_pv:"0000000a"
        )
      ; ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"2" ~socket_count:"4"
                ~features_hvm:"0000000a" ~features_pv:"0000000a"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"0000000a" ~features_pv:"0000000a"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"3" ~socket_count:"5"
            ~features_hvm:"0000000a" ~features_pv:"0000000a"
        )
      ; ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"8" ~socket_count:"2"
                ~features_hvm:"0000000a" ~features_pv:"00000002"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"4" ~socket_count:"1"
                ~features_hvm:"0000000f" ~features_pv:"00000001"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"12" ~socket_count:"3"
            ~features_hvm:"0000000a" ~features_pv:"00000000"
        )
      ; ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"24" ~socket_count:"1"
                ~features_hvm:"ffffffff-ffffffff"
                ~features_pv:"ffffffff-ffffffff"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"24" ~socket_count:"24"
                ~features_hvm:"ffffffff-ffffffff"
                ~features_pv:"ffffffff-ffffffff"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"48" ~socket_count:"25"
            ~features_hvm:"ffffffff-ffffffff" ~features_pv:"ffffffff-ffffffff"
        )
      ; ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"ffffffff"
                ~features_pv:"ffffffff-ffffffff-ffffffff"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"ffffffff-ffffffff"
                ~features_pv:"ffffffff-ffffffff"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"2" ~socket_count:"2"
            ~features_hvm:"ffffffff-00000000"
            ~features_pv:"ffffffff-ffffffff-00000000"
        )
      ; ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"10" ~socket_count:"1"
                ~features_hvm:"01230123-5a5a5a5a" ~features_pv:"00000002"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"10"
                ~features_hvm:"ffff1111-a5a56666" ~features_pv:"00004242"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"11" ~socket_count:"11"
            ~features_hvm:"01230101-00004242" ~features_pv:"00000002"
        )
      ; (* Include one host that is not HVM-capable *)
        ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"10" ~socket_count:"1"
                ~features_hvm:"00000000-00000000" ~features_pv:"00000002"
            , false
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"10"
                ~features_hvm:"ffff1111-a5a56666" ~features_pv:"00004242"
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"11" ~socket_count:"11"
            ~features_hvm:"ffff1111-a5a56666" ~features_pv:"00000002"
        )
      ; (* Test a Dundee host which has features_hvm, but not features_hvm_host (test for CA-188665 no longer relevant, was for pre-Dundeee) *)
        ( [
            ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"01230123-5a5a5a5a" ~features_pv:"00000002"
            , true
            )
          ; ( [
                ("cpu_count", "1")
              ; ("features", "ffff1111-a5a56666")
              ; ("features_hvm", "ffff1111-a5a56666")
              ; ("features_pv", "00004242")
              ; ("socket_count", "1")
              ; ("vendor", "Abacus")
              ]
            , true
            )
          ]
        , cpu_pinfo ~vendor:"Abacus" ~cpu_count:"2" ~socket_count:"2"
            ~features_hvm:"01230101-00004242" ~features_pv:"00000002"
        )
      ; (* Test that the new _host fields are used for pool leveling *)
        ( [
            ( cpu_info_common ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"deadbeef-deadbeef" ~features_pv:"deadbeef"
                ~features_hvm_host:"01230123-5a5a5a5a"
                ~features_pv_host:"00000002"
            , true
            )
          ; ( cpu_info ~vendor:"Abacus" ~cpu_count:"1" ~socket_count:"1"
                ~features_hvm:"01230123-5a5a5a5a" ~features_pv:"00000002"
            , true
            )
          ]
        , cpu_info_common ~vendor:"Abacus" ~cpu_count:"2" ~socket_count:"2"
            ~features_hvm:"00210023-5a081a4a" ~features_pv:"00000002"
            ~features_hvm_host:"01230123-5a5a5a5a" ~features_pv_host:"00000002"
        )
      ]
end)

let tests = [("test_pool_cpuinfo", PoolCpuinfo.tests)]
