(*
 * Copyright (C) 2017 Citrix Systems Inc.
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
open Test_common

module D=Debug.Make(struct let name="test_xapi_xenops" end)
open D

let add_host __context name =
  ignore(Xapi_host.create ~__context
    ~uuid:(Uuid.to_string (Uuid.make_uuid()))
    ~name_label:name ~name_description:"" ~hostname:"host2" ~address:"127.0.0.1"
    ~external_auth_type:"" ~external_auth_service_name:"" ~external_auth_configuration:[]
    ~license_params:[] ~edition:"" ~license_server:[] ~local_cache_sr:(Ref.null) ~chipset_info:[] ~ssl_legacy:false)

(* Creates an unlicensed pool with the maximum number of hosts *)
let setup_test () =
  (* Create an unlicensed pool *)
  let __context = make_test_database () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  Db.Pool.set_restrictions ~__context ~self:pool ~value:(Features.to_assoc_list []);

  (* Add hosts until we're at the maximum unlicensed pool size *)
  while (Db.Host.get_all ~__context |> List.length < Xapi_globs.restricted_pool_size) do
    add_host __context "host";
  done;
  Alcotest.(check int) "initial number of hosts"
    Xapi_globs.restricted_pool_size
    (Db.Host.get_all ~__context |> List.length);
  __context

let test_host_join_restriction () =
  let __context = setup_test () in

  (* Check adding one more is a failure *)
  Alcotest.check_raises "Should fail"
    (Api_errors.Server_error(Api_errors.license_restriction, [Features.name_of_feature Features.Pool_size]))
    (fun () -> ignore(add_host __context "badhost"));

  (* License the pool *)
  let pool = Db.Pool.get_all ~__context |> List.hd in
  Db.Pool.set_restrictions ~__context ~self:pool ~value:(Features.to_assoc_list [Features.Pool_size]);

  (* Adding hosts should now work *)
  add_host __context "goodhost";
  Alcotest.(check int) "one added OK"
    (Xapi_globs.restricted_pool_size + 1)
    (Db.Host.get_all ~__context |> List.length)

let test_cvm_exception () =
  let __context = setup_test () in

  (* Check adding one more is a failure *)
  Alcotest.check_raises "Should fail"
    (Api_errors.Server_error(Api_errors.license_restriction, [Features.name_of_feature Features.Pool_size]))
    (fun () -> ignore(add_host __context "badhost"));

  let new_vm = Test_common.make_vm ~__context ~name_label:"My test VM-CVM" () in
  Db.VM.set_is_control_domain ~__context ~self:new_vm ~value:true;

  (* Adding hosts should now work *)
  add_host __context "goodhost";
  Alcotest.(check int) "one added OK"
    (Xapi_globs.restricted_pool_size + 1)
    (Db.Host.get_all ~__context |> List.length)

let test =
  [
    "test_host_join_restriction", `Quick, test_host_join_restriction;
    "test_host_join_cvm_exception", `Quick, test_cvm_exception;
  ]
