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

open Test_common

let add_host __context name =
  ignore
    (Xapi_host.create ~__context
       ~uuid:(Uuidx.to_string (Uuidx.make ()))
       ~name_label:name ~name_description:"" ~hostname:"host2"
       ~address:"127.0.0.1" ~external_auth_type:""
       ~external_auth_service_name:"" ~external_auth_configuration:[]
       ~license_params:[] ~edition:"" ~license_server:[]
       ~local_cache_sr:Ref.null ~chipset_info:[] ~ssl_legacy:false
       ~last_software_update:Clock.Date.epoch ~last_update_hash:""
       ~ssh_enabled:true ~ssh_enabled_timeout:0L ~ssh_expiry:Clock.Date.epoch
       ~console_idle_timeout:0L ~ssh_auto_mode:false ~secure_boot:false
       ~software_version:(Xapi_globs.software_version ())
       ~https_only:false ~max_cstate:"" ~ntp_mode:`Factory
       ~ntp_custom_servers:[] ~timezone:"UTC"
       ~numa_affinity_policy:`default_policy
       ~latest_synced_updates_applied:`unknown ~pending_guidances_full:[]
       ~pending_guidances_recommended:[] ~gpu_partitioning_status:`not_reported
       ~gpu_partitioning_reason:""
    )

(* Creates an unlicensed pool with the maximum number of hosts *)
let setup_test () =
  (* Create an unlicensed pool *)
  let __context = make_test_database () in
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_restrictions ~__context ~self:pool
    ~value:(Features.to_assoc_list []) ;
  (* Add hosts until we're at the maximum unlicensed pool size *)
  while
    Db.Host.get_all ~__context |> List.length < Xapi_globs.restricted_pool_size
  do
    add_host __context "host"
  done ;
  Alcotest.(check int)
    "initial number of hosts" Xapi_globs.restricted_pool_size
    (Db.Host.get_all ~__context |> List.length) ;
  __context

let test_host_join_restriction () =
  let __context = setup_test () in
  (* Check adding one more is a failure *)
  Alcotest.check_raises "Should fail"
    (Api_errors.Server_error
       ( Api_errors.license_restriction
       , [Features.name_of_feature Features.Pool_size]
       )
    )
    (fun () -> ignore (add_host __context "badhost")) ;
  (* License the pool *)
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_restrictions ~__context ~self:pool
    ~value:(Features.to_assoc_list [Features.Pool_size]) ;
  (* Adding hosts should now work *)
  add_host __context "goodhost" ;
  Alcotest.(check int)
    "one added OK"
    (Xapi_globs.restricted_pool_size + 1)
    (Db.Host.get_all ~__context |> List.length)

(* Each row: the checks of the desired state, most specific first; a check
   after a failed one must not run. *)
let gpu_partitioning_desired (name, partitionable_gpu, pool_enabled, expected) =
  let test () =
    Alcotest.(check (result unit string))
      name expected
      Gpu.Host_partitioning.(
        desired ~partitionable_gpu ~pool_enabled |> Result.map_error token
      )
  in
  (name, `Quick, test)

let not_run () = Alcotest.fail "a check ran after an earlier one failed"

(* Each row: GPUs added in steps, the host's state written after each step. *)
let sync_gpu_partitioning (name, steps, reason) =
  let test () =
    let __context = make_test_database () in
    let host = Helpers.get_localhost ~__context in
    let add_gpu vendor_id =
      let pCI = make_pci ~__context ~host ~vendor_id () in
      ignore (make_pgpu ~__context ~pCI ~host () : API.ref_PGPU)
    in
    steps
    |> List.iter (fun gpus ->
        List.iter add_gpu gpus ;
        Xapi_host.sync_gpu_partitioning ~__context ~after_boot:false
    ) ;
    Alcotest.(check (pair string string))
      name ("inactive", reason)
      ( API.host_gpu_partitioning_status_to_string
          (Db.Host.get_gpu_partitioning_status ~__context ~self:host)
      , Db.Host.get_gpu_partitioning_reason ~__context ~self:host
      )
  in
  (name, `Quick, test)

let no_division_evidence =
  [`not_supported; `disabled; `unknown; `not_checked; `not_in_use]

let division_evidence =
  [`enable_on_reboot; `enabled; `disable_on_reboot; `possibly_enabled]

(* Each row: NVIDIA GPUs, one per value [before], on a host where partitioning
   is not in use; [after] is the value each reads after the start, if it
   changes *)
let partition_modes_at_start (name, after_boot, before, after) =
  let test () =
    let __context = make_test_database () in
    let host = Helpers.get_localhost ~__context in
    let gpus =
      List.map
        (fun partition_mode ->
          let pCI = make_pci ~__context ~host ~vendor_id:"10de" () in
          make_pgpu ~__context ~pCI ~host ~partition_mode ()
        )
        before
    in
    Xapi_host.sync_gpu_partitioning ~__context ~after_boot ;
    let names = List.map API.partition_mode_to_string in
    Alcotest.(check (list string))
      name
      (names (List.map (fun mode -> Option.value after ~default:mode) before))
      (names
         (List.map
            (fun self -> Db.PGPU.get_partition_mode ~__context ~self)
            gpus
         )
      )
  in
  (name, `Quick, test)

let test =
  ("test_host_join_restriction", `Quick, test_host_join_restriction)
  :: List.map gpu_partitioning_desired
       [
         ( "desired: no partitionable GPU"
         , Fun.const false
         , not_run
         , Error "no_partitionable_gpu"
         )
       ; ( "desired: a partitionable GPU, the pool switch off"
         , Fun.const true
         , Fun.const false
         , Error "pool_disabled"
         )
       ; ( "desired: a partitionable GPU, the pool switch on"
         , Fun.const true
         , Fun.const true
         , Ok ()
         )
       ]
  @ List.map sync_gpu_partitioning
      [
        ("sync: another vendor's GPU", [["8086"]], "no_partitionable_gpu")
      ; ("sync: an NVIDIA GPU", [["10de"]], "pool_disabled")
      ; ( "sync: an NVIDIA GPU added after a first sync"
        , [["8086"]; ["10de"]]
        , "pool_disabled"
        )
      ]
  @ List.map partition_modes_at_start
      [
        ( "sync: xapi restart, every GPU keeps its partition_mode"
        , false
        , no_division_evidence @ division_evidence
        , None
        )
      ; ( "sync: first start after a boot, GPUs showing no division read \
           not_in_use"
        , true
        , no_division_evidence
        , Some `not_in_use
        )
      ; ( "sync: first start after a boot, GPUs that may be divided read \
           possibly_enabled"
        , true
        , division_evidence
        , Some `possibly_enabled
        )
      ]
