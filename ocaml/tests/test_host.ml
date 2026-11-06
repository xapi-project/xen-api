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
        Xapi_host.sync_gpu_partitioning ~__context
    ) ;
    Alcotest.(check (pair string string))
      name ("inactive", reason)
      ( API.host_gpu_partitioning_status_to_string
          (Db.Host.get_gpu_partitioning_status ~__context ~self:host)
      , Db.Host.get_gpu_partitioning_reason ~__context ~self:host
      )
  in
  (name, `Quick, test)

let release_step = function
  | Xapi_host.Key_off ->
      "key off"
  | Regimes_off ->
      "regimes off"
  | Key_on_left ->
      "key on, left"

(* Each row: the key (None: no lock object), whether xapi or xenopsd takes the
   lock, and the steps of the release in order. *)
let gpu_lock_release_steps (name, key, locking, steps) =
  let test () =
    Alcotest.(check (list string))
      name
      (List.map release_step steps)
      (List.map release_step (Xapi_host.gpu_lock_release_steps ~key ~locking))
  in
  (name, `Quick, test)

(* Runs [f] while xapi takes the GPU lock or not, as [engaged] says, and its
   switch file setting names a temporary file that exists or not; restores
   both, and returns whether xapi takes the lock after [f]. *)
let with_gpu_lock_regime ~engaged ~exists f =
  let set engaged =
    Xapi_pgpu.set_gpu_lock_engaged ~timeout:Mtime.Span.zero engaged
    |> Result.iter_error Alcotest.fail
  in
  let file = Filename.temp_file "gpu-lock-engaged" "" in
  let default = !Xapi_globs.gpu_lock_engaged_file in
  if not exists then Sys.remove file ;
  set engaged ;
  Xapi_globs.gpu_lock_engaged_file := file ;
  Fun.protect
    ~finally:(fun () ->
      Xapi_globs.gpu_lock_engaged_file := default ;
      set false ;
      if exists then Sys.remove file
    )
    (fun () -> f () |> Xapi_pgpu.gpu_lock_engaged)

(* Each row: whether xenopsd's switch file exists when xapi starts. *)
let gpu_lock_regime_at_start (name, exists) =
  let test () =
    with_gpu_lock_regime ~engaged:(not exists) ~exists
      Xapi_host.set_gpu_lock_regime_from_switch_file
    |> Alcotest.(check bool) name exists
  in
  (name, `Quick, test)

(* Each row: the stored status, whether xapi takes the GPU lock before, and
   xenopsd's answer to "stop taking it" (None: unreachable), then whether xapi
   takes the lock and the status written. xenopsd's switch file exists; the
   machine has no host GPU lock object. *)
let release_or_leave (name, status, before, answer, engaged, written) =
  let test () =
    let __context = make_test_database () in
    let self = Helpers.get_localhost ~__context in
    let xenopsd (_ : Rpc.call) =
      match answer with
      | Some now ->
          Rpc.success (Rpc.Bool now)
      | None ->
          failwith "xenopsd unreachable"
    in
    let queues = !Xapi_xenops_queue.queue_override in
    Db.Host.set_gpu_partitioning_status ~__context ~self ~value:status ;
    Xapi_xenops_queue.queue_override :=
      (!Xapi_globs.default_xenopsd, xenopsd) :: queues ;
    let after =
      Fun.protect
        ~finally:(fun () -> Xapi_xenops_queue.queue_override := queues)
        (fun () ->
          with_gpu_lock_regime ~engaged:before ~exists:true (fun () ->
              Xapi_host.sync_gpu_partitioning ~__context
          )
        )
    in
    Alcotest.(check (pair bool string))
      name (engaged, written)
      ( after
      , API.host_gpu_partitioning_status_to_string
          (Db.Host.get_gpu_partitioning_status ~__context ~self)
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
  @ List.map gpu_lock_release_steps
      Xapi_host.
        [
          ("release: no lock object, nothing locks", None, false, [])
        ; ("release: no lock object, a daemon locks", None, true, [Regimes_off])
        ; ( "release: a stale pending key"
          , Some Gpu_lock.Pending
          , false
          , [Key_off]
          )
        ; ( "release: key pending, a daemon locks"
          , Some Gpu_lock.Pending
          , true
          , [Key_off; Regimes_off]
          )
        ; ( "release: key on, nothing locks"
          , Some Gpu_lock.On
          , false
          , [Key_on_left]
          )
        ]
  @ List.map release_or_leave
      [
        ( "release: xenopsd agrees, so neither daemon takes the lock"
        , `inactive
        , true
        , Some false
        , false
        , "inactive"
        )
      ; ( "release: xenopsd still busy, so xapi keeps taking the lock"
        , `inactive
        , true
        , Some true
        , true
        , "inactive"
        )
      ; ( "release: xenopsd busy, so xapi's previous regime (off) comes back"
        , `inactive
        , false
        , Some true
        , false
        , "inactive"
        )
      ; ( "release: xenopsd unreachable, so xapi keeps taking the lock"
        , `not_reported
        , true
        , None
        , true
        , "inactive"
        )
      ; ( "leave: an active host keeps its locks while it leaves"
        , `active
        , true
        , Some false
        , true
        , "leaving"
        )
      ; ( "leave: an entering host leaves like an active one"
        , `entering
        , true
        , Some false
        , true
        , "leaving"
        )
      ; ( "leave: a leaving host keeps its locks"
        , `leaving
        , true
        , Some false
        , true
        , "leaving"
        )
      ]
  @ List.map gpu_lock_regime_at_start
      [
        ("start: the switch file exists, so xapi takes the lock", true)
      ; ("start: no switch file, so xapi does not take the lock", false)
      ]
