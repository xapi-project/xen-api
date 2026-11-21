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

let release_step = function
  | Xapi_host.Turn_key_off ->
      "turn key off"
  | Disengage ->
      "disengage"
  | Warn_key_on ->
      "warn key on"

(* Each row: the key (None: no lock object), whether xapi or xenopsd takes the
   lock, and the steps of the release in order. *)
let gpu_lock_release_steps (name, key, engaged, steps) =
  let test () =
    Alcotest.(check (list string))
      name
      (List.map release_step steps)
      (List.map release_step (Xapi_host.gpu_lock_release_steps ~key ~engaged))
  in
  (name, `Quick, test)

(* Runs [f] while xapi takes the GPU lock or not, as [engaged] says, and its
   engaged-file setting names a temporary file that exists or not; restores
   both, and returns whether xapi takes the lock after [f]. *)
let with_gpu_lock_engaged ~engaged ~exists f =
  let set engaged =
    Xapi_pgpu.set_gpu_lock_engaged ~timeout:Mtime.Span.zero engaged
    |> Result.iter_error Alcotest.fail
  in
  let file = Filename.temp_file "gpu-lock-engaged" "" in
  let default = !Xapi_globs.gpu_lock_engaged_file in
  if not exists then Sys.remove file ;
  set engaged ;
  Xapi_globs.gpu_lock_engaged_file := file ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f () |> Xapi_pgpu.gpu_lock_engaged)
    (fun () ->
      Xapi_globs.gpu_lock_engaged_file := default ;
      set false ;
      if exists then Sys.remove file
    )

(* Each row: whether xenopsd's engaged file exists when xapi starts. *)
let gpu_lock_engaged_at_start (name, exists) =
  let test () =
    with_gpu_lock_engaged ~engaged:(not exists) ~exists
      Xapi_host.set_gpu_lock_engaged_at_start
    |> Alcotest.(check bool) name exists
  in
  (name, `Quick, test)

(* The release acts on this machine's host GPU lock: a test that finds one
   fails rather than change it *)
let no_host_gpu_lock () =
  match Gpu_lock.Gate.read_key Xapi_pgpu.gpu_lock ~timeout:Mtime.Span.zero with
  | Ok None ->
      ()
  | Ok (Some (Gpu_lock.Pending | On | Off)) | Error (_ : Gpu_lock.refusal) ->
      Alcotest.fail "needs a machine without a host GPU lock object"

(* Each row: the stored status, whether xapi takes the GPU lock before and
   xenopsd's engaged file exists, and xenopsd's answer to "stop taking it"
   (None: the call fails); then whether xapi takes the lock, whether xapi
   took it at each call to xenopsd, and the status written. *)
let release_or_leave
    (name, status, before, exists, answer, engaged, asked, written) =
  let test () =
    no_host_gpu_lock () ;
    let __context = make_test_database () in
    let self = Helpers.get_localhost ~__context in
    (* a host stored as partitioning may hold MIG state: its NVIDIA GPU is
       divided into partitions *)
    ( match status with
    | `entering | `active | `leaving ->
        let pCI = make_pci ~__context ~host:self ~vendor_id:"10de" () in
        let pgpu = make_pgpu ~__context ~pCI ~host:self () in
        Db.PGPU.set_partition_mode ~__context ~self:pgpu ~value:`enabled
    | `not_reported | `inactive ->
        ()
    ) ;
    let calls = ref [] in
    let xenopsd (_ : Rpc.call) =
      calls := Xapi_pgpu.gpu_lock_engaged () :: !calls ;
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
          with_gpu_lock_engaged ~engaged:before ~exists (fun () ->
              Xapi_host.sync_gpu_partitioning ~__context ~after_boot:false
          )
        )
    in
    Alcotest.(check (triple bool (list bool) string))
      name (engaged, asked, written)
      ( after
      , List.rev !calls
      , API.host_gpu_partitioning_status_to_string
          (Db.Host.get_gpu_partitioning_status ~__context ~self)
      )
  in
  (name, `Quick, test)

(* The pool's GPU partitioning setting and each host's call to apply it *)

let gpu_partitioning ~__context =
  Db.Pool.get_gpu_partitioning ~__context ~self:(Helpers.get_pool ~__context)
  |> API.pool_gpu_partitioning_to_string

let set_status ~__context value =
  List.iter
    (fun self -> Db.Host.set_gpu_partitioning_status ~__context ~self ~value)
    (Db.Host.get_all ~__context)

let mark_rolling_upgrade ~__context =
  Db.Pool.add_to_other_config ~__context
    ~self:(Helpers.get_pool ~__context)
    ~key:Xapi_globs.rolling_upgrade_in_progress ~value:"true"

(* Runs the pool's setter with each host's call answered by [apply]; returns
   the hosts called, in order, and the setting each saw stored *)
let set_gpu_partitioning ?(apply = fun (_ : API.ref_host) -> ()) ~__context
    value =
  let calls = ref [] in
  let rpc call =
    match (call.Rpc.name, call.Rpc.params) with
    | "host.apply_gpu_partitioning", [_session_id; host_rpc] ->
        let host = API.ref_host_of_rpc host_rpc in
        calls := (host, gpu_partitioning ~__context) :: !calls ;
        apply host ;
        Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
    | _ ->
        Mock_rpc.rpc __context call
  in
  Context.set_test_rpc __context rpc ;
  Xapi_pool.set_gpu_partitioning ~__context
    ~self:(Helpers.get_pool ~__context)
    ~value ;
  List.rev !calls

let store ?apply ~__context value =
  let (_ : (API.ref_host * string) list) =
    set_gpu_partitioning ?apply ~__context value
  in
  ()

let refused name code f =
  match f () with
  | (_ : (API.ref_host * string) list) ->
      Alcotest.failf "%s: accepted" name
  | exception Api_errors.Server_error (actual, _) ->
      Alcotest.(check string) name code actual

let test_set_enabled_stored () =
  let __context = make_test_database () in
  set_status ~__context `inactive ;
  let host = Helpers.get_localhost ~__context in
  let calls = set_gpu_partitioning ~__context `enabled in
  Alcotest.(check (list (pair string string)))
    "the host is called after the store"
    [(Ref.string_of host, "enabled")]
    (List.map (fun (h, seen) -> (Ref.string_of h, seen)) calls)

let test_set_enabled_refused_in_rolling_upgrade () =
  let __context = make_test_database () in
  set_status ~__context `inactive ;
  mark_rolling_upgrade ~__context ;
  refused "upgrade marked" Api_errors.not_supported_during_upgrade (fun () ->
      set_gpu_partitioning ~__context `enabled
  ) ;
  Alcotest.(check string)
    "unchanged" "default_policy"
    (gpu_partitioning ~__context)

(* No upgrade mark: a pool operator can remove it by hand *)
let test_set_enabled_refused_versions_differ () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  set_host_version ~__context ~host ~key:Xapi_globs._platform_version "99.0.0" ;
  set_status ~__context `inactive ;
  refused "versions differ" Api_errors.not_supported_during_upgrade (fun () ->
      set_gpu_partitioning ~__context `enabled
  )

let test_set_enabled_refused_not_reported () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  Alcotest.check_raises "the host is named"
    Api_errors.(
      Server_error (gpu_partitioning_status_not_reported, [Ref.string_of host])
    )
    (fun () -> store ~__context `enabled) ;
  Alcotest.(check string)
    "unchanged" "default_policy"
    (gpu_partitioning ~__context)

(* The license is checked first: the host has not reported and an upgrade is
   marked too *)
let test_set_enabled_refused_unlicensed () =
  let __context = make_test_database ~features:[] () in
  mark_rolling_upgrade ~__context ;
  refused "unlicensed" Api_errors.feature_restricted (fun () ->
      set_gpu_partitioning ~__context `enabled
  ) ;
  Alcotest.(check string)
    "unchanged" "default_policy"
    (gpu_partitioning ~__context)

(* The upgrade is checked before the hosts' reports: during an upgrade from a
   version that does not report GPU partitioning, the older hosts read
   not_reported *)
let test_set_enabled_refused_upgrade_before_not_reported () =
  let __context = make_test_database () in
  mark_rolling_upgrade ~__context ;
  refused "upgrade first" Api_errors.not_supported_during_upgrade (fun () ->
      set_gpu_partitioning ~__context `enabled
  )

let test_set_disabled_accepted_in_rolling_upgrade () =
  let __context = make_test_database () in
  mark_rolling_upgrade ~__context ;
  store ~__context `disabled ;
  Alcotest.(check string) "stored" "disabled" (gpu_partitioning ~__context)

(* The license is checked only for enabled: a pool whose license has lapsed
   can still switch GPU partitioning off *)
let test_set_disabled_accepted_unlicensed () =
  let __context = make_test_database ~features:[] () in
  store ~__context `disabled ;
  Alcotest.(check string) "stored" "disabled" (gpu_partitioning ~__context)

let test_set_default_policy_accepted_not_reported () =
  let __context = make_test_database () in
  set_status ~__context `inactive ;
  store ~__context `enabled ;
  Db.Host.set_gpu_partitioning_status ~__context
    ~self:(Helpers.get_localhost ~__context)
    ~value:`not_reported ;
  store ~__context `default_policy ;
  Alcotest.(check string) "stored" "default_policy" (gpu_partitioning ~__context)

(* A repeated call with the same value makes every host apply it again *)
let test_set_enabled_twice () =
  let __context = make_test_database () in
  set_status ~__context `inactive ;
  let first = set_gpu_partitioning ~__context `enabled in
  let second = set_gpu_partitioning ~__context `enabled in
  Alcotest.(check (pair int int))
    "every host called each time" (1, 1)
    (List.length first, List.length second)

(* The answer of a host whose evaluation is still running *)
let busy host =
  raise
    Api_errors.(
      Server_error (other_operation_in_progress, ["host"; Ref.string_of host])
    )

(* [fail] is the failing host's answer *)
let test_set_failed_host_named fail () =
  let __context = make_test_database () in
  let failing = make_host ~__context () in
  let apply host = if host = failing then fail host in
  Alcotest.check_raises "the failed host is named"
    Api_errors.(
      Server_error
        (set_gpu_partitioning_partially_failed, [Ref.string_of failing])
    )
    (fun () -> store ~apply ~__context `disabled) ;
  Alcotest.(check string) "stored" "disabled" (gpu_partitioning ~__context)

(* An offline host, or one of a version without the call, is not a failure:
   the first applies the stored value at its next start, the second has
   nothing to apply until it is upgraded *)
let test_set_host_skipped code () =
  let __context = make_test_database () in
  let later = make_host ~__context () in
  let apply host =
    if host = later then
      raise Api_errors.(Server_error (code, [Ref.string_of host]))
  in
  store ~apply ~__context `disabled ;
  Alcotest.(check string) "stored" "disabled" (gpu_partitioning ~__context)

(* With an NVIDIA GPU, the reason the host's evaluation writes. The evaluation
   releases the GPU lock, so it runs as in the release cases: away from this
   machine's GPU lock and xenopsd *)
let nvidia_host_reason ~__context ?partition_mode evaluate =
  no_host_gpu_lock () ;
  let host = Helpers.get_localhost ~__context in
  let pCI = make_pci ~__context ~host ~vendor_id:"10de" () in
  let (_ : API.ref_PGPU) = make_pgpu ~__context ~pCI ~host ?partition_mode () in
  let (_ : bool) =
    with_gpu_lock_engaged ~engaged:false ~exists:false evaluate
  in
  Db.Host.get_gpu_partitioning_reason ~__context ~self:host

let sync_reason ~__context () =
  nvidia_host_reason ~__context (fun () ->
      Xapi_host.sync_gpu_partitioning ~__context ~after_boot:false
  )

(* Until the evaluation reads the pool's setting, enabled changes nothing *)
let test_sync_pool_enabled_still_pool_disabled () =
  let __context = make_test_database () in
  Db.Pool.set_gpu_partitioning ~__context
    ~self:(Helpers.get_pool ~__context)
    ~value:`enabled ;
  sync_reason ~__context () |> Alcotest.(check string) "reason" "pool_disabled"

let test_apply_leg_evaluates () =
  let __context = make_test_database () in
  nvidia_host_reason ~__context (fun () ->
      Xapi_host.apply_gpu_partitioning ~__context
        ~host:(Helpers.get_localhost ~__context)
  )
  |> Alcotest.(check string) "reason" "pool_disabled"

(* The call evaluates as a xapi restart does, not as the first start after a
   boot: the GPU keeps its partition_mode *)
let test_apply_keeps_partition_mode () =
  let __context = make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let (_ : string) =
    nvidia_host_reason ~__context ~partition_mode:`enabled (fun () ->
        Xapi_host.apply_gpu_partitioning ~__context ~host
    )
  in
  Db.Host.get_PGPUs ~__context ~self:host
  |> List.map (fun self ->
      API.partition_mode_to_string (Db.PGPU.get_partition_mode ~__context ~self)
  )
  |> Alcotest.(check (list string)) "kept" ["enabled"]

let test_apply_leg_lock_held () =
  let __context = make_test_database () in
  let self = Helpers.get_localhost ~__context in
  Xapi_stdext_threads.Threadext.Mutex.execute Xapi_host.gpu_partitioning_m
    (fun () ->
      Alcotest.check_raises "busy"
        Api_errors.(
          Server_error
            (other_operation_in_progress, ["host"; Ref.string_of self])
        )
        (fun () ->
          Xapi_host.apply_gpu_partitioning_within ~__context
            ~timeout:Mtime.Span.(20 * ms)
        )
  ) ;
  Alcotest.(check string)
    "unchanged" "not_reported"
    (API.host_gpu_partitioning_status_to_string
       (Db.Host.get_gpu_partitioning_status ~__context ~self)
    )

let gpu_partitioning_tests =
  [
    ( "set: enabled is stored, then the host is called"
    , `Quick
    , test_set_enabled_stored
    )
  ; ( "set: enabled is refused while a rolling pool upgrade is marked"
    , `Quick
    , test_set_enabled_refused_in_rolling_upgrade
    )
  ; ( "set: no upgrade mark, versions differ: enabled is refused"
    , `Quick
    , test_set_enabled_refused_versions_differ
    )
  ; ( "set: enabled is refused while a host has not reported"
    , `Quick
    , test_set_enabled_refused_not_reported
    )
  ; ( "set: enabled is refused on an unlicensed pool, before the other checks"
    , `Quick
    , test_set_enabled_refused_unlicensed
    )
  ; ( "set: enabled is refused while an upgrade is marked, before the \
       not-reported check"
    , `Quick
    , test_set_enabled_refused_upgrade_before_not_reported
    )
  ; ( "set: disabled is accepted during a rolling pool upgrade"
    , `Quick
    , test_set_disabled_accepted_in_rolling_upgrade
    )
  ; ( "set: disabled is accepted on an unlicensed pool"
    , `Quick
    , test_set_disabled_accepted_unlicensed
    )
  ; ( "set: default_policy is accepted while a host has not reported"
    , `Quick
    , test_set_default_policy_accepted_not_reported
    )
  ; ( "set: enabled twice, every host applies it each time"
    , `Quick
    , test_set_enabled_twice
    )
  ; ( "set: a host that fails is named, and the value stays stored"
    , `Quick
    , test_set_failed_host_named (fun _ -> failwith "unreachable")
    )
  ; ( "set: a busy host is named, not left to its next start"
    , `Quick
    , test_set_failed_host_named busy
    )
  ; ( "set: an offline host is left to its next start"
    , `Quick
    , test_set_host_skipped Api_errors.host_offline
    )
  ; ( "set: a host of a version without the call is not a failure"
    , `Quick
    , test_set_host_skipped Api_errors.message_method_unknown
    )
  ; ( "sync: the pool's setting enabled, an NVIDIA GPU still reads pool_disabled"
    , `Quick
    , test_sync_pool_enabled_still_pool_disabled
    )
  ; ("apply: the host's evaluation runs", `Quick, test_apply_leg_evaluates)
  ; ( "apply: the GPU keeps its partition_mode, as at a xapi restart"
    , `Quick
    , test_apply_keeps_partition_mode
    )
  ; ( "apply: the lock held too long, it fails and changes nothing"
    , `Quick
    , test_apply_leg_lock_held
    )
  ]

let test =
  ("test_host_join_restriction", `Quick, test_host_join_restriction)
  :: List.map sync_gpu_partitioning
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
  @ List.map gpu_lock_release_steps
      Xapi_host.
        [
          ("release: no lock object, nothing locks", None, false, [])
        ; ("release: no lock object, a daemon locks", None, true, [Disengage])
        ; ("release: key off, nothing locks", Some Gpu_lock.Off, false, [])
        ; ( "release: key off, a daemon locks"
          , Some Gpu_lock.Off
          , true
          , [Disengage]
          )
        ; ( "release: a stale pending key"
          , Some Gpu_lock.Pending
          , false
          , [Turn_key_off]
          )
        ; ( "release: key pending, a daemon locks"
          , Some Gpu_lock.Pending
          , true
          , [Turn_key_off; Disengage]
          )
        ; ( "release: key on, a daemon locks"
          , Some Gpu_lock.On
          , true
          , [Turn_key_off; Disengage]
          )
        ; ( "release: key on, nothing locks"
          , Some Gpu_lock.On
          , false
          , [Warn_key_on]
          )
        ]
  @ List.map release_or_leave
      [
        ( "release: xenopsd agrees, so neither daemon takes the lock"
        , `inactive
        , true
        , true
        , Some false
        , false
        , [false]
        , "inactive"
        )
      ; ( "release: xenopsd still busy, so xapi keeps taking the lock"
        , `inactive
        , true
        , true
        , Some true
        , true
        , [false]
        , "inactive"
        )
      ; ( "release: xenopsd busy, so xapi's change is undone (it was off)"
        , `inactive
        , false
        , true
        , Some true
        , false
        , [false]
        , "inactive"
        )
      ; ( "release: the call to xenopsd fails, so xapi keeps taking the lock"
        , `not_reported
        , true
        , true
        , None
        , true
        , [false]
        , "inactive"
        )
      ; ( "release: nothing engaged, so xenopsd is not asked"
        , `inactive
        , false
        , false
        , Some false
        , false
        , []
        , "inactive"
        )
      ; ( "release: only xenopsd's file, so xenopsd is asked"
        , `inactive
        , false
        , true
        , Some false
        , false
        , [false]
        , "inactive"
        )
      ; ( "release: only xapi engaged, so both stop"
        , `inactive
        , true
        , false
        , Some false
        , false
        , [false]
        , "inactive"
        )
      ; ( "leave: an active host keeps its locks while it leaves"
        , `active
        , true
        , true
        , Some false
        , true
        , []
        , "leaving"
        )
      ; ( "leave: an entering host leaves like an active one"
        , `entering
        , true
        , true
        , Some false
        , true
        , []
        , "leaving"
        )
      ; ( "leave: a leaving host keeps its locks"
        , `leaving
        , true
        , true
        , Some false
        , true
        , []
        , "leaving"
        )
      ]
  @ List.map gpu_lock_engaged_at_start
      [
        ("start: the engaged file exists, so xapi takes the lock", true)
      ; ("start: no engaged file, so xapi does not take the lock", false)
      ]
  @ gpu_partitioning_tests
