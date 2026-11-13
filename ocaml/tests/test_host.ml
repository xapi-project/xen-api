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
