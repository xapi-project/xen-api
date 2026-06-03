(*
 * Copyright (C) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = "xapi_caller" end)

open D
module Rate_limit = Rate_limit_lib.Rate_limit
module Caller_table = Rate_limit_lib.Caller_table
module Caller_statistics = Rate_limit_lib.Caller_statistics

(** A single in-memory caller_table entry. The pattern_key is the table's
    primary key; [caller_ref] records which DB row this entry mirrors;
    [stats] tracks call counts and token use since startup;
    [rate_limit_ref] points at the rate-limit row (Ref.null when none),
    resolved to a live bucket via [Xapi_rate_limit.find_bucket] at dispatch
    time. *)
type entry = {
    caller_ref: API.ref_Caller
  ; pattern_key: Caller_table.Key.pattern_key
  ; stats: Caller_statistics.t
  ; rate_limit_ref: API.ref_Rate_limit
}

let caller_table : entry Caller_table.t = Caller_table.create ()

(* Serialises the read-then-create path that auto-populates unknown callers. *)
let create_mutex = Mutex.create ()

let pattern_of_db_string : string -> Caller_table.Key.match_pattern =
 fun s ->
  let len = String.length s in
  if len = 0 then
    Caller_table.Key.Prefix ""
  else if s.[len - 1] = '*' then
    Caller_table.Key.Prefix (String.sub s 0 (len - 1))
  else
    Caller_table.Key.Full s

let pattern_key_of_record (record : API.caller_t) : Caller_table.Key.pattern_key
    =
  Caller_table.Key.
    {
      user_agent_pattern= pattern_of_db_string record.caller_user_agent
    ; client_ip_pattern= pattern_of_db_string record.caller_client_ip
    }

let pattern_key_of_fields ~user_agent ~client_ip : Caller_table.Key.pattern_key
    =
  Caller_table.Key.
    {
      user_agent_pattern= pattern_of_db_string user_agent
    ; client_ip_pattern= pattern_of_db_string client_ip
    }

let target_of_request ~user_agent ~client_ip : Caller_table.Key.t =
  Caller_table.Key.{user_agent; client_ip}

(** A pattern is "fully specified" when neither field is a wildcard prefix.
    Auto-create only triggers if no fully-specified match is found. *)
let pattern_fully_specified
    ({user_agent_pattern; client_ip_pattern} : Caller_table.Key.pattern_key) =
  let open Caller_table.Key in
  let field_full = function Full _ -> true | Prefix _ -> false in
  field_full user_agent_pattern && field_full client_ip_pattern

let any_fully_specified entries =
  List.exists (fun e -> pattern_fully_specified e.pattern_key) entries

let validate_request_fields ~user_agent ~client_ip =
  if user_agent = "" && client_ip = "" then
    raise
      Api_errors.(
        Server_error
          ( invalid_value
          , [
              "user_agent/client_ip"
            ; "at least one of user_agent or client_ip must be set"
            ]
          )
      )

let insert_entry ~caller_ref ~caller_uuid ~pattern_key ~rate_limit_ref =
  let entry =
    {
      caller_ref
    ; pattern_key
    ; stats= Caller_statistics.create ~caller_uuid
    ; rate_limit_ref
    }
  in
  if not (Caller_table.insert caller_table ~pattern:pattern_key entry) then
    debug
      "Caller_table.insert refused entry (duplicate or all-wildcard) for \
       caller %s"
      (Ref.string_of caller_ref)

let create ~__context ~name_label ~name_description ~user_agent ~client_ip =
  validate_request_fields ~user_agent ~client_ip ;
  let pattern_key = pattern_key_of_fields ~user_agent ~client_ip in
  if Caller_table.Key.is_all_wildcard pattern_key then
    raise
      Api_errors.(
        Server_error
          ( invalid_value
          , ["user_agent/client_ip"; "all-wildcard pattern not allowed"]
          )
      ) ;
  match Caller_table.get_exact caller_table ~pattern:pattern_key with
  | Some entry ->
      (* Idempotent: an in-memory entry already mirrors this pattern. Update
         the DB-side name fields and return the existing ref. *)
      Db.Caller.set_name_label ~__context ~self:entry.caller_ref
        ~value:name_label ;
      Db.Caller.set_name_description ~__context ~self:entry.caller_ref
        ~value:name_description ;
      entry.caller_ref
  | None ->
      let uuid = Uuidx.(to_string (make () : [`Caller] t)) in
      let ref = Ref.make () in
      Db.Caller.create ~__context ~ref ~uuid ~name_label ~name_description
        ~user_agent ~client_ip ~last_access:Clock.Date.epoch ~groups:[]
        ~rate_limit:Ref.null ;
      insert_entry ~caller_ref:ref ~caller_uuid:uuid ~pattern_key
        ~rate_limit_ref:Ref.null ;
      ref

let destroy ~__context ~self =
  let record = Db.Caller.get_record ~__context ~self in
  let pattern_key = pattern_key_of_record record in
  Caller_table.delete caller_table ~pattern:pattern_key ;
  Db.Caller.destroy ~__context ~self

let add_group ~__context ~self ~group =
  Db.Caller.add_groups ~__context ~self ~value:group

let remove_group ~__context ~self ~group =
  Db.Caller.remove_groups ~__context ~self ~value:group

let entries_of_table () = Caller_table.to_list caller_table |> List.map snd

let find_entry_by_ref self =
  entries_of_table () |> List.find_opt (fun entry -> entry.caller_ref = self)

let query_token_usage ~__context:_ ~self =
  match find_entry_by_ref self with
  | None ->
      0.0
  | Some entry ->
      Caller_statistics.get_token_count entry.stats

let query_call_count ~__context:_ ~self =
  match find_entry_by_ref self with
  | None ->
      0L
  | Some entry ->
      Int64.of_int (Caller_statistics.get_call_count entry.stats)

(* Entries for every caller currently assigned to [group]. Raises if the group
   name is empty. *)
let group_entries ~__context ~group =
  if group = "" then
    raise
      Api_errors.(Server_error (invalid_value, ["group"; "empty group name"])) ;
  let in_group entry =
    try List.mem group (Db.Caller.get_groups ~__context ~self:entry.caller_ref)
    with _ -> false
  in
  entries_of_table () |> List.filter in_group

let query_group_token_usage ~__context ~group =
  group_entries ~__context ~group
  |> List.fold_left
       (fun tot entry -> tot +. Caller_statistics.get_token_count entry.stats)
       0.0

let query_group_call_count ~__context ~group =
  group_entries ~__context ~group
  |> List.fold_left
       (fun tot entry ->
         Int64.add tot
           (Int64.of_int (Caller_statistics.get_call_count entry.stats))
       )
       0L

let query_all_usage ~__context =
  entries_of_table ()
  |> List.filter_map (fun entry ->
      let tokens = Caller_statistics.get_token_count entry.stats in
      let calls = float_of_int (Caller_statistics.get_call_count entry.stats) in
      let uuid, name_label =
        try
          let record = Db.Caller.get_record ~__context ~self:entry.caller_ref in
          (record.API.caller_uuid, record.API.caller_name_label)
        with _ -> (Caller_statistics.get_uuid entry.stats, "")
      in
      Some (uuid, name_label, tokens, calls)
  )
  |> List.sort (fun (_, _, t1, _) (_, _, t2, _) -> compare t2 t1)
  |> List.map (fun (uuid, name_label, tokens, calls) ->
      [
        uuid
      ; name_label
      ; Printf.sprintf "%.3f" tokens
      ; Printf.sprintf "%.0f" calls
      ]
  )

(** Re-read the caller's rate_limit ref from DB and rebuild its entry. Called
    by Xapi_rate_limit whenever the caller's rate_limit field changes. *)
let refresh_caller_rate_limit ~__context caller_ref =
  match
    try Some (Db.Caller.get_record ~__context ~self:caller_ref) with _ -> None
  with
  | None ->
      ()
  | Some record ->
      let pattern_key = pattern_key_of_record record in
      Caller_table.delete caller_table ~pattern:pattern_key ;
      insert_entry ~caller_ref ~pattern_key
        ~rate_limit_ref:record.API.caller_rate_limit
        ~caller_uuid:record.caller_uuid

(* One token corresponds to a cheap DB read - expensive services are multiples *)
let token_costs =
  Hashtbl.of_seq
    (List.to_seq
       [
         ("VDI.pool_migrate", 2500.)
       ; ("VM.migrate_send", 2000.)
       ; ("VM.suspend", 400.)
       ; ("VM.resume_on", 400.)
       ; ("SR.probe", 400.)
       ; ("VM.copy", 300.)
       ; ("pool.enable_ha", 300.)
       ; ("VM.checkpoint", 200.)
       ; ("host.ha_join_liveset", 200.)
       ; ("Cluster.pool_create", 200.)
       ; ("VDI.copy", 200.)
       ; ("VM.pool_migrate", 200.)
       ; ("VM.resume", 200.)
       ; ("SR.destroy", 200.)
       ; ("Cluster_host.create", 150.)
       ; ("event.from", 150.)
       ; ("pool.management_reconfigure", 100.)
       ; ("pool.join", 100.)
       ; ("pool.disable_ha", 100.)
       ; ("host.prepare_for_poweroff", 100.)
       ; ("VM.set_memory_dynamic_range", 100.)
       ; ("host.evacuate", 75.)
       ; ("VM.clean_reboot", 70.)
       ; ("VM.restart_device_models", 70.)
       ; ("pool_update.apply", 70.)
       ; ("Bond.create", 60.)
       ; ("VM.clean_shutdown", 60.)
       ; ("VM.revert", 50.)
       ; ("host.install_server_certificate", 50.)
       ; ("pool.eject", 40.)
       ; ("Cluster.create", 40.)
       ; ("pool.sync_updates", 40.)
       ; ("host.apply_updates", 40.)
       ; ("SR.probe_ext", 40.)
       ; ("host.ha_wait_for_shutdown_via_statefile", 40.)
       ; ("pool_update.precheck", 30.)
       ; ("event.next", 30.)
       ; ("VDI.snapshot", 30.)
       ; ("pool_update.introduce", 30.)
       ; ("pool.enable_external_auth", 20.)
       ; ("VM.start_on", 20.)
       ; ("VM.hard_reboot", 20.)
       ; ("SR.create", 20.)
       ; ("VM.hard_shutdown", 20.)
       ; ("pool.designate_new_master", 20.)
       ; ("VM.start", 20.)
       ; ("VDI.clone", 20.)
       ; ("host.ha_release_resources", 15.)
       ; ("VM.snapshot", 15.)
       ; ("pool.is_slave", 15.)
       ; ("pool.recover_slaves", 15.)
       ; ("host.preconfigure_ha", 15.)
       ; ("pool_update.detach", 15.)
       ; ("pool_update.attach", 15.)
       ; ("host.update_master", 15.)
       ; ("PBD.plug", 15.)
       ; ("Repository.apply", 12.)
       ; ("pool.emergency_reset_master", 12.)
       ; ("VBD.plug", 12.)
       ; ("host.commit_new_master", 12.)
       ; ("SR.scan", 10.)
       ; ("VBD.unplug", 10.)
       ; ("pool_update.pool_clean", 10.)
       ; ("VM.clone", 10.)
       ; ("VM.provision", 10.)
       ; ("PIF.reconfigure_ip", 10.)
       ; ("pool.create_VLAN_from_PIF", 8.)
       ; ("pool.apply_edition", 8.)
       ; ("pool.disable_external_auth", 7.)
       ; ("VM.pool_migrate_complete", 7.)
       ; ("host.call_plugin", 7.)
       ; ("VLAN.create", 6.)
       ; ("VDI.create", 6.)
       ; ("host.update_firewalld_service_status", 6.)
       ; ("VDI.destroy", 5.)
       ; ("VIF.plug", 5.)
       ; ("host.set_iscsi_iqn", 5.)
       ; ("SR.update", 5.)
       ; ("VDI.resize", 5.)
       ; ("host.management_reconfigure", 4.)
       ; ("VIF.unplug", 3.)
       ; ("host.set_https_only", 3.)
       ; ("PIF.plug", 3.)
       ; ("host.disable_external_auth", 3.)
       ; ("VDI.set_name_label", 3.)
       ; ("VDI.set_name_description", 3.)
       ; ("PIF.scan", 3.)
       ]
    )

let default_token_cost = 1.

let get_token_cost name =
  Option.value ~default:default_token_cost (Hashtbl.find_opt token_costs name)

let bookkeeping_and_bucket ~task_create ~user_agent ~client_ip ~cost =
  let target = target_of_request ~user_agent ~client_ip in
  let matches = Caller_table.get caller_table ~caller_id:target in
  List.iter
    (fun entry -> Caller_statistics.register_call ~token_amount:cost entry.stats)
    matches ;
  if matches <> [] then
    task_create (fun __context ->
        let now = Clock.Date.now () in
        List.iter
          (fun entry ->
            try
              Db.Caller.set_last_access ~__context ~self:entry.caller_ref
                ~value:now
            with e ->
              debug "Failed to update last_access for caller %s: %s"
                (Ref.string_of entry.caller_ref)
                (Printexc.to_string e)
          )
          matches
    ) ;
  let bucket =
    List.find_map
      (fun entry ->
        if entry.rate_limit_ref = Ref.null then
          None
        else
          Xapi_rate_limit.find_bucket entry.rate_limit_ref
      )
      matches
  in
  (matches, bucket)

let maybe_autocreate ~task_create ~user_agent ~client_ip ~existing =
  let fully_specified_request = user_agent <> "" && client_ip <> "" in
  if (not fully_specified_request) || any_fully_specified existing then
    ()
  else (
    Mutex.lock create_mutex ;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock create_mutex)
      (fun () ->
        let target = target_of_request ~user_agent ~client_ip in
        let existing = Caller_table.get caller_table ~caller_id:target in
        if any_fully_specified existing then
          ()
        else
          task_create (fun __context ->
              try
                let caller_ref =
                  create ~__context
                    ~name_label:
                      (Printf.sprintf "user_agent: %s, client_ip: %s" user_agent
                         client_ip
                      )
                    ~name_description:
                      (Printf.sprintf
                         "Autogenerated caller for user_agent %s, client_ip %s"
                         user_agent client_ip
                      )
                    ~user_agent ~client_ip
                in
                Db.Caller.set_last_access ~__context ~self:caller_ref
                  ~value:(Clock.Date.now ())
              with e ->
                warn "Auto-create of caller for (%s, %s) failed: %s" user_agent
                  client_ip (Printexc.to_string e)
          )
      )
  )

let submit ~submit_fn ~user_agent ~client_ip ~callback ~task_create amount =
  if not !Xapi_globs.rate_limit_enabled then
    callback ()
  else
    let matches, bucket =
      bookkeeping_and_bucket ~task_create ~user_agent ~client_ip ~cost:amount
    in
    maybe_autocreate ~task_create ~user_agent ~client_ip ~existing:matches ;
    match bucket with
    | Some rl ->
        submit_fn rl ~callback amount
    | None ->
        callback ()

let submit_sync ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_sync ~user_agent ~client_ip ~callback
    ~task_create amount

let submit_async ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_async ~user_agent ~client_ip ~callback
    ~task_create amount

(* We publish two derive data sources per known caller to xcp-rrdd: cumulative
   tokens consumed and cumulative call count. *)

let reporter_uid = "xapi-rate-limit-callers"

let make_caller_dss () =
  Caller_table.to_list caller_table
  |> List.concat_map (fun (_pattern, entry) ->
      let uuid = Caller_statistics.get_uuid entry.stats in
      let tokens = Caller_statistics.get_token_count entry.stats in
      let calls = Caller_statistics.get_call_count entry.stats in
      [
        ( Rrd.Host
        , Ds.ds_make
            ~name:(Printf.sprintf "caller_%s_tokens" uuid)
            ~description:
              (Printf.sprintf "Total tokens consumed by caller %s" uuid)
            ~value:(Rrd.VT_Float tokens) ~ty:Rrd.Derive ~default:true
            ~units:"tokens" ~min:0.0 ()
        )
      ; ( Rrd.Host
        , Ds.ds_make
            ~name:(Printf.sprintf "caller_%s_calls" uuid)
            ~description:(Printf.sprintf "Total calls by caller %s" uuid)
            ~value:(Rrd.VT_Int64 (Int64.of_int calls))
            ~ty:Rrd.Derive ~default:true ~units:"calls" ~min:0.0 ()
        )
      ]
  )

let reporter : Rrdd_plugin.Reporter.t option ref = ref None

let start_reporter () =
  try
    let r =
      Rrdd_plugin.Reporter.start_async
        (module D : Debug.DEBUG)
        ~uid:reporter_uid ~neg_shift:0.5 ~target:(Rrdd_plugin.Reporter.Local 1)
        ~protocol:Rrd_interface.V2 ~dss_f:make_caller_dss
    in
    reporter := Some r
  with e ->
    warn "Failed to start caller RRD reporter: %s" (Printexc.to_string e)

let register ~__context =
  if not !Xapi_globs.rate_limit_enabled then
    debug
      "Rate limiting disabled (rate_limit=false); skipping caller registration"
  else (
    Xapi_rate_limit.set_caller_refresh_callback refresh_caller_rate_limit ;
    List.iter
      (fun self ->
        let record = Db.Caller.get_record ~__context ~self in
        let pattern_key = pattern_key_of_record record in
        insert_entry ~caller_ref:self ~pattern_key
          ~rate_limit_ref:record.API.caller_rate_limit
          ~caller_uuid:record.caller_uuid
      )
      (Db.Caller.get_all ~__context) ;
    start_reporter ()
  )
