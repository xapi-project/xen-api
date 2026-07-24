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
module Config_file = Xcp_service.Config_file
module Unixext = Xapi_stdext_unix.Unixext

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

(* Serialises ALL mutations of [caller_table] on the master. Caller_table
   itself uses Atomic for lock-free reads, but its writers are non-CAS
   Atomic.get/set pairs and its higher-level "delete then insert" refresh
   is not atomic - two concurrent refreshes for the same caller can
   otherwise interleave so that the later insert is silently refused
   as a duplicate, leaving the table with stale state. Held around
   create/destroy/refresh_caller_rate_limit and the auto-create path.
   NOT held by [register] because that runs single-threaded at startup. *)
let caller_table_mutex = Mutex.create ()

let with_caller_table_mutex f =
  Mutex.lock caller_table_mutex ;
  Fun.protect ~finally:(fun () -> Mutex.unlock caller_table_mutex) f

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

(* All [insert_entry_locked] callers must hold [caller_table_mutex], except
   [register] which runs single-threaded at startup. *)
let insert_entry_locked ~caller_ref ~stats ~pattern_key ~rate_limit_ref =
  let entry = {caller_ref; pattern_key; stats; rate_limit_ref} in
  if not (Caller_table.insert caller_table ~pattern:pattern_key entry) then
    debug
      "Caller_table.insert refused entry (duplicate or all-wildcard) for \
       caller %s"
      (Ref.string_of caller_ref)

(* Body of [create]; assumes [caller_table_mutex] is held and that
   [pattern_key] has already been validated. *)
let create_locked ~__context ~name_label ~name_description ~user_agent
    ~client_ip ~pattern_key =
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
      insert_entry_locked ~caller_ref:ref
        ~stats:(Caller_statistics.create ~caller_uuid:uuid)
        ~pattern_key ~rate_limit_ref:Ref.null ;
      ref

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
  with_caller_table_mutex (fun () ->
      create_locked ~__context ~name_label ~name_description ~user_agent
        ~client_ip ~pattern_key
  )

let destroy ~__context ~self =
  let record = Db.Caller.get_record ~__context ~self in
  let pattern_key = pattern_key_of_record record in
  with_caller_table_mutex (fun () ->
      Caller_table.delete caller_table ~pattern:pattern_key
  ) ;
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
    by Xapi_rate_limit whenever the caller's rate_limit field changes.

    Held under [caller_table_mutex] so the DB read + delete + insert are
    seen as one step: concurrent refreshes for the same caller can
    otherwise both start from the DB state seen before either mutation,
    and the later insert then silently loses to the earlier one.

    User_agent and client_ip are StaticRO in the datamodel, so a caller's
    pattern_key never changes; we preserve the existing [stats] across
    the swap so that attaching or detaching a rate_limit doesn't reset
    the "calls / tokens since Xapi startup" counters. *)
let refresh_caller_rate_limit ~__context caller_ref =
  with_caller_table_mutex (fun () ->
      match
        try Some (Db.Caller.get_record ~__context ~self:caller_ref)
        with _ -> None
      with
      | None ->
          ()
      | Some record ->
          let pattern_key = pattern_key_of_record record in
          let stats =
            match Caller_table.get_exact caller_table ~pattern:pattern_key with
            | Some existing ->
                existing.stats
            | None ->
                Caller_statistics.create ~caller_uuid:record.caller_uuid
          in
          Caller_table.delete caller_table ~pattern:pattern_key ;
          insert_entry_locked ~caller_ref ~stats ~pattern_key
            ~rate_limit_ref:record.API.caller_rate_limit
  )

(* Install the caller_table refresh callback at module load time. The API
   server can accept requests before [register] runs, and any
   [Rate_limit.add_caller] that lands in that window would otherwise leave
   the caller_table entry with rate_limit_ref = Ref.null (because
   [notify_caller_changed] would fall through to the default no-op). *)
let () = Xapi_rate_limit.set_caller_refresh_callback refresh_caller_rate_limit

(* One token corresponds to a cheap DB read; expensive services cost multiples.
   The costs are loaded at startup from [Xapi_globs.call_costs_file], one
   "Class.method = cost" per line (key=value, '#' comments), so the values can be
   tweaked and new calls added without recompiling xapi. Calls without an entry
   fall back to [default_token_cost]. *)
let token_costs : (string, float) Hashtbl.t = Hashtbl.create 256

let default_token_cost = 1.

let add_cost_line line =
  match Config_file.parse_line line with
  | Some (name, value) -> (
    match float_of_string_opt (String.trim value) with
    | Some cost ->
        Hashtbl.replace token_costs name cost
    | None ->
        warn "Ignoring call cost for %s: %S is not a number" name value
  )
  | None ->
      ()

(* Reload [token_costs] from [path]. On any failure the table is left empty and
   every call falls back to [default_token_cost]. *)
let load_token_costs ?(path = !Xapi_globs.call_costs_file) () =
  Hashtbl.reset token_costs ;
  ( try Unixext.file_lines_iter add_cost_line path
    with e ->
      warn
        "Could not load call costs from %s (%s); all calls will use the \
         default cost of %g"
        path (Printexc.to_string e) default_token_cost
  ) ;
  debug "Loaded %d call costs from %s" (Hashtbl.length token_costs) path

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
  let entry_and_bucket =
    List.find_map
      (fun entry ->
        if entry.rate_limit_ref = Ref.null then
          None
        else
          Option.map
            (fun rl -> (entry, rl))
            (Xapi_rate_limit.find_bucket entry.rate_limit_ref)
      )
      matches
  in
  (matches, entry_and_bucket)

let maybe_autocreate ~task_create ~user_agent ~client_ip ~existing =
  let fully_specified_request = user_agent <> "" && client_ip <> "" in
  if (not fully_specified_request) || any_fully_specified existing then
    ()
  else
    task_create (fun __context ->
        with_caller_table_mutex (fun () ->
            (* Re-check under the lock: another thread may have auto-created
               a matching row while we were racing to acquire the mutex. *)
            let target = target_of_request ~user_agent ~client_ip in
            let existing = Caller_table.get caller_table ~caller_id:target in
            if any_fully_specified existing then
              ()
            else
              try
                let pattern_key =
                  pattern_key_of_fields ~user_agent ~client_ip
                in
                let caller_ref =
                  create_locked ~__context
                    ~name_label:
                      (Printf.sprintf "user_agent: %s, client_ip: %s" user_agent
                         client_ip
                      )
                    ~name_description:
                      (Printf.sprintf
                         "Autogenerated caller for user_agent %s, client_ip %s"
                         user_agent client_ip
                      )
                    ~user_agent ~client_ip ~pattern_key
                in
                Db.Caller.set_last_access ~__context ~self:caller_ref
                  ~value:(Clock.Date.now ())
              with e ->
                warn "Auto-create of caller for (%s, %s) failed: %s" user_agent
                  client_ip (Printexc.to_string e)
        )
    )

(* Build a [Rate_limit.delay_observer] that opens/closes an
   [xapi.rate_limit.delay] span the moment the caller is queued and the moment
   they are released. Only called when both a parent span and a bucket exist,
   so we always have something to attach the span to. Uses [Ref.string_of] on
   the in-memory entry rather than a DB round-trip to resolve UUIDs -
   traces stay cheap and self-contained. *)
let make_observer ~parent ~entry ~user_agent ~client_ip ~cost =
  let span : Tracing.Span.t option ref = ref None in
  let attributes =
    [
      ("xapi.rate_limit.user_agent", user_agent)
    ; ("xapi.rate_limit.client_ip", client_ip)
    ; ("xapi.rate_limit.caller", Ref.string_of entry.caller_ref)
    ; ("xapi.rate_limit.bucket", Ref.string_of entry.rate_limit_ref)
    ; ("xapi.rate_limit.cost", Printf.sprintf "%g" cost)
    ]
  in
  let tracer = Tracing.Tracer.get_tracer ~name:"xapi" in
  let on_start () =
    match
      Tracing.Tracer.start ~tracer ~name:"xapi.rate_limit.delay"
        ~parent:(Some parent) ~span_kind:Tracing.SpanKind.Internal ~attributes
        ()
    with
    | Ok s ->
        span := s
    | Error _ ->
        ()
  in
  let on_end () = ignore (Tracing.Tracer.finish !span) in
  {Rate_limit.on_start; on_end}

(* [submit_fn] MUST be called with [?observer:observer] at the call site: if we
   apply it with only [~callback amount], OCaml infers a type without the
   optional [?observer] and silently erases the parameter, so any observer we
   construct here would never fire. *)
let submit ~submit_fn ?parent ~user_agent ~client_ip ~callback ~task_create
    amount =
  if not !Xapi_globs.rate_limit_enabled then
    callback ()
  else
    let matches, entry_and_bucket =
      bookkeeping_and_bucket ~task_create ~user_agent ~client_ip ~cost:amount
    in
    maybe_autocreate ~task_create ~user_agent ~client_ip ~existing:matches ;
    match entry_and_bucket with
    | Some (entry, rl) ->
        let observer =
          Option.map
            (fun p ->
              make_observer ~parent:p ~entry ~user_agent ~client_ip ~cost:amount
            )
            parent
        in
        submit_fn rl ?observer ~callback amount
    | None ->
        callback ()

let submit_sync ?parent ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_sync ?parent ~user_agent ~client_ip
    ~callback ~task_create amount

let submit_async ?parent ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_async ?parent ~user_agent ~client_ip
    ~callback ~task_create amount

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
    load_token_costs () ;
    (* Runs single-threaded at start-of-day, so bypasses caller_table_mutex. *)
    List.iter
      (fun self ->
        let record = Db.Caller.get_record ~__context ~self in
        let pattern_key = pattern_key_of_record record in
        insert_entry_locked ~caller_ref:self
          ~stats:(Caller_statistics.create ~caller_uuid:record.caller_uuid)
          ~pattern_key ~rate_limit_ref:record.API.caller_rate_limit
      )
      (Db.Caller.get_all ~__context) ;
    start_reporter ()
  )
