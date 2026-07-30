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

(* Monotonically increasing logical clock, bumped once per matched call. Each
   entry records the value it last saw in [last_call]; the smallest value
   therefore identifies the caller with the least recent call. A single atomic
   fetch-and-add per call keeps the dispatch path lock-free - we deliberately
   avoid a real time source (and its dependency) here since only the relative
   ordering matters for eviction. *)
let call_sequence = Atomic.make 0

let next_call_sequence () = Atomic.fetch_and_add call_sequence 1

(** A single in-memory caller_table entry. The pattern_key is the table's
    primary key; [caller_ref] records which DB row this entry mirrors;
    [stats] tracks call counts and token use since startup;
    [rate_limit_ref] points at the rate-limit row (Ref.null when none),
    resolved to a live bucket via [Xapi_rate_limit.find_bucket] at dispatch
    time; [auto_registered] is true for callers created by [maybe_autocreate]
    rather than by an administrator; [last_call] is the [call_sequence] value
    seen on this caller's most recent call, used to pick the eviction victim;
    [groups] mirrors the caller's DB [groups] field so the RRD reporter can
    aggregate usage per group without touching the database. *)
type entry = {
    caller_ref: API.ref_Caller
  ; pattern_key: Caller_table.Key.pattern_key
  ; stats: Caller_statistics.t
  ; rate_limit_ref: API.ref_Rate_limit
  ; auto_registered: bool
  ; last_call: int Atomic.t
  ; groups: string list
}

let caller_table : entry Caller_table.t = Caller_table.create ()

(* Number of auto-registered entries currently in [caller_table]. Mutated only
   under [caller_table_mutex] (or single-threaded in [register]) so it stays in
   step with the table. Lets [maybe_autocreate] decide in O(1) whether the cap
   has been reached without scanning the table. *)
let auto_registered_count = ref 0

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
   [register] which runs single-threaded at startup. Does not touch
   [auto_registered_count]; the create/destroy helpers own that counter so a
   delete-then-insert refresh does not perturb it. A freshly inserted entry is
   stamped with the current [call_sequence] so a just-registered caller is
   treated as recently used rather than an immediate eviction candidate. *)
let insert_entry_locked ~caller_ref ~stats ~pattern_key ~rate_limit_ref
    ~auto_registered ~groups ?last_call () =
  let last_call =
    match last_call with
    | Some v ->
        v
    | None ->
        Atomic.make (next_call_sequence ())
  in
  let entry =
    {
      caller_ref
    ; pattern_key
    ; stats
    ; rate_limit_ref
    ; auto_registered
    ; last_call
    ; groups
    }
  in
  if not (Caller_table.insert caller_table ~pattern:pattern_key entry) then
    debug
      "Caller_table.insert refused entry (duplicate or all-wildcard) for \
       caller %s"
      (Ref.string_of caller_ref)

(* Promote an auto-registered entry to an administrator-owned one: an explicit
   admin action (creating a caller for the same pattern, or attaching a
   rate-limit rule) has taken ownership of it, so it should no longer count
   against the auto-registration cap or be a candidate for LRU eviction.
   Assumes [caller_table_mutex] is held and that [entry] is the live table entry
   for its pattern with [auto_registered = true]. Preserves the existing stats
   and recency stamp across the swap. *)
let promote_entry_locked ~__context entry =
  Db.Caller.set_auto_registered ~__context ~self:entry.caller_ref ~value:false ;
  decr auto_registered_count ;
  Caller_table.delete caller_table ~pattern:entry.pattern_key ;
  insert_entry_locked ~caller_ref:entry.caller_ref ~stats:entry.stats
    ~pattern_key:entry.pattern_key ~rate_limit_ref:entry.rate_limit_ref
    ~auto_registered:false ~groups:entry.groups ~last_call:entry.last_call ()

(* Body of [create]; assumes [caller_table_mutex] is held and that
   [pattern_key] has already been validated. [auto_registered] marks whether the
   new row is created by the rate limiter (subject to the cap) or by an
   administrator. *)
let create_locked ~__context ~name_label ~name_description ~user_agent
    ~client_ip ~pattern_key ~auto_registered =
  match Caller_table.get_exact caller_table ~pattern:pattern_key with
  | Some entry ->
      (* Idempotent: an in-memory entry already mirrors this pattern. Update
         the DB-side name fields and return the existing ref. *)
      Db.Caller.set_name_label ~__context ~self:entry.caller_ref
        ~value:name_label ;
      Db.Caller.set_name_description ~__context ~self:entry.caller_ref
        ~value:name_description ;
      (* An explicit admin [create] for a pattern already held by an
         auto-registered caller takes ownership of it. *)
      if (not auto_registered) && entry.auto_registered then
        promote_entry_locked ~__context entry ;
      entry.caller_ref
  | None ->
      let uuid = Uuidx.(to_string (make () : [`Caller] t)) in
      let ref = Ref.make () in
      Db.Caller.create ~__context ~ref ~uuid ~name_label ~name_description
        ~user_agent ~client_ip ~last_access:Clock.Date.epoch ~groups:[]
        ~rate_limit:Ref.null ~auto_registered ;
      insert_entry_locked ~caller_ref:ref
        ~stats:(Caller_statistics.create ~caller_uuid:uuid)
        ~pattern_key ~rate_limit_ref:Ref.null ~auto_registered ~groups:[] () ;
      if auto_registered then incr auto_registered_count ;
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
        ~client_ip ~pattern_key ~auto_registered:false
  )

(* Body of [destroy]; assumes [caller_table_mutex] is held. Also used by the
   eviction path. Keeps [auto_registered_count] in step with the table. *)
let destroy_locked ~__context ~self =
  let record = Db.Caller.get_record ~__context ~self in
  let pattern_key = pattern_key_of_record record in
  ( match Caller_table.get_exact caller_table ~pattern:pattern_key with
  | Some entry when entry.auto_registered ->
      decr auto_registered_count
  | _ ->
      ()
  ) ;
  Caller_table.delete caller_table ~pattern:pattern_key ;
  Db.Caller.destroy ~__context ~self

let destroy ~__context ~self =
  with_caller_table_mutex (fun () -> destroy_locked ~__context ~self)

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

(** Re-read the caller's record from DB and rebuild its in-memory entry. Called
    by Xapi_rate_limit whenever the caller's rate_limit field changes, and
    whenever its group membership changes.

    Held under [caller_table_mutex] so the DB read + delete + insert are
    seen as one step: concurrent refreshes for the same caller can
    otherwise both start from the DB state seen before either mutation,
    and the later insert then silently loses to the earlier one.

    User_agent and client_ip are StaticRO in the datamodel, so a caller's
    pattern_key never changes; we preserve the existing [stats] and recency
    stamp across the swap so that attaching or detaching a rate_limit (or
    changing groups) doesn't reset the "calls / tokens since Xapi startup"
    counters. The rate_limit ref and group membership are taken from the freshly
    read record; the auto-registered flag likewise, except that attaching a
    rate-limit rule to an auto-registered caller promotes it (see below). *)
let refresh_caller_entry ~__context caller_ref =
  with_caller_table_mutex (fun () ->
      match
        try Some (Db.Caller.get_record ~__context ~self:caller_ref)
        with _ -> None
      with
      | None ->
          ()
      | Some record ->
          let pattern_key = pattern_key_of_record record in
          let existing =
            Caller_table.get_exact caller_table ~pattern:pattern_key
          in
          let stats, last_call =
            match existing with
            | Some existing ->
                (existing.stats, Some existing.last_call)
            | None ->
                (Caller_statistics.create ~caller_uuid:record.caller_uuid, None)
          in
          (* Attaching a rate-limit rule is an explicit admin action: promote an
             auto-registered caller so it is no longer subject to the
             auto-registration cap or LRU eviction. This is the one refresh path
             where the flag can flip true -> false, so keep
             [auto_registered_count] in step here - both the delete below and
             [insert_entry_locked] leave the counter untouched. *)
          let promote =
            record.API.caller_auto_registered
            && record.API.caller_rate_limit <> Ref.null
          in
          let auto_registered =
            record.API.caller_auto_registered && not promote
          in
          if promote then (
            Db.Caller.set_auto_registered ~__context ~self:caller_ref
              ~value:false ;
            match existing with
            | Some e when e.auto_registered ->
                decr auto_registered_count
            | _ ->
                ()
          ) ;
          Caller_table.delete caller_table ~pattern:pattern_key ;
          insert_entry_locked ~caller_ref ~stats ~pattern_key
            ~rate_limit_ref:record.API.caller_rate_limit ~auto_registered
            ~groups:record.API.caller_groups ?last_call ()
  )

(* Group names are administrator-supplied and flow verbatim into RRD data source
   names (see [make_group_dss]), so constrain them at the point of entry:
   restrict to characters that are safe in a data source name, and bound the
   length so a single group cannot blow the reporter's per-data-source size
   estimate (see [reporter_bytes_per_ds]) and reintroduce the "not enough
   memory" failure. Rejecting bad names here (rather than mangling them at report
   time) also keeps names collision-free, so distinct groups never merge into one
   data source. *)
let max_group_name_length = 64

let valid_group_name_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' ->
      true
  | _ ->
      false

let validate_group_name group =
  let invalid reason =
    raise Api_errors.(Server_error (invalid_value, ["group"; reason]))
  in
  if group = "" then invalid "empty group name" ;
  if String.length group > max_group_name_length then
    invalid
      (Printf.sprintf "group name must be at most %d characters"
         max_group_name_length
      ) ;
  if not (String.for_all valid_group_name_char group) then
    invalid
      "group name may only contain alphanumerics and '_', '-' or '.' characters"

let add_group ~__context ~self ~group =
  validate_group_name group ;
  Db.Caller.add_groups ~__context ~self ~value:group ;
  (* Keep the in-memory entry's [groups] in step so the RRD reporter aggregates
     correctly. *)
  refresh_caller_entry ~__context self

let remove_group ~__context ~self ~group =
  Db.Caller.remove_groups ~__context ~self ~value:group ;
  refresh_caller_entry ~__context self

(* Install the caller_table refresh callback at module load time. The API
   server can accept requests before [register] runs, and any
   [Rate_limit.add_caller] that lands in that window would otherwise leave
   the caller_table entry with rate_limit_ref = Ref.null (because
   [notify_caller_changed] would fall through to the default no-op). *)
let () = Xapi_rate_limit.set_caller_refresh_callback refresh_caller_entry

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
  (* Stamp recency lock-free on the dispatch path: one atomic write per matched
     entry, using the same value for every match in this call. Read back by
     [evict_lru_auto_registered_locked] to pick the least recently used
     caller. *)
  let seq = next_call_sequence () in
  List.iter
    (fun entry ->
      Caller_statistics.register_call ~token_amount:cost entry.stats ;
      Atomic.set entry.last_call seq
    )
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

(* Drop the auto-registered caller with the least recent call. Assumes
   [caller_table_mutex] is held. Only ever scans the (bounded) in-memory table
   and reads recency from an atomic, so no DB reads are needed to choose the
   victim; the O(n) scan runs only on the rare "at capacity" auto-create. *)
let evict_lru_auto_registered_locked ~__context =
  let victim =
    entries_of_table ()
    |> List.filter (fun entry -> entry.auto_registered)
    |> List.fold_left
         (fun acc entry ->
           match acc with
           | Some best
             when Atomic.get best.last_call <= Atomic.get entry.last_call ->
               acc
           | _ ->
               Some entry
         )
         None
  in
  match victim with
  | None ->
      ()
  | Some entry ->
      debug
        "Auto-registered caller limit (%d) reached; evicting least recently \
         used caller %s"
        !Xapi_globs.max_auto_registered_callers
        (Ref.string_of entry.caller_ref) ;
      destroy_locked ~__context ~self:entry.caller_ref

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
            let limit = !Xapi_globs.max_auto_registered_callers in
            if any_fully_specified existing || limit = 0 then
              (* A limit of 0 disables auto-registration entirely. *)
              ()
            else
              try
                (* Enforce the cap before adding a new auto-registered caller.
                   A negative limit means unbounded, so no eviction. *)
                if limit > 0 && !auto_registered_count >= limit then
                  evict_lru_auto_registered_locked ~__context ;
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
                    ~user_agent ~client_ip ~pattern_key ~auto_registered:true
                in
                Db.Caller.set_last_access ~__context ~self:caller_ref
                  ~value:(Clock.Date.now ())
              with e ->
                warn "Auto-create of caller for (%s, %s) failed: %s" user_agent
                  client_ip (Printexc.to_string e)
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
        let caller_details =
          Printf.sprintf "client_ip: %s, user_agent: %s" client_ip user_agent
        in
        submit_fn rl ~callback ~caller_details amount
    | None ->
        callback ()

let submit_sync ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_sync ~user_agent ~client_ip ~callback
    ~task_create amount

let submit_async ~user_agent ~client_ip ~callback ~task_create amount =
  submit ~submit_fn:Rate_limit.submit_async ~user_agent ~client_ip ~callback
    ~task_create amount

(* We publish two derive data sources per caller group to xcp-rrdd: the group's
   cumulative tokens consumed and its cumulative call count, summed over the
   callers in that group. Reporting is per group rather than per caller, which
   both matches how usage is analysed and bounds the number of data sources
   (groups are administrator-defined, callers are not). Callers that belong to no
   group are not reported. *)

let reporter_uid = "xapi-rate-limit-groups"

(* Sizing of the reporter's local shared-memory payload. The V2 protocol writes,
   per data source, an 8-byte value plus its JSON metadata (name, description,
   units, ...); a group's name-based data source comes to a few hundred bytes, so
   512 bytes each is a safe over-estimate. Two data sources (tokens, calls) are
   published per group. A single fixed 4 KB page previously overflowed at ~8
   data sources, failing the writer with Failure "not enough memory". *)
let reporter_page_size = 4096

let reporter_bytes_per_ds = 512

let reporter_fixed_overhead = 256

(* Group names are administrator-defined and not bounded by any config, so size
   the reporter generously. The backing file is sparse - only pages actually
   written are committed - so an ample bound costs almost nothing, and the clamp
   in [make_group_dss] guarantees the payload never exceeds the allocation. *)
let reporter_max_groups = 1024

(* Pages needed for two data sources per group plus fixed protocol overhead. *)
let reporter_page_count max_groups =
  let bytes =
    reporter_fixed_overhead + (2 * max_groups * reporter_bytes_per_ds)
  in
  max 1 ((bytes + reporter_page_size - 1) / reporter_page_size)

(* Ceiling on data sources written per cycle, captured when the reporter starts
   so it matches the shared memory actually allocated. 0 means no reporter is
   running yet. *)
let reporter_max_datasources = ref 0

(* Aggregate per-caller statistics into per-group [(group, tokens, calls)]
   totals. Group membership is mirrored into each entry, so this needs no
   database access - important because it runs on the reporter thread, which has
   no context. *)
let group_totals () =
  let totals : (string, float * int) Hashtbl.t = Hashtbl.create 64 in
  entries_of_table ()
  |> List.iter (fun entry ->
      let tokens = Caller_statistics.get_token_count entry.stats in
      let calls = Caller_statistics.get_call_count entry.stats in
      List.iter
        (fun group ->
          let t, c =
            Option.value ~default:(0.0, 0) (Hashtbl.find_opt totals group)
          in
          Hashtbl.replace totals group (t +. tokens, c + calls)
        )
        entry.groups
  ) ;
  Hashtbl.fold (fun group (t, c) acc -> (group, t, c) :: acc) totals []

let make_group_dss () =
  let groups = group_totals () in
  let groups =
    let max_groups = !reporter_max_datasources / 2 in
    if max_groups > 0 && List.length groups > max_groups then (
      (* Defensive: never emit more data sources than the shared memory was
         sized for. If somehow over, keep the busiest groups by token usage. *)
      debug
        "caller RRD reporter: %d groups exceed reporter capacity (%d); \
         reporting the busiest"
        (List.length groups) max_groups ;
      groups
      |> List.sort (fun (_, t1, _) (_, t2, _) -> compare t2 t1)
      |> List.filteri (fun i _ -> i < max_groups)
    ) else
      groups
  in
  groups
  |> List.concat_map (fun (group, tokens, calls) ->
      [
        ( Rrd.Host
        , Ds.ds_make
            ~name:(Printf.sprintf "group_%s_tokens" group)
            ~description:
              (Printf.sprintf "Total tokens consumed by caller group %s" group)
            ~value:(Rrd.VT_Float tokens) ~ty:Rrd.Derive ~default:true
            ~units:"tokens" ~min:0.0 ()
        )
      ; ( Rrd.Host
        , Ds.ds_make
            ~name:(Printf.sprintf "group_%s_calls" group)
            ~description:(Printf.sprintf "Total calls by caller group %s" group)
            ~value:(Rrd.VT_Int64 (Int64.of_int calls))
            ~ty:Rrd.Derive ~default:true ~units:"calls" ~min:0.0 ()
        )
      ]
  )

let reporter : Rrdd_plugin.Reporter.t option ref = ref None

let start_reporter () =
  reporter_max_datasources := 2 * reporter_max_groups ;
  let page_count = reporter_page_count reporter_max_groups in
  try
    let r =
      Rrdd_plugin.Reporter.start_async
        (module D : Debug.DEBUG)
        ~uid:reporter_uid ~neg_shift:0.5
        ~target:(Rrdd_plugin.Reporter.Local page_count)
        ~protocol:Rrd_interface.V2 ~dss_f:make_group_dss
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
    auto_registered_count := 0 ;
    List.iter
      (fun self ->
        let record = Db.Caller.get_record ~__context ~self in
        let pattern_key = pattern_key_of_record record in
        insert_entry_locked ~caller_ref:self
          ~stats:(Caller_statistics.create ~caller_uuid:record.caller_uuid)
          ~pattern_key ~rate_limit_ref:record.API.caller_rate_limit
          ~auto_registered:record.API.caller_auto_registered
          ~groups:record.API.caller_groups () ;
        if record.API.caller_auto_registered then incr auto_registered_count
      )
      (Db.Caller.get_all ~__context) ;
    (* Auto-registered callers persist across restarts, so the database may
       already hold more than the current cap (e.g. after lowering it, or after
       disabling auto-registration with a limit of 0). Trim the excess, dropping
       those with the least recent call first. A negative limit means unbounded,
       so nothing is trimmed. Recency here comes from the persisted [last_access]
       field - the only recency signal available before any calls have been seen
       this boot. This reads [last_access] once per auto-registered caller, but
       only on the rare boot where the database already exceeds the cap. *)
    let limit = !Xapi_globs.max_auto_registered_callers in
    ( if limit >= 0 && !auto_registered_count > limit then
        let auto_callers =
          entries_of_table ()
          |> List.filter (fun entry -> entry.auto_registered)
          |> List.map (fun entry ->
              let last_access =
                try Db.Caller.get_last_access ~__context ~self:entry.caller_ref
                with _ -> Clock.Date.epoch
              in
              (entry.caller_ref, last_access)
          )
          |> List.sort (fun (_, a) (_, b) -> Clock.Date.compare a b)
        in
        let to_drop = !auto_registered_count - limit in
        auto_callers
        |> List.filteri (fun i _ -> i < to_drop)
        |> List.iter (fun (self, _) ->
            debug
              "Auto-registered caller count exceeds limit (%d) at startup; \
               evicting least recently used caller %s"
              limit (Ref.string_of self) ;
            destroy_locked ~__context ~self
        )
    ) ;
    start_reporter ()
  )
