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
module D = Debug.Make (struct let name = "xapi_rate_limit" end)

open D
module Rate_limit = Rate_limit_lib.Rate_limit

(** Map of Rate_limit ref -> in-memory token-bucket worker. Owned here;
    Xapi_caller looks up workers by ref via [find_bucket]. *)
let buckets : (API.ref_Rate_limit, Rate_limit.t) Hashtbl.t = Hashtbl.create 16

let mutex = Mutex.create ()

let with_mutex f =
  Mutex.lock mutex ;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

(** Callback invoked after a caller's rate_limit ref changes in the database,
    so that the in-memory caller_table entry can be rebuilt to point at the
    correct bucket. Set by [Xapi_caller.register]. Default is a no-op so
    callbacks ordering during startup is not load-bearing. *)
let on_caller_rate_limit_changed :
    (__context:Context.t -> API.ref_Caller -> unit) ref =
  ref (fun ~__context:_ _ -> ())

let set_caller_refresh_callback f = on_caller_rate_limit_changed := f

let notify_caller_changed ~__context caller_ref =
  !on_caller_rate_limit_changed ~__context caller_ref

let find_bucket rate_limit_ref =
  with_mutex (fun () -> Hashtbl.find_opt buckets rate_limit_ref)

(* NaN fails every comparison, so check that the values are valid rather
   than that they are invalid. An infinite fill rate is refused because it
   can turn the token count into NaN, which never admits a request again. *)
let valid_fill_rate fill_rate = Float.is_finite fill_rate && fill_rate > 0.

let valid_burst_size burst_size = burst_size > 0.

let validate_params ~burst_size ~fill_rate =
  if not (valid_fill_rate fill_rate) then
    raise
      Api_errors.(
        Server_error (invalid_value, ["fill_rate"; string_of_float fill_rate])
      ) ;
  if not (valid_burst_size burst_size) then
    raise
      Api_errors.(
        Server_error (invalid_value, ["burst_size"; string_of_float burst_size])
      )

let build_bucket ~burst_size ~fill_rate =
  try Rate_limit.create ~burst_size ~fill_rate
  with Invalid_argument msg ->
    raise Api_errors.(Server_error (invalid_value, [msg]))

(* Serialises the changes of the in-memory limiters: [install_bucket] and
   [remove_bucket] each run as one step under it, and change the table
   under [mutex] within that step. Take [change_mutex] before [mutex], never
   the other way round. *)
let change_mutex = Mutex.create ()

let with_change_mutex f =
  Mutex.lock change_mutex ;
  Fun.protect ~finally:(fun () -> Mutex.unlock change_mutex) f

(* Build the limiter of [self] from its parameters in the database, and
   install it, both under [change_mutex]. Every change of a rate limit
   writes the database before it installs a limiter, so the install that
   runs last reads every change: concurrent changes of the burst size and
   the fill rate cannot leave a limiter built from an older value. If the
   rate limit has been destroyed, reading it raises and nothing is
   installed.

   [Rate_limit.delete] waits until the old worker has served every request
   still queued on it, at the old rate. Call it after releasing both
   mutexes: [find_bucket] takes [mutex] on every rate-limited dispatch. *)
let install_bucket ~__context ~self =
  let old =
    with_change_mutex (fun () ->
        let burst_size = Db.Rate_limit.get_burst_size ~__context ~self in
        let fill_rate = Db.Rate_limit.get_fill_rate ~__context ~self in
        let bucket = build_bucket ~burst_size ~fill_rate in
        with_mutex (fun () ->
            let old = Hashtbl.find_opt buckets self in
            Hashtbl.replace buckets self bucket ;
            old
        )
    )
  in
  Option.iter Rate_limit.delete old

let remove_bucket ~self =
  let old =
    with_change_mutex (fun () ->
        with_mutex (fun () ->
            let old = Hashtbl.find_opt buckets self in
            Hashtbl.remove buckets self ;
            old
        )
    )
  in
  Option.iter Rate_limit.delete old

let create ~__context ~name_label ~name_description ~burst_size ~fill_rate =
  validate_params ~burst_size ~fill_rate ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  Db.Rate_limit.create ~__context ~ref ~uuid:(Uuidx.to_string uuid) ~name_label
    ~name_description ~burst_size ~fill_rate ;
  install_bucket ~__context ~self:ref ;
  ref

(* Serialises [destroy] with [add_caller]. Without it, a caller attached
   between [destroy]'s read of the callers and its deletion of the row would
   keep a reference to the deleted row, and would run with no rate limit. *)
let membership_mutex = Mutex.create ()

let with_membership_mutex f =
  Mutex.lock membership_mutex ;
  Fun.protect ~finally:(fun () -> Mutex.unlock membership_mutex) f

let destroy ~__context ~self =
  with_membership_mutex (fun () ->
      let attached_callers = Db.Rate_limit.get_callers ~__context ~self in
      List.iter
        (fun caller ->
          Db.Caller.set_rate_limit ~__context ~self:caller ~value:Ref.null ;
          notify_caller_changed ~__context caller
        )
        attached_callers ;
      (* Destroy the row first: a concurrent change that installs a limiter
         after this point fails to read the row, and one that installed
         before is removed below. *)
      Db.Rate_limit.destroy ~__context ~self
  ) ;
  (* [remove_bucket] waits for the requests queued on the bucket: do not
     hold [membership_mutex] meanwhile. No caller names the row any more. *)
  remove_bucket ~self

let add_caller ~__context ~self ~caller =
  (* One rate limit per caller. Set the new value directly: the datamodel's
     reverse-relation machinery removes the caller from any previous
     Rate_limit's [callers] set automatically. The previous implementation
     cleared to Ref.null first and then set the target, which briefly
     left the caller unlimited and triggered two full refreshes (each
     an extra DB read for the callback). *)
  with_membership_mutex (fun () ->
      (* The database stores any reference it is given: refuse one that
         names no Rate_limit row (HANDLE_INVALID), rather than store it. *)
      ignore (Db.Rate_limit.get_uuid ~__context ~self) ;
      let previous = Db.Caller.get_rate_limit ~__context ~self:caller in
      if previous <> self then (
        Db.Caller.set_rate_limit ~__context ~self:caller ~value:self ;
        notify_caller_changed ~__context caller
      )
  )

let remove_caller ~__context ~self ~caller =
  let current = Db.Caller.get_rate_limit ~__context ~self:caller in
  if current = self then (
    Db.Caller.set_rate_limit ~__context ~self:caller ~value:Ref.null ;
    notify_caller_changed ~__context caller
  )

(* No [notify_caller_changed] on parameter changes: the caller_table entry
   still points at the same rate_limit_ref, and [install_bucket] has
   already swapped the in-memory bucket - the next [find_bucket] on the
   dispatch path picks up the new parameters. Notifying attached callers
   here would just be an extra DB read per caller with no state change. *)
let set_burst_size ~__context ~self ~value =
  let fill_rate = Db.Rate_limit.get_fill_rate ~__context ~self in
  validate_params ~burst_size:value ~fill_rate ;
  Db.Rate_limit.set_burst_size ~__context ~self ~value ;
  install_bucket ~__context ~self

let set_fill_rate ~__context ~self ~value =
  let burst_size = Db.Rate_limit.get_burst_size ~__context ~self in
  validate_params ~burst_size ~fill_rate:value ;
  Db.Rate_limit.set_fill_rate ~__context ~self ~value ;
  install_bucket ~__context ~self

let register ~__context =
  if not !Xapi_globs.rate_limit_enabled then
    debug
      "Rate limiting disabled (rate_limit=false); skipping bucket registration"
  else
    List.iter
      (fun self ->
        let record = Db.Rate_limit.get_record ~__context ~self in
        let burst_size = record.API.rate_limit_burst_size in
        let fill_rate = record.API.rate_limit_fill_rate in
        if valid_fill_rate fill_rate && valid_burst_size burst_size then
          install_bucket ~__context ~self
        else
          warn "Skipping rate_limit %s: invalid persisted parameters"
            record.API.rate_limit_uuid
      )
      (Db.Rate_limit.get_all ~__context)
