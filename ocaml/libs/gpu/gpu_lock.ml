(*
 * Copyright (C) 2026 Cloud Software Group
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

module D = Debug.Make (struct let name = __MODULE__ end)

open Xapi_stdext_unix
open Xapi_stdext_threads
module Pervasiveext = Xapi_stdext_pervasives.Pervasiveext

type local = Unbind of Xcp_pci.address | Vf_enable of Xcp_pci.address

type key = Pending | On | Off

let key_of_label = function 1 -> On | 2 -> Off | _ -> Pending

type reason = Busy of string | Stuck of string | Unusable of string

type refusal = {reason: reason; holders: int option}

let transient r =
  match r.reason with Busy _ -> true | Stuck _ | Unusable _ -> false

let reason_text = function
  | Busy what ->
      "the GPU lock stayed busy: " ^ what
  | Stuck what ->
      "a holder of the GPU lock is stuck: " ^ what
  | Unusable why ->
      "the GPU lock is unusable: " ^ why

let describe r =
  Printf.sprintf "%s; holders: %s (see the host's log or gpu-lock-cli status)"
    (reason_text r.reason)
    (Option.fold ~none:"unknown" ~some:string_of_int r.holders)

exception Refused of refusal

exception Killed of string

let () =
  Printexc.register_printer (function
    | Refused r ->
        Some (describe r)
    | Killed what ->
        Some what
    | _ ->
        None
    )

let of_acquire_error what : Lock_rw.acquire_error -> reason = function
  | `Busy ->
      Busy what
  | `Stuck ->
      Stuck what
  | `Absent ->
      Unusable "no lock object"
  | `Failed why ->
      Unusable why

let local_text = function
  | Unbind card ->
      "unbind " ^ Xcp_pci.string_of_address card
  | Vf_enable card ->
      "enable VFs of " ^ Xcp_pci.string_of_address card

(* A holder as a JSON object of one line, for logs: a process name cannot
   break the line *)
module Json = struct
  type hold = Lock_rw.hold =
    | Write [@name "exclusive"]
    | Read [@name "shared"]
    | Open [@name "open"]
  [@@deriving rpc]

  (* A process name prints as it is when it is UTF-8 without C1 control
     characters (C2 80 to C2 9F), which a terminal may act on; else escaped *)
  type process_name = string

  let rpc_of_process_name s =
    let c1 (i, c) = c = '\xc2' && s.[i + 1] < '\xa0' in
    if String.is_valid_utf_8 s && not (Seq.exists c1 (String.to_seqi s)) then
      Rpc.String s
    else
      Rpc.String (String.escaped s)

  let process_name_of_rpc = Rpc.string_of_rpc

  type holder = Lock_rw.holder = {
      pid: int
    ; command: process_name [@key "name"]
    ; run_state: string [@key "state"]
    ; hold: hold [@key "mode"]
    ; file: string
    ; d_threads: int list
  }
  [@@deriving rpc]
end

type bounds = Unbounded | Bounded

let timeout bounds span =
  match bounds with Bounded -> Some span | Unbounded -> None

(* sriov-manage takes 5 to 16 s, but systemd may take twice 45 s to stop each
   of the two services it stops: so a hang ends, and a slow run does not. A
   kill cannot end a wait in the kernel: the call then waits for the exit *)
let sriov_manage_timeout = Mtime.Span.(4 * min)

let sriov_manage bounds ~enable card run =
  try run (timeout bounds sriov_manage_timeout)
  with Forkhelpers.Subprocess_timeout ->
    let backtrace = Printexc.get_raw_backtrace () in
    let flag, left =
      if enable then
        ("-e", ", with its driver_override set, or with its VFs half set up")
      else
        ("-d", " or with its driver_override set")
    in
    Format.kasprintf
      (fun text -> Printexc.raise_with_backtrace (Killed text) backtrace)
      "sriov-manage %s %s did not end within %a and was killed: the card may \
       be left without its driver%s, and the GPU monitoring services stopped"
      flag
      (Xcp_pci.string_of_address card)
      Mtime.Span.pp sriov_manage_timeout left

let host = "host"

module Make (B : Lock_rw.S) = struct
  type t = {backend: B.t; waits: Lock_rw_wait.t; engaged: unit -> bool}

  let with_held h f =
    Pervasiveext.finally (fun () -> f h) (fun () -> B.release h)

  let timer t = Lock_rw_wait.waiter t.waits

  (* a census on dom0 takes 30-60 ms, measured *)
  let refusal_holders_timeout = Mtime.Span.(200 * ms)

  let census t key =
    match B.holders t.backend key (timer t refusal_holders_timeout) with
    | Ok holders ->
        ( Jsonrpc.to_string (Rpc.Enum (List.map Json.rpc_of_holder holders))
        , Some (List.length holders)
        )
    | Error why ->
        ("unknown (" ^ why ^ ")", None)

  let refuse t key reason =
    let names, holders = census t key in
    D.warn "%s; holders: %s" (reason_text reason) names ;
    Error {reason; holders}

  (* The creator; a creation follows every boot, so only an adoption is a
     warning *)
  let repair t key timer ~reason =
    let unusable r = Result.map_error (fun why -> Unusable why) r in
    let attest h = unusable (B.attest_if_alone t.backend h timer) in
    let attest_new h =
      Result.map (fun () -> `Info ("created " ^ B.incarnation h)) (attest h)
    in
    let adopt h =
      match B.state h with
      | Lock_rw.Attested ->
          Ok (`Info ("found " ^ B.incarnation h ^ " already attested"))
      | Unattested ->
          Result.map (fun () -> `Warn ("adopted " ^ B.incarnation h)) (attest h)
      | Replaced ->
          Error (Unusable "replaced while repairing")
    in
    let attempt () =
      match B.fresh t.backend key with
      | Ok h ->
          with_held h attest_new
      | Error (`Failed why) ->
          Error (Unusable why)
      | Error `Exists ->
          B.acquire t.backend key Lock_rw.Exclusive timer
          |> Result.map_error (of_acquire_error "repairing")
          |> Fun.flip Result.bind (fun h -> with_held h adopt)
    in
    let attempts () =
      Threadext.retry
        ~delay:Mtime.Span.(10 * ms)
        ~max_delay:Mtime.Span.(80 * ms)
        ~stop:timer.Lock_rw.stop attempt
    in
    Result.bind (unusable (B.prepare t.backend)) attempts
    |> Result.fold ~error:(refuse t key) ~ok:(function
      | `Info outcome ->
          D.info "GPU lock: %s (checked: %s)" outcome reason ;
          Ok ()
      | `Warn outcome ->
          D.warn "GPU lock: %s (checked: %s); holders: %s" outcome reason
            (fst (census t key)) ;
          Ok ()
      )

  let take t key mode timer ~what =
    let attempt on_unusable =
      match B.acquire t.backend key mode timer with
      | Ok h when B.state h = Lock_rw.Attested ->
          Ok h
      | Ok h ->
          let old = B.incarnation h in
          B.release h ;
          on_unusable (old ^ " is not the current attested lock object")
      | Error `Absent ->
          on_unusable "no lock object"
      | Error e ->
          refuse t key (of_acquire_error what e)
    in
    let still_unusable reason =
      refuse t key (Unusable ("still unusable after a repair: " ^ reason))
    in
    attempt @@ fun reason ->
    Result.bind (repair t key timer ~reason) (fun () -> attempt still_unusable)

  let with_lock t intent ~timeout f =
    if not (t.engaged ()) then
      f ~keep_fds:[] ~bounds:Unbounded
    else
      let use h = f ~keep_fds:(B.fds_for_child h) ~bounds:Bounded in
      take t host Lock_rw.Exclusive (timer t timeout) ~what:(local_text intent)
      |> Result.fold
           ~ok:(fun h -> with_held h use)
           ~error:(fun r -> raise (Refused r))

  module Switch = struct
    let ensure t ~timeout = repair t host (timer t timeout) ~reason:"ensure"

    let read_key t =
      match B.acquire t.backend host Shared (timer t Mtime.Span.zero) with
      | Error `Absent ->
          Ok None
      | Error e ->
          refuse t host (of_acquire_error "reading the key" e)
      | Ok h ->
          with_held h (fun h -> Ok (Some (key_of_label (B.label h))))

    (* How long listing the holders for an operator may take: five times as
       long as for a refusal, as a person waits for it *)
    let status_holders_timeout = Mtime.Span.(1 * s)

    let holders t =
      B.holders t.backend host (timer t status_holders_timeout)
      |> Result.map
           (List.map (fun h -> Jsonrpc.to_string (Json.rpc_of_holder h)))
  end
end

include Make (Flock)

(* under /run, cleared at boot; it holds only lock files *)
let create ?(dir = "/run/xapi-gpu-lock") ~engaged () =
  let boot_id () =
    String.trim (Unixext.string_of_file "/proc/sys/kernel/random/boot_id")
  in
  {
    backend= Flock.create ~boot_id ~magic:"xapi-gpu-lock" dir
  ; waits= Lock_rw_wait.create ()
  ; engaged
  }
