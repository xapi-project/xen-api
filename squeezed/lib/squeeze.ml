(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Implement a simple 'proportional memory' policy where VMs are squeezed
    equally to free memory. There is no direct attempt to respond to distress or
    memory pressure information from within the guest. Guests which are stuck
    (e.g. broken balloon driver or who cannot free any more memory) are observed
    and worked around. *)

(* Remember: Be tolerant of domains appearing or disappearing. What if target <>
   memory_actual because of domU accounting errors. Can we calibrate on boot if
   the offset is const? *)

(* Make debug printing work both when linked into a daemon and from the
   commandline *)
let start = Unix.gettimeofday ()

module D = Debug.Make (struct let name = "xenops" end)

let debug_oc = ref stdout

let debug fmt =
  Printf.kprintf
    (fun x ->
      Printf.fprintf !debug_oc "[%.2f] %s\n" (Unix.gettimeofday () -. start) x ;
      flush !debug_oc ;
      D.debug "%s" x
      )
    fmt

let error fmt =
  Printf.kprintf
    (fun x ->
      Printf.fprintf !debug_oc "[%.2f] %s\n" (Unix.gettimeofday () -. start) x ;
      flush !debug_oc ;
      D.error "%s" x
      )
    fmt

let manage_domain_zero = ref false

let gib = Int64.(mul 1024L (mul 1024L 1024L))

let domain_zero_dynamic_min : int64 ref = ref gib (* 1 GiB minimum for safety *)

let domain_zero_dynamic_max : int64 option ref = ref None (* static max *)

let boot_time_host_free_memory_constant_count_min : int ref = ref 10

let boot_time_host_free_memory_check_interval : float ref = ref 1.0

(** Per-domain data *)
type domain = {
    domid: int
        (** true if the domain has ballooning capability i.e. is not paused etc. *)
  ; can_balloon: bool  (** admin-imposed lower-limit on the balloon target *)
  ; dynamic_min_kib: int64
        (** current balloon target requested by the system *)
  ; target_kib: int64  (** admin-imposed upper-limit on the balloon target *)
  ; dynamic_max_kib: int64
        (** view from dom0 of how much memory this guest is using *)
  ; memory_actual_kib: int64  (** xen maxmem *)
  ; memory_max_kib: int64
        (** amount by which the target may differ from memory_actual and be
            declared a 'hit' *)
  ; inaccuracy_kib: int64
}

let domain_make domid can_balloon dynamic_min_kib target_kib dynamic_max_kib
    memory_actual_kib memory_max_kib inaccuracy_kib =
  {
    domid
  ; can_balloon
  ; dynamic_min_kib
  ; target_kib
  ; dynamic_max_kib
  ; memory_actual_kib
  ; memory_max_kib
  ; inaccuracy_kib
  }

let domain_to_string_pairs (x : domain) =
  let i64 = Int64.to_string and i = string_of_int in
  [
    ("domid", i x.domid)
  ; ("can_balloon", string_of_bool x.can_balloon)
  ; ("dynamic_min_kib", i64 x.dynamic_min_kib)
  ; ("target_kib", i64 x.target_kib)
  ; ("dynamic_max_kib", i64 x.dynamic_max_kib)
  ; ("memory_actual_kib", i64 x.memory_actual_kib)
  ; ("memory_max_kib", i64 x.memory_max_kib)
  ; ("inaccuracy_kib", i64 x.inaccuracy_kib)
  ]

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

(** Per-Host data *)
type host = {
    domains: domain list  (** VMs running on this host *)
  ; domid_to_domain: domain IntMap.t  (** total free memory on this host *)
  ; free_mem_kib: int64
}

let make_host ~domains ~free_mem_kib =
  {
    domains
  ; domid_to_domain=
      List.fold_left
        (fun map domain -> IntMap.add domain.domid domain map)
        IntMap.empty domains
  ; free_mem_kib
  }

let string_pairs_to_string (x : (string * string) list) =
  String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x)

let domain_to_string d = string_pairs_to_string (domain_to_string_pairs d)

let host_to_string_pairs (x : host) =
  let domains = String.concat "; " (List.map domain_to_string x.domains) in
  [
    ("domains", "[" ^ domains ^ "]")
  ; ("free_mem_kib", Int64.to_string x.free_mem_kib)
  ]

(** The ballooning algorithm returns a list of actions to perform *)
type action = {
    action_domid: int  (** domid of domain to operate on *)
  ; new_target_kib: int64  (** new balloon target to set *)
}

let action_to_string_pairs (x : action) =
  [
    ("domid", string_of_int x.action_domid)
  ; ("new_target_kib", Int64.to_string x.new_target_kib)
  ]

let ( -* ) = Int64.sub

let ( +* ) = Int64.add

let ( ** ) = Int64.mul

let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

let sum = List.fold_left ( +* ) 0L

(** The value returned from the 'free_memory' function *)
type result =
  | Success  (** enough memory is now available *)
  | Failed of int list
      (** we have run out of options: all domains are either fully-ballooned or
          are stuck (stuck domids returned) *)
  | AdjustTargets of action list
      (** we want to change the targets of some domains *)

type direction = Up | Down

let string_of_direction = function
  | Some Up ->
      "^"
  | Some Down ->
      "v"
  | None ->
      "x"

let direction_of_actual inaccuracy_kib memory_actual_kib target_kib =
  let delta_kib = memory_actual_kib -* target_kib in
  let abs x = if x < 0L then 0L -* x else x in
  if abs delta_kib <= inaccuracy_kib then
    None
  else if target_kib > memory_actual_kib then
    Some Down
  else
    Some Up

let direction_of_int64 a b =
  if a = b then None else if a > b then Some Down else Some Up

(** Work around the fact that the target may not be hit precisely *)
let has_hit_target inaccuracy_kib memory_actual_kib target_kib =
  direction_of_actual inaccuracy_kib memory_actual_kib target_kib = None

let short_string_of_domain domain =
  Printf.sprintf "%d T%Ld A%Ld M%Ld %s%s" domain.domid domain.target_kib
    domain.memory_actual_kib domain.memory_max_kib
    (if domain.can_balloon then "B" else "?")
    (string_of_direction
       (direction_of_actual domain.inaccuracy_kib domain.memory_actual_kib
          domain.target_kib
       )
    )

(** Generic code to guesstimate if a balloon driver is stuck *)
module Stuckness_monitor = struct
  (* We keep some state to help us spot stuck / dead / unco-operative balloon
     drivers. If a driver has been requested to release some memory but nothing
     has changed after some threshold time, we mark it as stuck and exclude it
     from our calculations. The effect is to ask the remaining functioning
     balloon drivers to balloon down faster. *)
  let assume_balloon_driver_stuck_after = 5. (* seconds *)

  type per_domain_state = {
      mutable last_actual_kib: int64  (** last value of memory actual seen *)
    ; mutable last_makingprogress_time: float
          (** last time we saw progress towards the target *)
    ; mutable stuck: bool
  }

  type t = {per_domain: (int, per_domain_state) Hashtbl.t}

  (** Make a monitoring object *)
  let make () : t = {per_domain= Hashtbl.create 10}

  (** Update our internal state given a snapshot of the outside world *)
  let update (x : t) (host : host) (now : float) =
    List.iter
      (fun (domain : domain) ->
        let direction =
          direction_of_actual domain.inaccuracy_kib domain.memory_actual_kib
            domain.target_kib
        in
        if not (Hashtbl.mem x.per_domain domain.domid) then
          Hashtbl.replace x.per_domain domain.domid
            (* new domains are considered to be making progress now and not
               stuck *)
            {
              last_actual_kib= domain.memory_actual_kib
            ; last_makingprogress_time= now
            ; stuck= false
            } ;
        let state = Hashtbl.find x.per_domain domain.domid in
        let delta_actual = domain.memory_actual_kib -* state.last_actual_kib in
        state.last_actual_kib <- domain.memory_actual_kib ;
        (* If memory_actual is moving towards the target then we say we are
           makingprogress *)
        let makingprogress =
          (delta_actual > 0L && direction = Some Down)
          || (delta_actual < 0L && direction = Some Up)
        in
        (* We keep track of the last time we were makingprogress. If we are
           makingprogress now then we are not stuck. *)
        if makingprogress then (
          state.last_makingprogress_time <- now ;
          state.stuck <- false
        ) ;
        (* If there is a request (ie work to do) and we haven't been
           makingprogress for more than the assume_balloon_driver_stuck_after
           then declare this domain stuck. *)
        let request = direction <> None in
        (* ie target <> actual *)
        if
          request
          && now -. state.last_makingprogress_time
             > assume_balloon_driver_stuck_after
        then
          state.stuck <- true
        )
      host.domains ;
    (* Clear out dead domains just in case someone keeps *)
    (* one of these things around for a long time.       *)
    let live_domids = List.map (fun domain -> domain.domid) host.domains in
    let to_delete =
      Hashtbl.fold
        (fun domid _ acc ->
          if List.mem domid live_domids then acc else domid :: acc
          )
        x.per_domain []
    in
    List.iter (Hashtbl.remove x.per_domain) to_delete

  (** Return true if we think a particular driver is still making useful
      progress. If it is not making progress it may have either hit its target
      or it may have failed. *)
  let domid_is_active (x : t) domid (_ : float) =
    if not (Hashtbl.mem x.per_domain domid) then
      false (* it must have been destroyed *)
    else
      not (Hashtbl.find x.per_domain domid).stuck
end

type fistpoint =
  | DisableTwoPhaseTargetSets
      (** this prevents free memory going to 0 and allowing all low memory to be
          allocated *)
  | DisableInaccuracyCompensation
      (** don't factor balloon driver inaccuracy into calculations *)

(** The minimum amount we will free by setting target = dynamic_min *)
let min_freeable ?(fistpoints = []) domain =
  if List.mem DisableInaccuracyCompensation fistpoints then
    max 0L (domain.memory_actual_kib -* domain.dynamic_min_kib)
  else
    max 0L
      (domain.memory_actual_kib
      -* domain.dynamic_min_kib
      -* (2L ** domain.inaccuracy_kib)
      )

(** The minimum amount we will allocate by setting target = dynamic_max *)
let min_allocatable domain =
  max 0L
    (domain.dynamic_max_kib
    -* domain.memory_actual_kib
    -* (2L ** domain.inaccuracy_kib)
    )

(** The range between dynamic_min and dynamic_max i.e. the total amount we may
    vary the balloon target NOT the total amount the memory_actual may vary. *)
let range domain = max 0L (domain.dynamic_max_kib -* domain.dynamic_min_kib)

module type POLICY = sig
  val compute_target_adjustments :
    ?fistpoints:fistpoint list -> bool -> host -> int64 -> (domain * int64) list
end

(** Represents an algorithm which attempts to (i) free memory; and (ii) balance
    memory between VMs on a host by setting balloon targets. *)
module Proportional = struct
  (** Constrains [value] within the range [minimum] ... [maximum]. *)
  let constrain minimum maximum value =
    assert (minimum < maximum) ;
    if value < minimum then
      minimum
    else if value > maximum then
      maximum
    else if classify_float value = FP_nan then
      minimum
    else (* arbitrary *)
      value

  (** Given an amount of theoretically surplus memory (= host free memory + that
      which would be freed by ballooning down to dynamic_min) produce a set of
      balloon-target-set actions to divide it up amongst the given VMs so that
      (target - min) / (max - min) is the same for all VMs. *)
  let allocate_memory_in_proportion verbose surplus_memory_kib domains =
    (* We allocate surplus memory in proportion to each domain's dynamic_range: *)
    let allocate gamma domain =
      Int64.of_float (gamma *. Int64.to_float (range domain))
    in
    (* gamma = the proportion where 0 <= gamma <= 1 *)
    let total_range = sum (List.map range domains) in
    let gamma' =
      Int64.to_float surplus_memory_kib /. Int64.to_float total_range
    in
    let gamma = constrain 0. 1. gamma' in
    if verbose then (
      debug "total_range = %Ld gamma = %f gamma' = %f" total_range gamma gamma' ;
      debug
        "Total additional memory over dynamic_min = %Ld KiB; will set gamma = \
         %.2f (leaving unallocated %Ld KiB)"
        surplus_memory_kib gamma
        ( if total_range = 0L then
            0L
        else
          Int64.of_float (Int64.to_float total_range *. (gamma' -. gamma))
        )
    ) ;
    List.map
      (fun domain -> (domain, domain.dynamic_min_kib +* allocate gamma domain))
      domains

  (* Given a set of domains and a host free memory target, return balloon target
     adjustments *)
  let compute_target_adjustments ?fistpoints verbose (host : host)
      host_target_kib =
    (* If all domains balloon down to dynamic_min: *)
    let maximum_free_mem_kib =
      host.free_mem_kib +* sum (List.map (min_freeable ?fistpoints) host.domains)
    in
    let surplus_memory_kib = max 0L (maximum_free_mem_kib -* host_target_kib) in
    allocate_memory_in_proportion verbose surplus_memory_kib host.domains
end

module Policy : POLICY = Proportional

module Squeezer = struct
  (** State maintained between invocations of the algorithm function *)
  type t = {
      stuckness: Stuckness_monitor.t
    ; non_active_domids: int list (* domids are unique and constant *)
  }

  let make () = {stuckness= Stuckness_monitor.make (); non_active_domids= []}

  (** Takes a view of the host state and amount of free memory desired and
      returns a list of ballooning actions which may help achieve the goal. *)
  let one_iteration ?(fistpoints = []) verbose success_condition (x : t)
      (host : host) host_target_kib (now : float) =
    (* 1. Compute which domains are still considered active *)
    Stuckness_monitor.update x.stuckness host now ;
    let active_domains =
      List.filter
        (fun domain ->
          domain.can_balloon
          && Stuckness_monitor.domid_is_active x.stuckness domain.domid now
          )
        host.domains
    in
    let non_active_domids =
      List.map (fun d -> d.domid) (set_difference host.domains active_domains)
    in
    let declared_inactive_domids =
      set_difference non_active_domids x.non_active_domids
    in
    let declared_active_domids =
      set_difference x.non_active_domids non_active_domids
    in
    if verbose then (
      List.iter
        (fun d -> debug "domid %d has been declared inactive" d)
        declared_inactive_domids ;
      List.iter
        (fun d -> debug "domid %d has been declared active" d)
        declared_active_domids
    ) ;
    let x = {x with non_active_domids} in
    (* 2. Compute how we would adjust the domain memory targets *)
    let targets =
      Policy.compute_target_adjustments ~fistpoints verbose
        {host with domains= active_domains}
        host_target_kib
    in
    let maximum_possible_free_memory_kib =
      host.free_mem_kib
      +* sum (List.map (min_freeable ~fistpoints) active_domains)
    in
    if verbose then
      debug
        "Maximum possible free memory if all active domains balloon down to \
         dynamic_min = %Ld"
        maximum_possible_free_memory_kib ;
    (* Xen heap workaround: *)
    let is_freeing_memory (domain, new_target_kib) =
      (not
         (has_hit_target domain.inaccuracy_kib domain.memory_actual_kib
            new_target_kib
         )
      )
      && new_target_kib < domain.memory_actual_kib
    in
    let freeing, allocating = List.partition is_freeing_memory targets in
    let allocation_phase = freeing = [] in
    let targets =
      if List.mem DisableTwoPhaseTargetSets fistpoints then
        targets
      else if allocation_phase then
        allocating
      else
        freeing
    in
    (* Have all the non-stuck domains reached their current targets? *)
    let all p xs = List.fold_left ( && ) true (List.map p xs) in
    let hit_target domain =
      has_hit_target domain.inaccuracy_kib domain.memory_actual_kib
        domain.target_kib
    in
    let all_targets_reached = all hit_target active_domains in
    let max_target domain = domain.target_kib = domain.dynamic_max_kib in
    let cant_allocate_any_more = all max_target active_domains in
    (* Note the asymmetry between:

       1. increasing free memory: if we think we can't free enough then we give
       up

       2. reducing free memory: we wait until as much as possible is allocated *)
    let success = success_condition host.free_mem_kib in
    let target_too_big = maximum_possible_free_memory_kib < host_target_kib in
    let no_target_changes =
      List.filter (fun (domain, target) -> domain.target_kib <> target) targets
      = []
    in
    if verbose then
      debug
        "current host free mem = %Ld KiB (aiming for %Ld KiB);%s; all domain \
         targets%s reached%s; %s"
        host.free_mem_kib host_target_kib
        ( if success then
            " OK"
        else if target_too_big then
          " cannot free enough"
        else if cant_allocate_any_more then
          " cannot allocate enough"
        else
          ""
        )
        (if all_targets_reached then "" else " not")
        (if no_target_changes then "" else "; however about to adjust targets")
        (if allocation_phase then "allocation phase" else "freeing phase") ;
    (* If we have to blame a domain for being stuck, we don't blame it if it
       can't balloon. *)
    let non_active_domains =
      List.concat
        (List.map
           (fun d ->
             try [IntMap.find d host.domid_to_domain] with Not_found -> []
             )
           non_active_domids
        )
    in
    let non_active_can_balloon_domains =
      List.filter (fun d -> d.can_balloon) non_active_domains
    in
    let non_active_can_balloon_domids =
      List.map (fun d -> d.domid) non_active_can_balloon_domains
    in
    (* 1. In all cases we wait for all targets to be reached and to be stable.
       2. If targets are reached and stable we evaluate our success condition
       and succeed/fail accordingly. *)
    let action =
      if (not all_targets_reached) || not no_target_changes then
        AdjustTargets
          (List.map
             (fun (d, t) -> {action_domid= d.domid; new_target_kib= t})
             targets
          )
      else if success then
        Success
      else
        Failed non_active_can_balloon_domids
    in
    (x, declared_active_domids, declared_inactive_domids, action)
end

module Gnuplot = struct
  type colspec = Dynamic_min | Memory_actual | Dynamic_max | Target

  let write_header oc cols =
    let all =
      List.concat
        [
          (if List.mem Dynamic_min cols then ["dynamic_min"] else [])
        ; (if List.mem Dynamic_max cols then ["dynamic_max"] else [])
        ; (if List.mem Memory_actual cols then ["memory_actual"] else [])
        ; (if List.mem Target cols then ["target"] else [])
        ]
    in
    Printf.fprintf oc "# for each domain: %s\n" (String.concat " " all)

  let write_row oc host cols time =
    Printf.fprintf oc "%.2f %Ld " time host.free_mem_kib ;
    List.iter
      (fun domain ->
        if List.mem Dynamic_min cols then
          Printf.fprintf oc " %Ld " domain.dynamic_min_kib ;
        if List.mem Dynamic_max cols then
          Printf.fprintf oc " %Ld " domain.dynamic_max_kib ;
        if List.mem Memory_actual cols then
          Printf.fprintf oc " %Ld " domain.memory_actual_kib ;
        if List.mem Target cols then
          Printf.fprintf oc " %Ld " domain.target_kib
        )
      host.domains ;
    Printf.fprintf oc "\n"

  let write_gp stem host cols =
    let oc = open_out (Printf.sprintf "%s.gp" stem) in
    Printf.fprintf oc "set xlabel 'time/seconds'\n" ;
    Printf.fprintf oc "set ylabel 'memory/KiB'\n" ;
    Printf.fprintf oc
      "plot \"%s.dat\" using 1:2 title \"free host memory\" with lines " stem ;
    let col = ref 3 in
    List.iter
      (fun domain ->
        if List.mem Dynamic_min cols then (
          Printf.fprintf oc
            ", \"%s.dat\" using 1:%d title \"domid %d dynamic_min\" with \
             points "
            stem !col domain.domid ;
          incr col
        ) ;
        if List.mem Dynamic_max cols then (
          Printf.fprintf oc
            ", \"%s.dat\" using 1:%d title \"domid %d dynamic_max\" with \
             points "
            stem !col domain.domid ;
          incr col
        ) ;
        if List.mem Memory_actual cols then (
          Printf.fprintf oc
            ", \"%s.dat\" using 1:%d title \"domid %d memory_actual\" with \
             lines "
            stem !col domain.domid ;
          incr col
        ) ;
        if List.mem Target cols then (
          Printf.fprintf oc
            ", \"%s.dat\" using 1:%d title \"domid %d target\" with lines " stem
            !col domain.domid ;
          incr col
        )
        )
      host.domains ;
    close_out oc
end

type io = {
    verbose: bool
  ; make_host: unit -> string * host
  ; domain_setmaxmem: int -> int64 -> unit
  ; execute_action: action -> unit
  ; wait: float -> unit
  ; gettimeofday: unit -> float
  ; target_host_free_mem_kib: int64
  ; free_memory_tolerance_kib: int64
}

(** even if we balloon everyone down we can't free this much *)
exception Cannot_free_this_much_memory of int64 * int64

(** these VMs didn't release memory and we failed *)
exception Domains_refused_to_cooperate of int list

let change_host_free_memory ?fistpoints io required_mem_kib success_condition =
  (* For performance concern, do not call squeezer if host memory is enough and
     other VMs target has reached. *)
  let host = snd (io.make_host ()) in
  if io.verbose then
    debug
      "change_host_free_memory required_mem = %Ld KiB target_mem = %Ld KiB \
       free_mem = %Ld KiB"
      required_mem_kib io.target_host_free_mem_kib host.free_mem_kib ;
  let active_domains =
    List.filter (fun domain -> domain.can_balloon) host.domains
  in
  let hit_target domain =
    has_hit_target domain.inaccuracy_kib domain.memory_actual_kib
      domain.target_kib
  in
  let all_targets_reached = List.for_all hit_target active_domains in
  if io.verbose then
    debug "change_host_free_memory all VM target meet %B" all_targets_reached ;
  let finished =
    ref
      ((not (required_mem_kib = io.target_host_free_mem_kib))
      && required_mem_kib <= host.free_mem_kib
      && all_targets_reached
      )
  in
  let acc = ref (Squeezer.make ()) in
  while not !finished do
    let t = io.gettimeofday () in
    let host_debug_string, host = io.make_host () in
    let acc', _declared_active_domids, declared_inactive_domids, result =
      Squeezer.one_iteration ?fistpoints io.verbose success_condition !acc host
        required_mem_kib t
    in
    acc := acc' ;

    (* Set the max_mem of a domain as follows:

       If the VM has never been run && is paused -> use initial-reservation

       If the VM is active -> use target

       If the VM is inactive -> use min(target, actual)

       So active VMs may move up or down towards their target and either get
       there (while we actively monitor them) or are declared inactive. Inactive
       VMs are allowed to free memory while we aren't looking but they are not
       to allocate more.

       Note that the concept of having 'never been run' is hidden from us by the
       'make_host' function above. The data we receive here will show either an
       inactive (paused) domain with target=actual=initial_reservation or an
       active (unpaused) domain. So the we need only deal with 'active' vs
       'inactive'. *)

    (* Compile a list of new targets *)
    let new_targets =
      match result with
      | AdjustTargets actions ->
          List.map (fun a -> (a.action_domid, a.new_target_kib)) actions
      | _ ->
          []
    in
    let new_target_direction domain =
      let new_target =
        if List.mem_assoc domain.domid new_targets then
          Some (List.assoc domain.domid new_targets)
        else
          None
      in
      match new_target with
      | None ->
          string_of_direction None
      | Some t ->
          string_of_direction (direction_of_int64 domain.target_kib t)
    in
    let debug_string =
      String.concat "; "
        (host_debug_string
         ::
         List.map
           (fun domain ->
             short_string_of_domain domain ^ new_target_direction domain
             )
           host.domains
        )
    in
    debug "%s" debug_string ;
    (* For each domid, decide what maxmem should be *)
    let maxmems =
      IntMap.mapi
        (fun domid domain ->
          if List.mem domid declared_inactive_domids then
            (* CA-41832: clip the target of an 'inactive' domain to within the
               dynamic min-max range. The danger here is that a domain might be
               using less than dynamic min now, but might suddenly wake up and
               allocate memory belonging to someone else later. *)
            let ideal_kib = min domain.target_kib domain.memory_actual_kib in
            min domain.dynamic_max_kib (max domain.dynamic_min_kib ideal_kib)
          else if List.mem_assoc domid new_targets then
            List.assoc domid new_targets
          else
            domain.target_kib
          )
        host.domid_to_domain
    in
    IntMap.iter io.domain_setmaxmem maxmems ;
    match result with
    | Success ->
        if io.verbose then
          debug "Success: Host free memory = %Ld KiB" required_mem_kib ;
        finished := true
    | Failed [] ->
        if io.verbose then
          debug
            "Failed to free %Ld KiB of memory: operation impossible within \
             current dynamic_min limits of balloonable domains"
            required_mem_kib ;
        raise
          (Cannot_free_this_much_memory (required_mem_kib, host.free_mem_kib))
    | Failed [0] ->
        (* CA-75568: dom0 is changing its memory allocation without being asked *)
        if io.verbose then
          error
            "Failed to free %Ld KiB of memory: domain 0 has failed to meet its \
             memory target"
            required_mem_kib ;
        raise
          (Cannot_free_this_much_memory (required_mem_kib, host.free_mem_kib))
    | Failed domids ->
        let s = String.concat ", " (List.map string_of_int domids) in
        if io.verbose then
          debug
            "Failed to free %Ld KiB of memory: the following domains have \
             failed to meet their targets: [ %s ]"
            required_mem_kib s ;
        raise (Domains_refused_to_cooperate domids)
    | AdjustTargets actions ->
        (* Set all the balloon targets *)
        List.iter io.execute_action actions ;
        io.wait 1.
  done

let free_memory fistpoints io required_mem_kib =
  change_host_free_memory ?fistpoints io
    (required_mem_kib +* io.target_host_free_mem_kib) (fun x ->
      x >= required_mem_kib +* io.target_host_free_mem_kib
  )

let free_memory_range ?fistpoints io min_kib max_kib =
  (* First compute the 'ideal' amount of free memory based on the proportional
     allocation policy *)
  let domain =
    {
      domid= -1
    ; can_balloon= true
    ; dynamic_min_kib= min_kib
    ; dynamic_max_kib= max_kib
    ; target_kib= min_kib
    ; memory_actual_kib= 0L
    ; memory_max_kib= 0L
    ; inaccuracy_kib= 4L
    }
  in
  let host = snd (io.make_host ()) in
  let host' = {host with domains= domain :: host.domains} in
  let adjustments =
    Policy.compute_target_adjustments io.verbose host'
      io.target_host_free_mem_kib
  in
  let target =
    if List.mem_assoc domain adjustments then
      List.assoc domain adjustments
    else
      min_kib
  in
  debug "free_memory_range ideal target = %Ld" target ;
  change_host_free_memory ?fistpoints io (target +* io.target_host_free_mem_kib)
    (fun x -> x >= min_kib +* io.target_host_free_mem_kib
  ) ;
  let host = snd (io.make_host ()) in
  let usable_free_mem_kib = host.free_mem_kib -* io.target_host_free_mem_kib in
  if usable_free_mem_kib < min_kib then
    debug
      "WARNING usable_free_mem_kib (%Ld) < min_kib (%Ld) (difference = %Ld KiB)"
      usable_free_mem_kib min_kib
      (min_kib -* usable_free_mem_kib) ;
  max min_kib (min usable_free_mem_kib max_kib)

let is_balanced ?fistpoints:_ io x =
  Int64.sub x io.target_host_free_mem_kib < io.free_memory_tolerance_kib

let balance_memory ?fistpoints io =
  try
    change_host_free_memory ?fistpoints io io.target_host_free_mem_kib
      (is_balanced io)
  with e -> debug "balance memory caught: %s" (Printexc.to_string e)

(** Return true if the host memory is currently unbalanced and needs rebalancing *)
let is_host_memory_unbalanced ?fistpoints io =
  let debug_string, host = io.make_host () in
  let domains = List.map (fun d -> (d.domid, d)) host.domains in
  let _, _, _, result =
    Squeezer.one_iteration ?fistpoints false (is_balanced io) (Squeezer.make ())
      host io.target_host_free_mem_kib (io.gettimeofday ())
  in
  let is_new_target a =
    let existing = (List.assoc a.action_domid domains).target_kib in
    a.new_target_kib <> existing
  in
  match result with
  | AdjustTargets ts ->
      if List.fold_left ( || ) false (List.map is_new_target ts) then (
        debug "Memory is not currently balanced" ;
        debug "%s" debug_string ;
        true
      ) else
        false
  | _ ->
      false
