val manage_domain_zero : bool ref

val domain_zero_dynamic_min : int64 ref

val domain_zero_dynamic_max : int64 option ref

val boot_time_host_free_memory_constant_count_min : int ref

val boot_time_host_free_memory_check_interval : float ref

type domain = {
    domid: int
  ; can_balloon: bool
  ; dynamic_min_kib: int64
  ; target_kib: int64
  ; dynamic_max_kib: int64
  ; memory_actual_kib: int64
  ; memory_max_kib: int64
  ; inaccuracy_kib: int64
}

val domain_make :
  int -> bool -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> domain

module DomainSet : Set.S with type elt = domain

type host = {domains: DomainSet.t; free_mem_kib: int64}

val make_host : domains:DomainSet.t -> free_mem_kib:int64 -> host

type action = {action_domid: int; new_target_kib: int64}

val ( -* ) : int64 -> int64 -> int64

val ( +* ) : int64 -> int64 -> int64

val ( ** ) : int64 -> int64 -> int64

type result = Success | Failed of int list | AdjustTargets of action list

val has_hit_target : int64 -> int64 -> int64 -> bool

module Stuckness_monitor : sig
  type per_domain_state = {
      mutable last_actual_kib: int64
    ; mutable last_makingprogress_time: float
    ; mutable stuck: bool
  }

  type t = {per_domain: (int, per_domain_state) Hashtbl.t}
end

type fistpoint = DisableTwoPhaseTargetSets | DisableInaccuracyCompensation

val min_freeable : ?fistpoints:fistpoint list -> domain -> int64

module Gnuplot : sig
  type colspec = Dynamic_min | Memory_actual | Dynamic_max | Target

  val write_header : out_channel -> colspec list -> unit

  val write_row : out_channel -> host -> colspec list -> float -> unit

  val write_gp : string -> host -> colspec list -> unit
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

exception Cannot_free_this_much_memory of int64 * int64

exception Domains_refused_to_cooperate of int list

val change_host_free_memory :
  ?fistpoints:fistpoint list -> io -> int64 -> (int64 -> bool) -> unit

val free_memory_range :
  ?fistpoints:fistpoint list -> io -> int64 -> int64 -> int64

val is_balanced : ?fistpoints:'a -> io -> int64 -> bool

val balance_memory : ?fistpoints:fistpoint list -> io -> unit

val is_host_memory_unbalanced : ?fistpoints:fistpoint list -> io -> bool
