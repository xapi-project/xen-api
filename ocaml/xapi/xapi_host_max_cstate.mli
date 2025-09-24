(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** Host CPU C-state management functions.

    C-states are power management states for CPUs where higher numbered states
    represent deeper sleep modes with lower power consumption but higher wake-up
    latency. The max_cstate parameter controls the deepest C-state that CPUs
    are allowed to enter.

    Common C-state values:
    - C0: CPU is active (not a sleep state)
    - C1: CPU is halted but can wake up almost instantly
    - C2: CPU caches are flushed, slightly longer wake-up time
    - C3+: Deeper sleep states with progressively longer wake-up times

    Setting max_cstate=1 restricts CPUs to only use C0 and C1 states,
    which can improve performance for latency-sensitive workloads at the
    cost of higher power consumption.
*)

exception Invalid_cstate of (int option * int option)

exception Invalid_cstate_string of string

exception Parse_cmd_output_failure of string

exception Command_failure of string

val to_string : int option * int option -> string
(** [to_string (cstate, sub_cstate)] converts the (cstate, sub_cstate) tuple to a string representation.
    
    @param cstate The maximum C-state value, [None] means unlimited
    @param sub_cstate The maximum C-substate value, [None] means unlimited
    @return A string representation of the (cstate, sub_cstate) tuple
    @raise Invalid_cstate if cstate < 0 or sub_cstate < 0 *)

val of_string : string -> int option * int option
(** [of_string s] parses a string representation of (cstate, sub_cstate) back into a tuple.
    
    @param s The string representation of (cstate, sub_cstate), e.g. "1,2" or "None,None"
    @return The parsed (cstate, sub_cstate) tuple, [None] means unlimited
    @raise Invalid_cstate_string if the string is not in the correct format *)

val xenpm_set : int option -> int option -> unit
(** [xenpm_set cstate sub_cstate] sets the maximum C-state and C-substate using the xenpm tool.
    This affects the runtime power management behavior immediately.
    
    @param cstate The maximum C-state value, [None] means unlimited
    @param sub_cstate The maximum C-substate value, [None] means unlimited
    @raise Invalid_cstate if cstate < 0 or sub_cstate < 0
    @raise Command_failure if the xenpm command fails
    @raise Parse_cmd_output_failure if the output of xenpm cannot be parsed *)

val xen_cmdline_set : int option -> int option -> unit
(** [xen_cmdline_set cstate sub_cstate] sets the max_cstate parameter in the Xen command line.
    This setting will take effect on the next reboot.
    
    @param cstate The maximum C-state value, [None] means unlimited
    @param sub_cstate The maximum C-substate value, [None] means unlimited
    @raise Invalid_cstate if cstate < 0 or sub_cstate < 0
    @raise Command_failure if the Xen command line cannot be updated *)

val xen_cmdline_get : unit -> int option * int option
(** [xen_cmdline_get ()] retrieves the current max_cstate setting from the Xen command line.
    
    @return The current (max_cstate, max_sub_cstate) in xen cmdline configuration, [None] means unlimited
    @raise Invalid_cstate if cstate < 0 or sub_cstate < 0
    @raise Command_failure if the Xen command line cannot be updated
    @raise Parse_cmd_output_failure if the Xen command line output cannot be parsed *)
