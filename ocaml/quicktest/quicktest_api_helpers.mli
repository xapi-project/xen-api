(*
 * Copyright (c) Cloud Software Group, Inc
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

val points_between : int64 -> int64 -> int64 Seq.t
(** [points_between a b] generate a sequence of test values between [a, b].
  Currently this generates [a*2**i], and always includes [b] exactly.
*)

val localhost_free_pages : Quicktest_trace.Scope.t -> int64
(** [localhost_free_pages scope] asks Xen for the actual free pages on the
local host. *)

val stable_localhost_free_pages :
  Quicktest_trace.Scope.t -> Client.Client.client -> host:API.ref_host -> int64
(** [stable_localhost_free_pages scope client ~host]
  waits until there no active tasks on [host], and then repeatedly queries
  [localhost_free_pages] with a delay inbetween until it stabilizes.
*)

val with_measure_memory_pages :
     Quicktest_trace.Scope.t
  -> Client.Client.client
  -> localhost:API.ref_host
  -> (unit -> unit)
  -> int64
(** [with_measure_memory_pages scope client ~localhost f]
  measures the amount of actual free memory on the host
  before and after calling [f ()], and returns the difference.
*)

val check_tasks : ('a, exn) result list -> 'a list
(** [check_tasks tasks] raises an exception if [tasks] contains errors. *)

val ensure_vm_clones :
     Client.Client.client
  -> vm:Quicktest_trace_api__Api.VM.dbref
  -> int
  -> string
  -> [`VM] API.Ref.t list
(** [ensure_vm_clones client ~vm n purpose] ensures that there are at least [n]
clones of [vm] for [purpose].
  This creates new VM clones as needed, or reuses existing ones.
  It ensures that existing VMs are halted.
*)

val pagesize : unit -> int64
(** [pagesize ()] the CPU's pagesize. On x86-64 this is always 4096. *)

val start_vm :
     Client.Client.client
  -> host:[< `host] Ref.t
  -> vm:Quicktest_trace_api__Api.VM.dbref
  -> unit
(** [start_vm client ~host ~vm] starts a single [vm] on [host], tracking progress. *)

val start_vms :
  Client.Client.client -> host:API.ref_host -> [`VM] API.Ref.t list -> unit
(** [start_vms client ~host vms] starts [vms] on [host], tracking progress.
  It attempts to perform a parallel start first, and if that fails,
  it attempts to start any remaining VMs sequentially.
  If the parallel start fails this always raises an exception at the end.
*)

val shutdown_vms :
  Client.Client.client -> Quicktest_trace_api__Api.VM.dbref list -> unit
(** [shutdown_vms client vms] hard shutdowns [vms], tracking progress. *)

val fill_mem_pow2 :
  Client.Client.client -> host:[`host] API.Ref.t -> vm:[`VM] API.Ref.t -> unit
(** [fill_mem_pow2 client ~host ~vm] fills the available memory on [host] in
  power of 2 increments, trying to ensure that the computed VM sizes sum up to exactly
  the amount of available free memory on the host, including VM memory overhead.
  It may not completely fill available memory due to rounding.
*)

val cleanup : (Rpc.call -> Rpc.response) -> API.ref_session -> unit -> unit
(** [cleanup rpc session ()] hard shutdowns all VMs used by these tests,
    and deletes the clones.

  For pause on fail to work, this should be a separate quicktest invoked before
  VM calibration tests.
*)
