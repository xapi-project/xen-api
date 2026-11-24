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

(** Detaching gpumon from NVML around an operation, as
    [Xapi_gpumon.with_gpumon_stopped] does when
    [Xapi_globs.nvidia_gpumon_detach] is set. *)

(** How [Xapi_gpumon.with_gpumon_stopped] keeps gpumon off the card while a
    VM with GPUs starts *)
type how =
  | Stop_gpumon  (** stop gpumon, then start it again *)
  | Detach_nvml  (** keep gpumon running, detached from NVML *)

val how : detach_switch:bool -> pass_through:bool -> how
(** [how ~detach_switch ~pass_through] is [Detach_nvml] only when the switch
    ([Xapi_globs.nvidia_gpumon_detach]) is on and the VM has no pass-through
    GPU. A pass-through start always stops gpumon: the card must be free when
    xenopsd takes it, and with the detach, pass-through starts failed with
    "Couldn't lock GPU" (CP-41126). *)

module type NVML = sig
  val is_attached : unit -> bool
  (** Whether gpumon is attached to NVML. Raises if gpumon does not answer. *)

  val detach : unit -> unit
  (** Detach gpumon from NVML, releasing the NVIDIA devices. *)

  val attach : unit -> unit
  (** Attach gpumon to NVML again. *)
end

module Make : functor (_ : NVML) -> sig
  val with_detached : (unit -> 'a) -> 'a
  (** [with_detached f] detaches NVML if it is attached, runs [f], then
      attaches it again. If gpumon does not answer, [f] runs without a
      detach. If NVML is attached again by the time [f] has finished (gpumon
      was restarted, e.g. by sriov-manage), it is not attached a second time.
      The re-attach never raises: [f]'s result or exception is what the
      caller gets. If the detach raises, a warning is logged and [f] runs
      anyway; NVML is attached again afterwards only if it is detached. Each
      call to gpumon can wait up to its client's RPC timeout (10 s) when
      gpumon is not answering. For vGPU starts only: see [how]. *)
end
