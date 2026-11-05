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

(** The GPU partition helper's reply for one card, as the four cases xapi acts
    on. Only the partition mode and the case are read strictly. *)

(** Why the partitions of a card could not be read, when xapi finds it *)
type xapi_kind =
  | Helper_absent
  | Helper_not_executable
  | Caller_timeout
  | Helper_crashed
  | Decode_failed
  | Card_busy  (** another read of the card still runs *)

type reason =
  | Helper of Gpumon_interface.Partition_helper.Kind.t
  | Xapi of xapi_kind

val string_of_reason : reason -> string
(** The reason's name, spelt as the helper's reply spells kinds *)

(** Readers match the cases as [{mode; _}]: later fields are added, not edited *)
type t =
  | Partitions of {mode: Gpu.Partition_mode.mode}
  | None_carved of {mode: Gpu.Partition_mode.mode}
      (** can be divided, is not *)
  | Unreadable of {mode: Gpu.Partition_mode.mode option; reason: reason}
      (** [mode] is [Some] when it was read *)
  | Not_applicable  (** cannot be divided *)

(** One card's decoded reply; later members are added by their readers *)
type reply = private {probe: t}

(** What the helper printed and its exit code, [None] on a signal *)
type output = {exit: int option; stdout: string}

(** What one run of the helper gives: its output, or why there is none *)
type run_result = (output, xapi_kind) result

val helper_args : page_size:int -> string -> string list
(** The helper's arguments for the card at a PCI address *)

val axes : t -> Gpu.Partition_mode.mode option list
(** The argument of [Xapi_pgpu.update_partition_mode] *)

val decode : bdf:string -> page_size:int -> run_result -> reply
(** The reply of a run of the helper for the card at [bdf], or why there is none *)
