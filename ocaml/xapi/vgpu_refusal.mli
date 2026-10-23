(*
 * Copyright (C) Cloud Software Group
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

(** Why a PGPU, or a GPU group, cannot accommodate a VGPU of some type.

    Payloads are the strings the API error is built from rather than
    references, so a reason can be built and inspected without a database. *)

type t =
  | Type_not_supported of {vgpu_type: string; supported: string list}
      (** the type is not among the PGPU's [supported_VGPU_types] *)
  | Type_not_enabled of {vgpu_type: string; enabled: string list}
      (** supported by the PGPU, but not among its [enabled_VGPU_types] *)
  | Type_incompatible_with_running of {
        pgpu: string
      ; vgpu_type: string
      ; running: string
    }
      (** some VGPU already allocated on the PGPU does not list this type in
          its [compatible_types_on_pgpu]. [running] is the allocated types,
          deduplicated and joined with ";", as the existing error carries
          them. *)
  | Insufficient_capacity of {pgpu: string; vgpu_type: string}
      (** the type's size does not fit in what is left of the PGPU *)
  | Group_contains_no_pgpus of {gpu_group: string}
      (** asked of a GPU group that holds no PGPUs at all, so there is no
          per-PGPU refusal to report instead *)
  | Unexpected of exn
      (** any other exception raised while deciding, such as a database
          failure or a division by a size-[0L] type *)

val to_api_error : t -> exn
(** The API error a refusal is reported as. [to_api_error (Unexpected e)] is
    [e]. *)

val to_string : t -> string
(** A one-line description for logs. *)
