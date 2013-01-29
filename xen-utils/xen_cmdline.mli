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
(** Helper module to view, modify and delete Xen command-line options *)

(** Gives a list of CPU feature masks currently set. *)
val list_cpuid_masks : unit -> (string * string) list

(** Sets CPU feature masks. *)
val set_cpuid_masks : (string * string) list -> string

(** Removes CPU feature masks. *)
val delete_cpuid_masks : string list -> string list

