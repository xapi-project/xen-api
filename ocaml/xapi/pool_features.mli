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
(** Module that controls feature restriction.
 * @group Licensing
*)

(** Check whether a given feature is currently enabled on the pool. *)
val is_enabled : __context:Context.t -> Features.feature -> bool

(** Raise appropriate exception if feature is not enabled. *)
val assert_enabled : __context:Context.t -> f:Features.feature -> unit

(** Update the pool-level restrictions list in the database. *)
val update_pool_features : __context:Context.t -> unit

