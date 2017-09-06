(*
 * Copyright (C) Citrix Systems Inc.
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

(** Find VDA reference associated to a specific VM *)
val find_vda:  __context:Context.t
  -> vm:[ `VM ] Ref.t
  -> [ `VDA ] Ref.t option

val create: __context:Context.t
          -> vm:[ `VM ] Ref.t
          -> version:string
          -> [ `VDA ] Ref.t

val destroy: __context:Context.t
           -> self:[ `VDA ] Ref.t
           -> unit

(** Copy VDA record metadata to a new VDA attached to the specified VM *)
val copy: __context:Context.t
        -> vm:[ `VM ] Ref.t
        -> [ `VDA ] Ref.t
        -> [ `VDA ] Ref.t

(** VDA.get_status API call implementation *)
val get_status: __context:Context.t 
              -> self:[ `VDA ] Ref.t
              -> string

(** VDA.get_log_report API call implementation *)
val get_log_report: __context:Context.t
                  -> self:[ `VDA ] Ref.t
                  -> string
