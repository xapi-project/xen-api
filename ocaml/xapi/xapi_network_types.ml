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
(** Common code between the fake and real servers for dealing with Networks.
 * @group Networking
 *)

(** Helper type *)
type backend = 
    {
      attach: __context:Context.t -> self:API.ref_network -> unit;
      (** Function to attach a Network *)
      
      detach: string -> unit;
      (** Function to detach a Network *)
    }

