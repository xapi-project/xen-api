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

module type TRANSPORT = sig
  type id_t

  type state_t

  val init: id_t -> state_t

  val cleanup: id_t -> state_t -> unit

  val expose: state_t -> Cstruct.t
end

type interdomain_id = {
  frontend_domid: int;
  shared_page_refs: int list;
}

type reader = {
  read_payload: unit -> Rrd_protocol.payload;
  cleanup: unit -> unit;
}

module Make (T: TRANSPORT) : sig
  val create: T.id_t -> Rrd_protocol.protocol -> reader
end

module FileReader : sig
  val create: string -> Rrd_protocol.protocol -> reader
end

module PageReader : sig
  val create: interdomain_id -> Rrd_protocol.protocol -> reader
end
