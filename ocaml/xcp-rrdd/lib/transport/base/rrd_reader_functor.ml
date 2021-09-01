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
  (** An identifier needed to open the resource. *)
  type id_t

  (** A handle to an open resource. *)
  type state_t

  val init : id_t -> state_t
  (** Open a resource for writing, given its identifier. *)

  val cleanup : id_t -> state_t -> unit
  (** Cleanup an open resource when it is no longer needed. *)

  val expose : state_t -> Cstruct.t
  (** Given the state of the open resource, expose its contents as a Cstruct. *)
end

type reader = {
    read_payload: unit -> Rrd_protocol.payload
  ; cleanup: unit -> unit
}

module Make (T : TRANSPORT) = struct
  let create id protocol =
    let state = ref (T.init id) in
    let reader = protocol.Rrd_protocol.make_payload_reader () in
    let is_open = ref true in
    let read_payload () =
      if !is_open then
        let cs =
          if Cstruct.len (T.expose !state) <= 0 then
            state := T.init id ;
          T.expose !state
        in
        reader cs
      else
        raise Rrd_io.Resource_closed
    in
    let cleanup () =
      if !is_open then (
        T.cleanup id !state ;
        is_open := false
      ) else
        raise Rrd_io.Resource_closed
    in
    {read_payload; cleanup}
end
