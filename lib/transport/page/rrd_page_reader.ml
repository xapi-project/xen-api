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

type interdomain_id = {frontend_domid: int; shared_page_refs: int list}

module Mutex = struct
  include Mutex

  let execute lock f =
    Mutex.lock lock ;
    let result = try f () with e -> Mutex.unlock lock ; raise e in
    Mutex.unlock lock ; result
end

module Page = struct
  open Gnt

  let interface_ref : Gnttab.interface option ref = ref None

  let interface_m = Mutex.create ()

  let with_interface f =
    Mutex.execute interface_m (fun () ->
        let interface =
          match !interface_ref with
          | Some interface ->
              interface
          | None ->
              let interface = Gnttab.interface_open () in
              interface_ref := Some interface ;
              interface
        in
        f interface)

  (** Remote domid * list of grant references. *)
  type id_t = interdomain_id

  type state_t = Gnttab.Local_mapping.t

  let init {frontend_domid; shared_page_refs} =
    let grants =
      List.map
        (fun ref -> {Gnttab.domid= frontend_domid; Gnttab.ref})
        shared_page_refs
    in
    let mapping_opt =
      with_interface (fun gnttab -> Gnttab.mapv gnttab grants false)
    in
    match mapping_opt with
    | Some mapping ->
        mapping
    | None ->
        failwith "failed to map shared page(s)"

  let cleanup _ mapping =
    with_interface (fun gnttab -> Gnttab.unmap_exn gnttab mapping)

  let expose mapping =
    let buf = Gnttab.Local_mapping.to_buf mapping in
    Io_page.to_cstruct buf
end

include Rrd_reader_functor.Make (Page)
