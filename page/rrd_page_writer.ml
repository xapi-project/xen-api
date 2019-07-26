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

type interdomain_id = {
  backend_domid: int;
  shared_page_count: int;
}

module Page = struct
  open Gnt

  type id_t = interdomain_id

  (** list of shared pages *)
  type info_t = int list
  type state_t = Gntshr.share

  let init {backend_domid; shared_page_count} =
    let share =
      Gntshr.with_gntshr
        (fun gntshr ->
           Gntshr.share_pages_exn gntshr backend_domid shared_page_count false)
    in
    share.Gntshr.refs, share

  let cleanup _ _ share =
    Gntshr.with_gntshr
      (fun gntshr -> Gntshr.munmap_exn gntshr share)

  (** The allocator returns a Cstruct mapping all of the shared memory, unless
      	 *  the size requested is greater than the size of this memory in which case
      	 *  the allocator fails. *)
  let get_allocator share =
    let alloc_cstruct size =
      let c = Io_page.to_cstruct share.Gntshr.mapping in
      if size > Cstruct.len c then
        failwith "not enough memory";
      c
    in
    alloc_cstruct
end

include Rrd_writer_functor.Make(Page)
