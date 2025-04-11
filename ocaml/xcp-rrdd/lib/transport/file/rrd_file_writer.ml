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

type local_id = {path: string; shared_page_count: int}

let finally f finally = Fun.protect ~finally f

module File = struct
  let page_size = 4096

  (** Filesystem path. *)
  type id_t = local_id

  (** Filesystem path is returned to the caller for future reference. *)
  type info_t = string

  (** fd for writing to the shared file. *)
  type state_t = Cstruct.t

  let init {path; shared_page_count} =
    let size = shared_page_count * page_size in
    let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
    finally
      (fun () ->
        let mapping =
          Bigarray.(
            array1_of_genarray @@ Unix.map_file fd char c_layout true [|size|]
          )
        in
        let cstruct = Cstruct.of_bigarray mapping in
        (path, cstruct)
      )
      (fun () -> Unix.close fd)

  let cleanup _ path _ = Unix.unlink path

  (** This assumes there's no limit to the size of file which can be used. *)
  let get_allocator cstruct =
    let alloc_cstruct size =
      if size > Cstruct.length cstruct then
        failwith "not enough memory" ;
      cstruct
    in
    alloc_cstruct
end

include Rrd_writer_functor.Make (File)
