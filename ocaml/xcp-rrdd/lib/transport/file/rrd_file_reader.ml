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

module File = struct
  (** Filesystem path. *)
  type id_t = string

  type state_t = {path: string; mutable mapping: Cstruct.t}

  let map path =
    let fd = Unix.openfile path [Unix.O_RDONLY] 0o400 in
    if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then failwith "lseek" ;
    let mapping =
      Bigarray.(
        array1_of_genarray @@ Unix.map_file fd char c_layout false [|-1|]
      )
    in
    Unix.close fd ;
    Cstruct.of_bigarray mapping

  let init path = {path; mapping= map path}

  let cleanup _ _ = ()

  (* A writer that adds datasources extends the file in place, past the end
     of a mapping taken earlier, so the data and metadata the reader must
     then re-read would be cut short. *)
  let expose state =
    let size =
      try (Unix.stat state.path).Unix.st_size with Unix.Unix_error _ -> 0
    in
    if size > Cstruct.length state.mapping then state.mapping <- map state.path ;
    state.mapping
end

include Rrd_reader_functor.Make (File)
