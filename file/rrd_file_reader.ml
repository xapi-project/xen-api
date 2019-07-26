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
  type state_t = Cstruct.t

  let init path =
    let fd = Unix.openfile path [Unix.O_RDONLY] 0o400 in
    if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
      failwith "lseek";
    let mapping = Bigarray.(array1_of_genarray @@ Unix.map_file fd char
                              c_layout false [|-1|]) in
    Unix.close fd;
    Cstruct.of_bigarray mapping

  let cleanup _ _ = ()

  let expose cstruct = cstruct
end

include Rrd_reader_functor.Make(File)
