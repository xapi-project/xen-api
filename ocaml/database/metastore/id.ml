(*
 * Copyright (C) Cloud Software Group, Inc.
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

type t = Uuidm.t

let compare = Uuidm.compare

module Map = struct
  include Map.Make (Uuidm)

  let dump pp_val =
    Fmt.Dump.iter_bindings iter (Fmt.any "Id.Map") Uuidm.pp pp_val
end

module Set = struct
  include Set.Make (Uuidm)

  let dump = Fmt.Dump.iter iter (Fmt.any "Id.Set") Uuidm.pp
end
