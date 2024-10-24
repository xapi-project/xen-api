(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module F : module type of Vhd_format.F.From_file (Vhd_format_lwt.IO)

val raw :
  ?extent_reader:string -> 'a -> string -> string -> int64 -> 'a F.stream Lwt.t

val vhd :
     ?extent_reader:string
  -> Vhd_format_lwt.IO.fd Vhd_format.F.Raw.t
  -> string
  -> string
  -> int64
  -> Vhd_format_lwt.IO.fd F.stream Lwt.t
