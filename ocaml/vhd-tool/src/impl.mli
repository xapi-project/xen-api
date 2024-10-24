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

val get :
     'a
  -> string option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val info : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val contents : 'a -> string option -> [> `Error of bool * string | `Ok of unit]

val create :
     Common.t
  -> string option
  -> string option
  -> string option
  -> [> `Error of bool * string | `Ok of unit]

val check :
  Common.t -> string option -> [> `Error of bool * string | `Ok of unit]

val stream :
  Common.t -> StreamCommon.t -> [> `Error of bool * string | `Ok of unit]

val serve :
     Common.t
  -> string
  -> int option
  -> string
  -> string option
  -> string
  -> int option
  -> string
  -> int64 option
  -> bool
  -> bool
  -> bool
  -> string option
  -> bool
  -> [> `Error of bool * string | `Ok of unit]

(** Functions used by sparse_dd *)

val make_stream :
     Common.t
  -> string
  -> string option
  -> string
  -> string
  -> Vhd_format_lwt.IO.fd Nbd_input.F.stream Lwt.t

val write_stream :
     Common.t
  -> Vhd_format_lwt.IO.fd F.stream
  -> string
  -> StreamCommon.protocol option
  -> bool
  -> (int64 -> int64 -> unit)
  -> string option
  -> string option
  -> Channels.verification_config option
  -> unit Lwt.t
