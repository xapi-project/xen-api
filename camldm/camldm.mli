(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type devty = | Dereferenced of string | Real of string
type dev = { device : devty; offset : int64; }
type stripety = { chunk_size : int64; dests : dev array; }
type mapty = Linear of dev | Striped of stripety
type mapping = { start : int64; len : int64; map : mapty; }
type mapping_array = {
  m : mapping array 
} 
type status = {
  exists : bool;
  suspended : bool;
  live_table : bool;
  inactive_table : bool;
  open_count : int32;
  event_nr : int32;
  major : int32;
  minor : int32;
  read_only : bool;
  targets : (int64 * int64 * string * string) list;
}

exception CreateError of string
exception ReloadError of string

val rpc_of_mapping_array : mapping_array -> Rpc.t
val mapping_array_of_rpc : Rpc.t -> mapping_array
val convert_mapty : mapty -> (string * string) list -> string * string
val create : string -> mapping array -> (string * string) list -> unit
val reload : string -> mapping array -> (string * string) list -> unit
val suspend : string -> unit
val resume : string -> unit
val remove : string -> unit
val table : string -> status
val mknods : string -> unit
val mknod : string -> int -> int -> int -> unit
val get_sector_pos_of : mapping -> int64 -> (string * string) list -> string * int64
val to_string : mapping array -> string
val of_string : string -> mapping array

val rpc_of_status : status -> Rpc.t
val ls : unit -> (string list) option
