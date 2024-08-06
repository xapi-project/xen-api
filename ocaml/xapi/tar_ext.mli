(*
 * Copyright (c) Cloud Software Group, Inc.
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

type unpack_error =
  | Illegal_file_path of string
  | Unsupported_file_type of string
  | Unpacked_exceeds_max_size_limit of Int64.t
  | File_size_mismatch of {
        path: string
      ; expected_size: Int64.t
      ; actual_size: Int64.t
    }
  | File_incomplete
  | File_corrupted
  | Unpacking_failure

val unpack_error_to_string : unpack_error -> string

val unpack_tar_file :
     dir:string
  -> ifd:Unix.file_descr
  -> max_size_limit:int64
  -> (unit, unpack_error) result
(** [unpack_tar_file dir ifd max_size_limit] unpacks a tar file from file
    descriptor [ifd] to specific directory [dir]. The total size of all unpacked
    files should not exceed the maximum size limitation [max_size_limit].
    The function result is:
    {ul
        {- [Ok ()] if successful.}
        {- [Error unpack_error] otherwise. [unpack_error] indicates the
        failing reason.}} *)
