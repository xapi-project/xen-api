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

(** Makes a new file in the same directory as 'otherfile' *)
let temp_file_in_dir otherfile =
  let base_dir = Filename.dirname otherfile in
  let rec keep_trying () = 
    try 
      let uuid = Uuidm.to_string (Uuidm.create `V4) in
      let newfile = base_dir ^ "/" ^ uuid in
      Unix.close (Unix.openfile newfile [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_EXCL] 0o600);
      newfile
    with
      Unix.Unix_error (Unix.EEXIST, _, _)  -> keep_trying ()
  in
  keep_trying ()



