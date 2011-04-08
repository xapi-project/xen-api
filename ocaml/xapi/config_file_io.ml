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
(* This file should probably be a script that we fork to read config files; write config files
   and generate hash of packaged config files.. For now just embedd this in xapi since
   we're only implementing this mechanism to address CA-7472 -- cannot change passwords
   on slaves *)

let _etc_passwd="/etc/passwd"
let read_config_files() =
  Unixext.string_of_file _etc_passwd
let rewrite_config_files s =
  Unixext.write_string_to_file _etc_passwd s (* a bit naughty, but will be fine.. ;) *)

(* essential we base64 this as md5 contains non-XML chars and the result of this fn goes
   straight onto the wire as the hash parameter of the Host.request_config_file_sync RPC *)
let hash_fn s = Base64.encode (Digest.string s) 
let compute_hash() =
  let config_files = read_config_files() in
  hash_fn config_files
let transmit_config_files s =
  let config_files = read_config_files() in
  let (_: int) = Unix.write s config_files 0 (String.length config_files) in
  ()
