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

(* if we already have a way to convert a type to Rpc.t we can use that to dump
   it, but in a readable way, not by using Rpc.to_string *)

let iter_alist f = List.iter @@ fun (k, v) -> f k v

let rec dump_rpc ppf =
  let open Fmt in
  let open Fmt.Dump in
  let open Rpc in
  function
  | Int i64 ->
      int64 ppf i64
  | Int32 i32 ->
      int32 ppf i32
  | Bool b ->
      bool ppf b
  | Float f ->
      float ppf f
  | String s ->
      string ppf s
  | DateTime s ->
      string ppf s
  | Enum lst ->
      list dump_rpc ppf lst
  | Dict alist ->
      dump_rpc_record ppf alist
  | Base64 b64 ->
      Fmt.pf ppf "base64:%s" b64
  | Null ->
      string ppf "null"

and dump_rpc_record ppf =
  (* Do not use Fmt.Dump here, we want more control over separators.
     This is similar to [Fmt.Dump.{record,field}], except we already have a
     list of fields to print.
  *)
  let open Fmt in
  let pp_pair = pair ~sep:(any " =@ ") string dump_rpc |> box ~indent:1 in
  braces (hovbox (iter_bindings ~sep:semi iter_alist pp_pair)) ppf

let dump typ_of t = Fmt.using (Rpcmarshal.marshal typ_of) dump_rpc t

let using ~aname to_other from_other ?test_data typ_of_other =
  let open Rpc.Types in
  let rpc_of v = v |> to_other |> Rpcmarshal.marshal typ_of_other
  and of_rpc rpc =
    rpc |> Rpcmarshal.unmarshal typ_of_other |> Result.map from_other
  in
  let test_data =
    match test_data with
    | Some t ->
        t
    | None ->
        Rpc_genfake.gentest typ_of_other |> List.rev_map from_other
  in
  Abstract {aname; test_data; rpc_of; of_rpc}

let serialize typ_of t = t |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string

let deserialize typ_of str =
  str
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal typ_of
  |> Rresult.R.open_error_msg

module StringMap = Map.Make (String)

type 'a dict = (string * 'a) list [@@deriving rpcty]

let stringmap_of_alist alist = alist |> List.to_seq |> StringMap.of_seq

let typ_of_stringmap typ_of_el =
  using ~aname:"stringmap" StringMap.bindings stringmap_of_alist
  @@ typ_of_dict typ_of_el

let string_to_file_exn path str =
  (* convenience wrapper because [write_string_to_file] takes 2 strings,
     this function makes it clearer which parameter is the file and which is
     the contents by using separate types.
     We use [write_string_to_file] which internally uses [atomic_write_to_file]
     and ensures that the file is [fsync]-ed on the disk, and would thus be
     expected to survive a crash (provided that the OS and disk controllers and
     disks work correctly).
  *)
  Xapi_stdext_unix.Unixext.write_string_to_file ~perms:0o600
    (Fpath.to_string path) str

let string_of_file_exn path =
  path |> Fpath.to_string |> Xapi_stdext_unix.Unixext.string_of_file

(* https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282 *)
let is_posix_filename_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' ->
      true
  | _ ->
      false

let max_filename_component = 255
(* on Linux *)

let max_filename_length = 4096

let max_path_component =
  String.init max_filename_component @@ fun i ->
  let c = Char.chr i in
  if is_posix_filename_char c then
    c
  else
    '_'

let max_path =
  List.init (max_filename_length / max_filename_component) (fun _ ->
      Fpath.v max_path_component
  )
  |> List.fold_left
       (fun acc e -> Fpath.(acc // e))
       (Fpath.v
       @@ String.sub max_path_component 0
       @@ (max_filename_length mod max_filename_component)
       )

let typ_of_path =
  let test_data =
    [
      Fpath.v "/"
    ; Fpath.v "a/b"
    ; Fpath.v "a/b.ext"
    ; Fpath.v max_path_component
    ; max_path
    ]
  in
  using ~aname:"path" Fpath.to_string Fpath.v ~test_data
    Rpc.Types.string.Rpc.Types.ty
