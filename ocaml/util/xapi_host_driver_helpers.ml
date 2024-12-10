(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

module J = Yojson
open Angstrom

let defer finally = Fun.protect ~finally

let int n = Int32.to_int n

let ( // ) = Filename.concat

(** Read a (small) file into a string *)
let read path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  let size = in_channel_length ic in
  really_input_string ic size

type note = {typ: int32; name: string; desc: string}

module JSON = struct
  let note l =
    let l =
      List.map
        (fun d ->
          `Assoc
            [
              ("type", `Int (int d.typ))
            ; ("name", `String d.name)
            ; ("desc", `String d.desc)
            ]
        )
        l
    in
    `List l

  let emit json = J.pretty_to_channel stdout json
end

(** return the smallest k >= n such that k is divisible by 4 *)
let align4 n =
  let ( & ) = Int.logand in
  n + (-n & 3)

(** advance the cursor to position n *)
let advance_to n =
  let* pos in
  advance (max 0 (n - pos))

(** align the cursor to a multiple of 4 *)
let align =
  let* pos in
  advance_to (align4 pos)

(** parse an ELF note entry; it assumes that name and desc are null
    terminated strings. This should be always true for name but desc
    depends on the entry. We don't capture the terminating zero for
    strings. *)
let note =
  let* name_length = LE.any_int32 in
  let* desc_length = LE.any_int32 in
  let* typ = LE.any_int32 in
  let* name = take (int name_length - 1) in
  (* skip over terminating null and re-align cursor *)
  let* _ = char '\000' in
  let* () = align in
  let* desc = take (int desc_length - 1) in
  (* skip over terminating null and re-align cursor *)
  let* _ = char '\000' in
  let* () = align in
  return {typ; name; desc}

(** parser for a sequence of note entries *)
let notes = many note

(** parse a sequence of note entries from a string *)
let parse str =
  let consume = Consume.Prefix in
  parse_string ~consume notes str

let get_version path =
  let version =
    read path
    |> parse
    |> Result.map
       @@ List.filter_map (fun note ->
              match (note.typ, note.name) with
              | 1l, "XenServer" ->
                  Some note.desc
              | _ ->
                  None
          )
  in
  match version with
  | Ok (v :: _) ->
      Ok v
  | _ ->
      Error
        (Format.sprintf
           "Failed to parse %s, didn't find a XenServer driver version notes \
            section"
           path
        )

let get_notes path =
  let version = read path |> parse in
  match version with
  | Ok (_ :: _) as v ->
      v
  | _ ->
      Error
        (Format.sprintf "Failed to parse %s, didn't find a notes section" path)

let dump_notes prefix =
  let notes_dir = prefix // "notes" in
  try
    let lst =
      Sys.readdir notes_dir
      |> Array.to_list
      |> List.map (fun n -> read (notes_dir // n))
      |> List.filter_map (fun note_str -> Result.to_option (parse note_str))
      |> List.map (fun note -> (prefix, JSON.note note))
    in
    JSON.emit (`Assoc lst)
  with _ -> ()
