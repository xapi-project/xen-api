(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Lwt

module Int64Map = Map.Make(struct type t = int64 let compare = Int64.compare end)

type t = Cstruct.t Int64Map.t

let empty = Int64Map.empty

let sector_size = 512

let empty_sector = Bytes.make sector_size '\000'

let write t ofs cstr = Int64Map.add ofs cstr t
let write_string t ofs s =
    let cstr = Cstruct.create (String.length s) in
    Cstruct.blit_from_string s 0 cstr 0 (String.length s);
    write t ofs cstr

let of_file filename =
  Lwt_unix.openfile filename [ Unix.O_RDONLY ] 0o0 >>= fun fd ->
  let buf = Bytes.make sector_size '\000' in
  let rec loop i result =
    Lwt_unix.read fd buf 0 sector_size >>= fun n ->
    let finished = n <> sector_size in
    let result = if buf <> empty_sector then begin
        let sector = Cstruct.create n in
        Cstruct.blit_from_bytes buf 0 sector 0 n;
        Int64Map.add i sector result
      end else result in
    if finished then Lwt.return result
    else loop (Int64.add i 1L) result
  in
  loop 0L Int64Map.empty >>= fun result ->
  Lwt_unix.close fd >>= fun () ->
  return result

let print_ocaml out t =
  Printf.fprintf out "let disk =\n";
  Printf.fprintf out "  let t = ref Disk.empty in\n";
  Int64Map.iter
    (fun ofs cstr ->
      let buf = Bytes.make (Cstruct.length cstr) '\000' in
      Cstruct.blit_to_bytes cstr 0 buf 0 (Cstruct.length cstr);
      Printf.fprintf out "  t := Disk.write_string !t %LdL \"%s\";\n"
        ofs (buf |> Bytes.to_string |> String.escaped)
    ) t;
  Printf.fprintf out "  !t\n"

