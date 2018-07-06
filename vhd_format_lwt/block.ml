(*
 * Copyright (C) Citrix Systems Inc.
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

module M = Vhd_format.F.From_file(IO)
open M

type 'a io = 'a Lwt.t

type error = Mirage_block.error

let pp_error = Mirage_block.pp_error

type write_error = Mirage_block.write_error

let pp_write_error = Mirage_block.pp_write_error

type page_aligned_buffer = Cstruct.t

type info = Mirage_block.info

type t = {
  mutable vhd: IO.fd Vhd_format.F.Vhd.t option;
  info: info;
  id: string;
}

let connect path =
  Lwt_unix.LargeFile.stat path >>= fun _ ->
  Lwt.catch
    (fun () -> Lwt_unix.access path [ Lwt_unix.W_OK ] >>= fun () -> return true)
    (fun _ -> return false)
  >>= fun read_write ->
  Vhd_IO.openchain path read_write >>= fun vhd ->
  let open Vhd_format.F in
  let sector_size = 512 in
  let size_sectors = Int64.div vhd.Vhd.footer.Footer.current_size 512L in
  let info = Mirage_block.{ read_write; sector_size; size_sectors } in
  let id = path in
  return ({ vhd = Some vhd; info; id })

let disconnect t = match t.vhd with
  | None -> return ()
  | Some vhd ->
    Vhd_IO.close vhd >>= fun () ->
    t.vhd <- None;
    return ()

let get_info t = return t.info

let to_sectors bufs =
  let rec loop acc remaining =
    if Cstruct.len remaining = 0 then List.rev acc else
    let available = min 512 (Cstruct.len remaining) in
    loop (Cstruct.sub remaining 0 available :: acc) (Cstruct.shift remaining available) in
  List.concat (List.map (loop []) bufs)

let forall_sectors f offset bufs =
  let rec one offset = function
  | [] -> return ()
  | b :: bs -> f offset b >>= fun () -> one (Int64.succ offset) bs in
  one offset (to_sectors bufs)

let zero =
  let buf = Cstruct.create 512 in
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done;
  buf

let read t offset bufs = match t.vhd with
  | None -> return (Rresult.R.error `Disconnected)
  | Some vhd ->
    forall_sectors
      (fun offset sector ->
        ( Vhd_IO.read_sector vhd offset sector >>= function
          | false -> Cstruct.blit zero 0 sector 0 512; return ()
          | true -> return () ) >>= fun () ->
        return ()
      ) offset bufs >>= fun () ->
    return (Rresult.R.ok ())

let write t offset bufs = match t.vhd with
  | None -> return (Rresult.R.error `Disconnected)
  | Some vhd -> Vhd_IO.write vhd offset bufs >>= fun () -> return (Rresult.R.ok ())
