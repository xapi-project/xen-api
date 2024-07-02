(*
 * Copyright (C) 2023 Cloud Software Group
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

type (+!'a, +!'b) props = {rw: 'a; kind: 'b}

type rdonly = [`rdonly]

type wronly = [`wronly]

type rdwr = [`rdwr]

let pp_rw fmt =
  Fmt.of_to_string
    (function #rdonly -> "RDONLY" | #wronly -> "WRONLY" | #rdwr -> "RDWR")
    fmt

type reg = [`reg]

type blk = [`blk]

type chr = [`chr]

type dir = [`dir]

type lnk = [`lnk]

type fifo = [`fifo]

type sock = [`sock]

type kind = [reg | blk | chr | dir | lnk | fifo | sock]

let to_unix_kind =
  let open Unix in
  function
  | #reg ->
      S_REG
  | #blk ->
      S_BLK
  | #chr ->
      S_CHR
  | #dir ->
      S_DIR
  | #lnk ->
      S_LNK
  | #fifo ->
      S_FIFO
  | #sock ->
      S_SOCK

let of_unix_kind =
  let open Unix in
  function
  | S_REG ->
      `reg
  | S_BLK ->
      `blk
  | S_CHR ->
      `chr
  | S_DIR ->
      `dir
  | S_LNK ->
      `lnk
  | S_FIFO ->
      `fifo
  | S_SOCK ->
      `sock

let pp_kind fmt = Fmt.using to_unix_kind Safefd.pp_kind fmt

let pp fmt =
  Fmt.(
    record
      ~sep:Fmt.(any ", ")
      [field "rw" (fun t -> t.rw) pp_rw; field "kind" (fun t -> t.kind) pp_kind]
  )
    fmt

type readable = [rdonly | rdwr]

type writable = [wronly | rdwr]

type rw = [rdonly | wronly | rdwr]

type (+!'a, +!'b) t = (([< rw] as 'a), ([< kind] as 'b)) props

let as_readable ({rw= #readable; _} as t) = t

let as_writable ({rw= #writable; _} as t) = t

let as_readable_opt = function
  | {rw= #readable; _} as x ->
      Some x
  | {rw= #wronly; _} ->
      None

let as_writable_opt = function
  | {rw= #writable; _} as x ->
      Some x
  | {rw= #rdonly; _} ->
      None

type espipe = [fifo | sock]

let as_kind_opt expected ({kind; _} as t) =
  (* we cannot compare the values directly because we want to keep the type parameters distinct *)
  match (kind, expected) with
  | #reg, #reg ->
      Some {t with kind= expected}
  | #blk, #blk ->
      Some {t with kind= expected}
  | #chr, #chr ->
      Some {t with kind= expected}
  | #dir, #dir ->
      Some {t with kind= expected}
  | #lnk, #lnk ->
      Some {t with kind= expected}
  | #fifo, #fifo ->
      Some {t with kind= expected}
  | #sock, #sock ->
      Some {t with kind= expected}
  | #kind, #kind ->
      None

type seekable = [reg | blk]

type truncatable = reg

let make rw kind = {rw; kind}
