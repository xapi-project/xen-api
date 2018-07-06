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

type cdrom_drive_status =
  | NO_INFO
  | NO_DISC
  | TRAY_OPEN
  | DRIVE_NOT_READY
  | DISC_OK

let string_of_cdrom_drive_status = function
  | NO_INFO         -> "NO_INFO"
  | NO_DISC         -> "NO_DISC"
  | TRAY_OPEN       -> "TRAY_OPEN"
  | DRIVE_NOT_READY -> "DRIVE_NOT_READY"
  | DISC_OK         -> "DISC_OK"

type cdrom_disc_status =
  | DISC_NO_INFO
  | DISC_NO_DISC
  | AUDIO
  | DATA_1
  | DATA_2
  | XA_2_1
  | XA_2_2
  | MIXED

let string_of_cdrom_disc_status = function
  | DISC_NO_INFO -> "DISC_NO_INFO"
  | DISC_NO_DISC -> "DISC_NO_DISC"
  | AUDIO   -> "AUDIO"
  | DATA_1  -> "DATA_1"
  | DATA_2  -> "DATA_2"
  | XA_2_1  -> "XA_2_1"
  | XA_2_2  -> "XA_2_2"
  | MIXED   -> "MIXED"

external _query_cdrom_drive_status : Unix.file_descr -> cdrom_drive_status = "stub_CDROM_DRIVE_STATUS"
external _query_cdrom_disc_status : Unix.file_descr -> cdrom_disc_status = "stub_CDROM_DISC_STATUS"
external _query_cdrom_mcn : Unix.file_descr -> string = "stub_CDROM_GET_MCN"

let with_cdrom (name: string) f =
  let fd = Unix.openfile name [ Unix.O_RDONLY; Unix.O_NONBLOCK ] 0 in
  try
    let result = f fd in
    Unix.close fd;
    result
  with e ->
    Unix.close fd;
    raise e

let query_cdrom_status (name: string) : (cdrom_drive_status * cdrom_disc_status) =
  with_cdrom name (fun fd ->
      let status = _query_cdrom_drive_status fd in
      let disc = _query_cdrom_disc_status fd in
      status, disc
    )

let query_cdrom_drive_status (name: string) : cdrom_drive_status =
  with_cdrom name _query_cdrom_drive_status

let query_cdrom_mcn (name: string) : string = with_cdrom name _query_cdrom_mcn

