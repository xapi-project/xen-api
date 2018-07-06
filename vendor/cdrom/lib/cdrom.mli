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

val string_of_cdrom_drive_status : cdrom_drive_status -> string

type cdrom_disc_status =
  | DISC_NO_INFO
  | DISC_NO_DISC
  | AUDIO
  | DATA_1
  | DATA_2
  | XA_2_1
  | XA_2_2
  | MIXED

val string_of_cdrom_disc_status : cdrom_disc_status -> string

val query_cdrom_status : string -> cdrom_drive_status * cdrom_disc_status
(** [query_cdrom_status device] returns the state of both the drive
    and any disc currently inside. *)

val query_cdrom_drive_status : string -> cdrom_drive_status
(** [query_cdrom_drive_status device] returns the state of the drive
    only. *)

val query_cdrom_mcn : string -> string
(** [query_cdrom_mcn device] returns the "Universal Product Code" if
    available. *)
