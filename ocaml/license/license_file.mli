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
(** Handling of license files
 * @group Licensing
 *)

(** Path to the license file in use. *)
val filename : string ref

(** Parse a license file and return the result.
 *  Called from host.license_apply. *)
val read_license_file : string -> License.license option

(** As read_license_file, but also set license state variable if not expired.
 *  Called from host.license_apply. *)
val do_parse_and_validate : string -> License.license


(** Thrown if the license data is malformed. *)
exception LicenseParseError

(** Thrown if the given license file cannot be opened. *)
exception LicenseCannotReadFile

(** Thrown if a particular field is missing from the license data. *)
exception LicenseFieldMissing of string

(** Thrown if the license in a given file is found to be expired. *)
exception License_expired of License.license

(** Thrown if an old-style, deprecated license file is used. *)
exception License_file_deprecated
