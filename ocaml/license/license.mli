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
(** Module that keeps track of licenses and related matter
 * @group Licensing
 *)

(** Type for holding all details about a license *)
type license =
    {
      sku       : string;
      (** License type: currently either [XE Express] (free) or [XE Enterprise] (paid for) *)
      version   : string;
      (** No longer used *)
      serialnumber : string;
      (** No longer used *)
      sockets   : int;
      (** No longer used *)
      productcode : string;
      (** No longer used *)
      expiry : float;
      (** Expiry date (result of Unix.time) *)
      grace : string;
      (** Indicates whether the current license is a grace license.
       *  Possible values: "no", "upgrade grace", "regular grace" *)

      name : string;
      (** No longer used *)
      company : string;
      (** No longer used *)
      address1 : string;
      (** No longer used *)
      address2 : string;
      (** No longer used *)
      city : string;
      (** No longer used *)
      state : string;
      (** No longer used *)
      postalcode : string;
      (** No longer used *)
      country : string;
      (** No longer used *)

      sku_marketing_name : string;
      (** Official marketing name of the license *)
    }

val license : license ref
(** The current license *)

val filename : string ref
(** Path to the license file in use. Only free-license activation keys are
 *  used since XS 6.0 *)

exception Missing_license_param of string
(** Thrown if we fail to find a license param *)
exception LicenseParseError
(** Thrown if the license data is malformed *)
exception LicenseCannotReadFile
(** Thrown if the given license file cannot be opened *)
exception LicenseFieldMissing of string
(** Thrown if a particular field is missing from the license data *)
exception License_expired of license
(** Thrown if the license in a given file is found to be expired *)
exception License_file_deprecated
(** Thrown if an old-style, deprecated license file is used *)

val sku_and_name_of_edition : string -> string * string
(** map an edition string to an sku name and a marketing name *)

val to_assoc_list : license -> (string * string) list
(** Converts a license into a association list to place in DB. *)
val of_assoc_list : (string * string) list -> license
(** Converts a license association list from DB into a license value. *)
val default : unit -> license
(** Returns a default, free license with 30-day grace *)

val license_valid : unit -> bool
(** Check whether the current license is valid or expired.
 *  Called from xapi/license_check.ml. *)
val read_license_file : string -> license option
(** Parse a license file and return the result. Only works for free-license
 *  activation keys since XS 6.0.
 *  Called from host.license_apply. *)
val do_parse_and_validate : string -> unit
(** As read_license_file, but also set license state variable if not expired.
 *  Only works for free-license activation keys since XS 6.0.
 *  Called from host.license_apply. *)

val initialise : __context:Context.t -> host:[ `host ] Ref.t -> unit
(** Initialises licensing on xapi start up *)

