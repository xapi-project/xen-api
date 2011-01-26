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
		sku       : string;		(** License type *)
		version   : string;		(** No longer used *)
		serialnumber : string;	(** No longer used *)
		sockets   : int;		(** No longer used *)
		productcode : string;	(** No longer used *)
		expiry : float;			(** Expiry date (result of Unix.time) *)
		grace : string;			(** Indicates whether the current license is a grace license.
								 *  Possible values: "no", "upgrade grace", "regular grace" *)

		name : string;			(** No longer used *)
		company : string;		(** No longer used *)
		address1 : string;		(** No longer used *)
		address2 : string;		(** No longer used *)
		city : string;			(** No longer used *)
		state : string;			(** No longer used *)
		postalcode : string;	(** No longer used *)
		country : string;		(** No longer used *)

		sku_marketing_name : string;	(** Official marketing name of the license *)
	}

(** Converts a license into a association list to place in DB. *)
val to_assoc_list : license -> (string * string) list

(** Converts a license association list from DB into a license value. *)
val of_assoc_list : (string * string) list -> license

(** Returns a default, free license with 30-day grace. *)
val default : unit -> license

(** Check whether a given license is valid or expired. *)
val check_expiry : license -> bool

(** Thrown if we fail to find a license param. *)
exception Missing_license_param of string

(** Obtain a date that lies 30 days in the future to set as grace expiry date. *)
val grace_expiry : unit -> float

(** Obtain a date that lies 30 days in the future to set as upgrade grace expiry date. *)
val upgrade_grace_expiry : unit -> float

