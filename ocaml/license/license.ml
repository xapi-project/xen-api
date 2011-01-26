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

open Stringext
open Pervasiveext

module D = Debug.Debugger(struct let name="license" end)
open D

(* Defaults *)
let edition = Edition.Free

(* Round a date, given by Unix.time, to days *)
let round_to_days d =
	let days = (int_of_float d) / (24 * 3600) in
	(float_of_int days) *. 24. *. 3600.
	
(* Obtain a date that lies 30 days in the future to set as grace expiry date *)
let grace_expiry () =
	if Xapi_fist.reduce_grace_period () then
		Unix.time () +. 15. *. 60. (* 15 minutes in the future *)
	else
		round_to_days (Unix.time () +. 30. *. 24. *. 60. *. 60.) (* 30 days in the future *)

(* Obtain a date that lies 30 days in the future to set as upgrade grace expiry date *)
let upgrade_grace_expiry () =
	if Xapi_fist.reduce_upgrade_grace_period () then
		Unix.time () +. 15. *. 60. (* 15 minutes in the future *)
	else
		round_to_days (Unix.time () +. 30. *. 24. *. 60. *. 60.) (* 30 days in the future *)

let default_version = Version.product_version
let default_sockets = 1
let default_productcode = ""

(* Only read out the fields we care about. The signature covers the other
   fields so there's no verification required here. *)
type license =
    {
      sku       : string;
      version   : string;
      serialnumber : string;
      sockets   : int;
      productcode : string;
      expiry : float;
      grace : string;

      name : string;
      company : string;
      address1 : string;
      address2 : string;
      city : string;
      state : string;
      postalcode : string;
      country : string;

      sku_marketing_name : string; (* calculated only on the host with the license file, copied otherwise *)
    }

(* String constants used for converting the license record to/from string*string association lists *)
let _sku_type = "sku_type"
let _version = "version"
let _serialnumber = "serialnumber"
let _sockets = "sockets"
let _productcode = "productcode"
let _expiry = "expiry"
let _grace = "grace"
let _name = "name"
let _company = "company"
let _address1 = "address1"
let _address2 = "address2"
let _city = "city"
let _state = "state"
let _postalcode = "postalcode"
let _country = "country"
let _sku_marketing_name = "sku_marketing_name"

let to_assoc_list (x: license) = 
  [ _sku_type, x.sku;
    _version, x.version;
    _serialnumber, x.serialnumber;
    _sockets, string_of_int x.sockets;
    _productcode, x.productcode;
    _expiry, Date.to_string (Date.of_float x.expiry);
    _grace, x.grace;
    _name, x.name;
    _company, x.company;
    _address1, x.address1;
    _address2, x.address2;
    _city, x.city;
    _state, x.state;
    _postalcode, x.postalcode;
    _country, x.country;
    _sku_marketing_name, x.sku_marketing_name;
  ]

exception Missing_license_param of string

(** Takes an association list (eg from Host.license_params) and returns a license record. This may throw 
    Missing_license_param if the key is absent *)
let of_assoc_list (x: (string * string) list) = 
  let find k = if List.mem_assoc k x then List.assoc k x else raise (Missing_license_param k) in
  { sku = find _sku_type;
    version = find _version;
    serialnumber = find _serialnumber;
    sockets = (try int_of_string (find _sockets) with _ -> 1); (* sockets are now irrelevant *)
    productcode = find _productcode;
    expiry = (Date.to_float (Date.of_string (find _expiry)));
    grace = "no"; (* NOTE: 'grace' key left out for backwards compatibility *)
    name = find _name;
    company = find _company;
    address1 = find _address1;
    address2 = find _address2;
    city = find _city;
    state = find _state;
    postalcode = find _postalcode;
    country = find _country;
    (* NB: it would be dangerous to use this host's sku_marketing_name db to resolve another host's sku *)
    sku_marketing_name = (try find _sku_marketing_name with Missing_license_param _ -> "");
  }
    
let default () =
    { 
      sku = Edition.to_string edition;
      version = default_version;
      serialnumber = "";
      sockets = default_sockets;
      productcode = default_productcode;
      expiry = grace_expiry ();
      grace = "no";
      name = "";
      company = "";
      address1 = "";
      address2 = "";
      city = "";
      state = "";
      postalcode = "";
      country = "";
      sku_marketing_name = Edition.to_marketing_name edition;
    }

(* Calls to obtain info about license *)

let check_expiry l = 
	Unix.time () < l.expiry

