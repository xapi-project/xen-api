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
(* OLD:
 * There are 2 ways that a license can be registered by xapi. The
 * first is on startup, when the file /etc/xensource/license is read,
 * and the second is the api call 'host.license_apply'. There are also
 * 2 possible problems with a license - that it might have expired, and
 * that it might be invalid. The API call will refuse to do anything if
 * the license has either problem, but initial startup will always
 * apply a parsable license, even if it has expired.
 * 
 * The license is checked to see if it has expired in exactly one place
 * - Vmops.create_check, and so an expired license will mean it is
 * impossible to start new domains.  *)

open Stringext
open Pervasiveext

module D = Debug.Debugger(struct let name="license" end)
open D

(* Set from config file: *)
let filename = ref ""

(* Defaults *)
let sku = "XE Enterprise"

(* round a date, given by Unix.time, to days *)
let round_to_days d =
	let days = (int_of_float d) / (24 * 3600) in
	(float_of_int days) *. 24. *. 3600.
	
let grace30days () =
	if Xapi_fist.reduce_grace_period () then
		Unix.time () +. 15. *. 60. (* 15 minutes in the future *)
	else
		round_to_days (Unix.time () +. 30. *. 24. *. 60. *. 60.) (* 30 days in the future *)

let grace4days () =
	if Xapi_fist.reduce_upgrade_grace_period () then
		Unix.time () +. 15. *. 60. (* 15 minutes in the future *)
	else
		round_to_days (Unix.time () +. 4. *. 24. *. 60. *. 60.) (* 4 days in the future *)

let default_version = Version.product_version
let default_sockets = 1
let default_productcode = ""

let sku_and_name_of_edition = function
| _ -> "undefined", "Undefined"

(* Convert the sku_type encoded and signed in the license into a sku_marketing_name by looking up the
   key in an XML table stored in the dom0. *)
let marketing_string_of_sku sku = 
  try
    let xml = Unixext.read_whole_file_to_string Xapi_globs.sku_marketing_name_db in
    let db = Hashtbl_xml.of_xml (Xmlm.input_of_string xml) in
    if Hashtbl.mem db sku
    then Hashtbl.find db sku
    else begin
      warn "marketing_string_of_sku %s: no corresponding entry in the sku_marketing_name db; defaulting to \"\"" sku;
      ""
    end
  with 
  | Unix.Unix_error(Unix.ENOENT, _, _) -> warn "marketing_string_of_sku %s: sku_marketing_name db missing; defaulting to \"\"" sku; ""
  | Hashtbl_xml.Unmarshall_error x -> warn "marketing_string_of_sku %s: Caught error unmarshalling sku_marketing_name db: %s; defaulting to \"\""  sku x; ""
  | e -> warn "marketing_string_of_sku %s: Caught unknown exception unmarshalling sku_marketing_name db: %s; defaulting to \"\"" sku (Printexc.to_string e); ""


(* Only read out the fields we care about. The signature covers the other
   fields so there's no verification required here. *)
type license =
    {
      sku       : string;
      version   : string;
      serialnumber : string;
      sockets   : int;
      productcode : string;
      expiry : float;  (* Using the result of Unix.time for this field atm *)
      grace : bool; (* indicates whether the current license is a grace license *)

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
    _grace, string_of_bool x.grace;
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

(** Thrown if we fail to find a license param *)
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
    grace = false; (* NOTE: 'grace' key left out for backwards compatibility *)
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
      sku = sku;
      version = default_version;
      serialnumber = "";
      sockets = default_sockets;
      productcode = default_productcode;
      expiry = grace30days ();
      grace = false;
      name = "";
      company = "";
      address1 = "";
      address2 = "";
      city = "";
      state = "";
      postalcode = "";
      country = "";
      sku_marketing_name = marketing_string_of_sku sku;
    }

let license : license ref = ref (default ())

exception LicenseParseError
exception LicenseCannotReadFile
exception LicenseFieldMissing of string
exception License_expired of license
exception License_file_deprecated

(* Calls to obtain info about license *)

let check_expiry l =
  Unix.time () < l.expiry

let license_valid () = check_expiry !license

(* License setting functions *)
	
let validate_signature fname = 
  Gpg.with_signed_cleartext fname
    (fun fingerprint fd -> 
      (match fingerprint with 
	| Some f -> 
	    (* base64-encoded fingerprint of our licensing public key *)
	    if (Base64.encode f)<>"QzA5Qzk4REIwNjM4RjNFQjZEQUFERkU4QTJCRjA0QkM3QThDNzhBNw==" 
	    then
	      (
		debug "Got fingerprint: %s" f;
		(* debug "Encoded: %s" (Base64.encode f); -- don't advertise the fact that we've got an encoded string in here! *)
		raise Gpg.InvalidSignature
	      )
	| None -> 
	    debug "No fingerprint!";
	    raise Gpg.InvalidSignature);
      Unixext.read_whole_file 500 500 fd)
      
(* only activation keys are accepted as license files since XS 6.0 *)
let parse_license license_data =
  let lic_xml = Xml.parse_string license_data in

  let readfld fname attrs =
    try
      List.assoc fname attrs
    with Not_found -> raise (LicenseFieldMissing fname) in

  let maybe_readfld fname attrs =
    try
      List.assoc fname attrs
    with Not_found -> "" in

  match lic_xml with
  | Xml.Element("xe_license", attrs, _) ->
	let sku = readfld "sku_type" attrs in
	(* we now only accept activation keys for the free edition fo XS *)
	if sku <> "XE Express" then
		raise License_file_deprecated
	else
		{sku = sku;
		 version = readfld "version" attrs;
		 serialnumber = readfld "serialnumber" attrs;
		 sockets = int_of_string (readfld "sockets" attrs); 
	  	 productcode = (readfld "productcode" attrs);
		 expiry = float_of_string (readfld "expiry" attrs);
		 grace = false;
		 name = maybe_readfld "name" attrs;
		 company = maybe_readfld "company" attrs;
		 address1 = maybe_readfld "address1" attrs;
		 address2 = maybe_readfld "address2" attrs;
		 city = maybe_readfld "city" attrs;
		 state = maybe_readfld "state" attrs;
		 postalcode = maybe_readfld "postalcode" attrs;
		 country = maybe_readfld "country" attrs;
		 sku_marketing_name = marketing_string_of_sku sku;
		}
  | _ -> raise LicenseParseError

(* only activation keys are accepted as license files since XS 6.0 *)
let read_license_file fname =
  try
    Unix.access fname [Unix.F_OK];
    let license_data = validate_signature fname in
    let newlicense = parse_license license_data in
      Some newlicense
  with 
  | License_file_deprecated -> raise License_file_deprecated
  | e ->
    begin
      debug "Failed to read license file: %s" (Printexc.to_string e);
      None
    end

(* only activation keys are accepted as license files since XS 6.0 *)
let do_parse_and_validate fname =
  try
    let _ = try Unix.access fname [Unix.F_OK] with _ -> raise LicenseCannotReadFile in
    let license_data = validate_signature fname in
    let newlicense = parse_license license_data in

    (if not (check_expiry newlicense) then raise (License_expired newlicense));

    (* At this point, license is valid and hasn't expired *)
    license := newlicense
  with e ->
    (match e with
      | License_expired l -> warn "License has expired"
      | LicenseCannotReadFile -> warn "License application failed: cannot read license file."
      | Gpg.InvalidSignature -> warn "License application failed: invalid signature on license file."
      | LicenseFieldMissing fname -> warn "License application failed: essential field '%s' missing from license." fname
      | LicenseParseError -> warn "License application failed: reverting to previous license"
      | License_file_deprecated -> warn "License application failed: deprecated license file"
      | e -> warn "License application failed: exception '%s' in license parsing." (Printexc.to_string e);
	  log_backtrace ());
    raise e

let write_grace_to_file grace_expiry =
	let grace_expiry_str = string_of_float grace_expiry in
	Unixext.write_string_to_file Xapi_globs.upgrade_grace_file grace_expiry_str

let read_grace_from_file () =
	try
		let grace_expiry_str = Unixext.read_whole_file_to_string Xapi_globs.upgrade_grace_file in
		float_of_string grace_expiry_str
	with _ -> 0.

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
	let existing_license_params = Db.Host.get_license_params ~__context ~self:host in
	let existing_edition = Db.Host.get_edition ~__context ~self:host in
	let default = default () in
	let new_license = try 
		let existing_license = of_assoc_list existing_license_params in
		match existing_edition with
		| "free" ->
			(* old Floodgate-free behaviour *)
			begin try
				do_parse_and_validate !filename;
				info "Existing free license with expiry date %s still in effect." (Date.to_string (Date.of_float !license.expiry));
				!license (* do_parse_and_validate already sets !license *)
			with
			| License_expired l -> l (* keep expired license *)
			| _ ->
				(* activation file does not exist or is invalid *)
				if existing_license.expiry < default.expiry then begin
					info "Existing free license with expiry date %s still in effect." (Date.to_string (Date.of_float existing_license.expiry));
					{default with expiry = existing_license.expiry}
				end else begin
					info "Generating new free license, which needs to be activated in 30 days.";
					default
				end
			end
		| "enterprise" | "platinum" ->
			(* existing license is a v6 Essentials license -> try to check one out again *)
			V6client.get_v6_license ~__context ~host ~edition:existing_edition;
			begin match !V6client.licensed with 
			| None ->
				let upgrade_grace = read_grace_from_file () > Unix.time () in
				if upgrade_grace then begin
					info "No %s license is available, but we are still in the upgrade grace period." existing_edition;
					{existing_license with grace = true}
				end else begin
					info "No %s license is available. Essentials features have been disabled." existing_edition;
					{existing_license with expiry = 0.} (* expiry date 0 means 01-01-1970, so always expired *)
				end
			| Some license ->
				info "Successfully checked out %s license." existing_edition;
				(* delete upgrade-grace file, if it exists *)
				Unixext.unlink_safe Xapi_globs.upgrade_grace_file;
				if !V6client.grace then
					{existing_license with grace = true; expiry = !V6client.expires}
				else
					{existing_license with grace = false; expiry = !V6client.expires}
			end
		| "" -> 
			(* upgrade from pre-MNR *)
			if existing_license.sku = "XE Express" then begin
				info "Upgrade from free: set to free edition.";
				(* all existing license_params are kept; only fill in edition field *)
				Db.Host.set_edition ~__context ~self:host ~value:"free";
				{default with expiry = existing_license.expiry}
			end else begin
				info "Upgrade from Essentials: transition to enterprise edition (4-day grace license).";
				Db.Host.set_edition ~__context ~self:host ~value:"enterprise";
				let grace_expiry = grace4days () in
				write_grace_to_file grace_expiry;
				Unixext.unlink_safe !filename;
				let sku, name = sku_and_name_of_edition "enterprise" in
				{default with sku = sku; expiry = grace_expiry; grace = true; sku_marketing_name = name}
			end
		| _ ->
			warn "Edition field corrupted; generating a new free license, which needs to be activated in 30 days.";
			default
		with _ ->
			(* no license_params -> first boot *)
			Db.Host.set_edition ~__context ~self:host ~value:"free";
			begin try
				do_parse_and_validate !filename;
				info "Found a free-license activation key with expiry date %s." (Date.to_string (Date.of_float !license.expiry));
				!license (* do_parse_and_validate already sets !license *)
			with
			| License_expired l -> l (* keep expired license *)
			| _ ->
				(* activation file does not exist or is invalid *)
				info "Generating new free license, which needs to be activated in 30 days.";
				default
			end
	in
	license := new_license
	
