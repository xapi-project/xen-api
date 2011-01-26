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
 
(* Slightly outdated comment:
 *
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
 * impossible to start new domains.
 *)

open Stringext
open Pervasiveext
open License

module D = Debug.Debugger(struct let name="license" end)
open D

exception LicenseParseError
exception LicenseCannotReadFile
exception LicenseFieldMissing of string
exception License_expired of license
exception License_file_deprecated

(* Set from config file: *)
(* ...but only in xapi, not in the v6 daemon!!! *)
let filename = ref "/etc/xensource/license"

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
      Unixext.string_of_fd fd)
      
(* only activation keys are accepted as license files since XS 5.6 *)
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
		{sku = Edition.to_string Edition.Free;
		 version = readfld "version" attrs;
		 serialnumber = readfld "serialnumber" attrs;
		 sockets = int_of_string (readfld "sockets" attrs); 
	  	 productcode = (readfld "productcode" attrs);
		 expiry = float_of_string (readfld "expiry" attrs);
		 grace = "no";
		 name = maybe_readfld "name" attrs;
		 company = maybe_readfld "company" attrs;
		 address1 = maybe_readfld "address1" attrs;
		 address2 = maybe_readfld "address2" attrs;
		 city = maybe_readfld "city" attrs;
		 state = maybe_readfld "state" attrs;
		 postalcode = maybe_readfld "postalcode" attrs;
		 country = maybe_readfld "country" attrs;
		 sku_marketing_name = Edition.to_marketing_name Edition.Free;
		}
  | _ -> raise LicenseParseError

(* only activation keys are accepted as license files since XS 5.6 *)
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

(* only activation keys are accepted as license files since XS 5.6 *)
let do_parse_and_validate fname =
  try
    let _ = try Unix.access fname [Unix.F_OK] with _ -> raise LicenseCannotReadFile in
    let license_data = validate_signature fname in
    let newlicense = parse_license license_data in

    (if not (License.check_expiry newlicense) then raise (License_expired newlicense));

    (* At this point, license is valid and hasn't expired *)
    newlicense
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

