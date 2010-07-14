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

open License

module D = Debug.Debugger(struct let name="license" end)
open D

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
				License_file.do_parse_and_validate !License_file.filename;
				info "Existing free license with expiry date %s still in effect." (Date.to_string (Date.of_float !license.expiry));
				!license (* do_parse_and_validate already sets !license *)
			with
			| License_file.License_expired l -> l (* keep expired license *)
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
		| "enterprise" | "platinum" | "enterprise-xd" ->
			(* existing license is a v6 Essentials license -> try to check one out again *)
			begin try
				V6client.get_v6_license ~__context ~host ~edition:existing_edition;
			with _ -> error "The license-server connection details (address or port) were missing or incomplete." end;
			begin match !V6client.licensed with 
			| None ->
				let upgrade_grace = read_grace_from_file () > Unix.time () in
				if upgrade_grace then begin
					info "No %s license is available, but we are still in the upgrade grace period." existing_edition;
					{existing_license with grace = "upgrade grace"}
				end else begin
					info "No %s license is available. Essentials features have been disabled." existing_edition;
					{existing_license with expiry = 0.} (* expiry date 0 means 01-01-1970, so always expired *)
				end
			| Some license ->
				info "Successfully checked out %s license." existing_edition;
				(* delete upgrade-grace file, if it exists *)
				Unixext.unlink_safe Xapi_globs.upgrade_grace_file;
				if !V6client.grace then begin
					Grace_retry.retry_periodically host existing_edition;
					{existing_license with grace = "regular grace"; expiry = !V6client.expires}
				end else
					{existing_license with grace = "no"; expiry = !V6client.expires}
			end
		| "" -> 
			(* upgrade from pre-MNR *)
			if existing_license.sku = "XE Express" then begin
				info "Upgrade from free: set to free edition.";
				(* all existing license_params are kept; only fill in edition field *)
				Db.Host.set_edition ~__context ~self:host ~value:"free";
				{default with sku = "free"; expiry = existing_license.expiry}
			end else begin
				info "Upgrade from Essentials: transition to enterprise edition (30-day grace license).";
				Db.Host.set_edition ~__context ~self:host ~value:"enterprise";
				let expiry = upgrade_grace_expiry () in
				write_grace_to_file expiry;
				Unixext.unlink_safe !License_file.filename;
				V6alert.send_v6_upgrade_grace_license ();
				let name = Edition.to_marketing_name Edition.Enterprise in
				{default with sku = "enterprise"; expiry = expiry; grace = "upgrade grace"; sku_marketing_name = name}
			end
		| _ ->
			warn "Edition field corrupted; generating a new free license, which needs to be activated in 30 days.";
			default
		with _ ->
			(* no license_params -> first boot *)
			Db.Host.set_edition ~__context ~self:host ~value:"free";
			begin try
				License_file.do_parse_and_validate !License_file.filename;
				info "Found a free-license activation key with expiry date %s." (Date.to_string (Date.of_float !license.expiry));
				!license (* do_parse_and_validate already sets !license *)
			with
			| License_file.License_expired l -> l (* keep expired license *)
			| _ ->
				(* activation file does not exist or is invalid *)
				info "Generating new free license, which needs to be activated in 30 days.";
				default
			end
	in
	license := new_license
	
