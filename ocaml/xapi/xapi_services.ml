(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(** Advertise services
 *)

module D=Debug.Debugger(struct let name="xapi" end)
open D

open Fun
open Stringext

(* URI prefix *)
let _services = "services"
let _SM = "SM"

let path xs = "/" ^ (String.concat "/" xs)

let convert_driver_info x =
	let open Smint in {
		Storage_interface.Driver_info.uri = path [ _services; _SM; x.sr_driver_filename ];
		name = x.sr_driver_name;
		description = x.sr_driver_description;
		vendor = x.sr_driver_vendor;
		copyright = x.sr_driver_copyright;
		version = x.sr_driver_version;
		required_api_version = x.sr_driver_required_api_version;
		capabilities = x.sr_driver_text_capabilities;
		configuration = x.sr_driver_configuration
	}

let list_sm_drivers ~__context =
	let all = List.map (convert_driver_info ++ Sm.info_of_driver) (Sm.supported_drivers ()) in
	Storage_interface.Driver_info.rpc_of_ts all

let respond req rpc s =
	let txt = Jsonrpc.to_string rpc in
	Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ());
	req.Http.Request.close <- true;
	Unixext.really_write s txt 0 (String.length txt)

let handler (req: Http.Request.t) s _ =
	Xapi_http.with_context ~dummy:true "Querying services" req s
		(fun __context ->
			debug "uri = %s" req.Http.Request.uri;
			match String.split '/' req.Http.Request.uri with
				| [ ""; services; "SM"; driver ] when services = _services ->
					begin
						try
							respond req (Storage_interface.Driver_info.rpc_of_t (convert_driver_info (Sm.info_of_driver driver))) s
						with _ ->
							Http_svr.headers s (Http.http_404_missing ~version:"1.0" ());
							req.Http.Request.close <- true
					end
				| [ ""; services; "SM" ] when services = _services ->
					let rpc = list_sm_drivers ~__context in
					respond req rpc s
				| [ ""; services ] when services = _services ->
					let q = {
						Storage_interface.name = "XCP";
						vendor = "xen.org";
						version = "0.1";
						features = List.map (fun x -> path [_services; x]) [ _SM ];
					} in
					respond req (Storage_interface.rpc_of_query_result q) s
				| _ ->
					Http_svr.headers s (Http.http_404_missing ~version:"1.0" ());
					req.Http.Request.close <- true
		)
