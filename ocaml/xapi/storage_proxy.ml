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

(* This file should be auto-generated from storage_interface.
   Corrollary: don't add anything which can't be auto-generated from storage_interface! *)

open Storage_interface

module type RPC = sig
	val rpc : Rpc.call -> Rpc.response
end

module Proxy = functor(RPC: RPC) -> struct
	type context = Smint.request

	module Client = Client(RPC)

    let query _ = Client.query
	module DP = struct
		let create _ = Client.DP.create
		let destroy _ = Client.DP.destroy
		let diagnostics _ = Client.DP.diagnostics
	end
	module SR = struct
		let attach _ = Client.SR.attach
		let detach _ = Client.SR.detach
		let reset _ = Client.SR.reset
		let destroy _ = Client.SR.destroy
		let scan _ = Client.SR.scan
		let list _ = Client.SR.list
	end
	module VDI = struct
		let attach _ = Client.VDI.attach
		let activate _ = Client.VDI.activate
		let deactivate _ = Client.VDI.deactivate
		let detach _ = Client.VDI.detach

		let stat _ = Client.VDI.stat

		let create _ = Client.VDI.create
		let destroy _ = Client.VDI.destroy
	end
end
