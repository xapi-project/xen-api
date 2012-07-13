(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Debugger(struct let name = "config_shared" end)
open D

let disable_logging_for =
	"disable-logging-for", Config.String
		(fun x ->
			try
				let modules = String.split_f String.isspace x in
				List.iter
					(fun x ->
						debug "Disabling logging for: %s" x;
						Debug.disable x
					) modules
			with e ->
				error "Processing disabled-logging-for = %s" x;
				log_backtrace ()
		)
