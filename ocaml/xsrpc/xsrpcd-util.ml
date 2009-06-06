(* XSRPC small fork&exec utility. *)

open Printf
open Stringext

let () =
	let rpc domid queryid cmd data =
		let exec = sprintf "/opt/xensource/rpc/fe-%s" cmd in
		if (try Unix.access exec [ Unix.X_OK ]; true with _ -> false) then (
			try
				let output, stderr = Forkhelpers.execute_command_get_output exec [ data ] in
				Some (Xsrpc.Success, output)
			with _ ->
				Some (Xsrpc.Error, "cmd failed")
		) else (
			Some (Xsrpc.Error, sprintf "cmd %s: not available" cmd) 
		)
		in
	Xsrpc.listen "fe" rpc
