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
open Xenops_utils

module D = Debug.Make(struct let name = "xenstore" end)
open D

module Client = Xs_client_unix.Client(Xs_transport_unix_client)
let make_client () =
	try
		Client.make ()
	with e ->
		error "Failed to connect to xenstore. The raw error was: %s" (Printexc.to_string e);
		begin match e with
		| Unix.Unix_error(Unix.EACCES, _, _) ->
			error "Access to xenstore was denied.";
			let euid = Unix.geteuid () in
			if euid <> 0 then begin
				error "My effective uid is %d." euid;
				error "Typically xenstore can only be accessed by root (uid 0).";
				error "Please switch to root (uid 0) and retry."
			end
		| Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
			error "Access to xenstore was refused.";
			error "This normally indicates that the service is not running.";
			error "Please start the xenstore service and retry."
		| _ -> ()
		end;
		raise e

let get_client =
	let client = ref None in
	fun () -> match !client with
		| None ->
			let c = make_client () in
			client := Some c;
			c
		| Some c -> c

type domid = int

module Xs = struct
	type domid = int

	type xsh = {
(*
        debug: string list -> string;
*)
        directory : string -> string list;
        read : string -> string;
(*
        readv : string -> string list -> string list;
*)
        write : string -> string -> unit;
        writev : string -> (string * string) list -> unit;
        mkdir : string -> unit;
        rm : string -> unit;
(*
        getperms : string -> perms;
        setpermsv : string -> string list -> perms -> unit;
        release : domid -> unit;
        resume : domid -> unit;
*)
        setperms : string -> Xs_protocol.ACL.t -> unit;

        getdomainpath : domid -> string;
        watch : string -> string -> unit;
        unwatch : string -> string -> unit;
        introduce : domid -> nativeint -> int -> unit;
        set_target : domid -> domid -> unit;
}

    let ops h = {
        read = Client.read h;
		directory = Client.directory h;
        write = Client.write h;
        writev = (fun base_path -> List.iter (fun (k, v) -> Client.write h (base_path ^ "/" ^ k) v));
        mkdir = Client.mkdir h;
        rm = (fun path -> try Client.rm h path with Xs_protocol.Enoent _ -> ());
        setperms = Client.setperms h;
        getdomainpath = Client.getdomainpath h;
        watch = Client.watch h;
        unwatch = Client.unwatch h;
        introduce = Client.introduce h;
        set_target = Client.set_target h;
    }
    let with_xs f = Client.with_xs (get_client ()) (fun h -> f (ops h))
    let wait f = Client.wait (get_client ()) (fun h -> f (ops h))
	let transaction _ f = Client.with_xst (get_client ()) (fun h -> f (ops h))

end

module Xst = Xs

let with_xs = Xs.with_xs
