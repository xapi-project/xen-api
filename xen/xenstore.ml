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
let client = Client.make ()

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
}

    let ops h = {
        read = Client.read h;
		directory = Client.directory h;
        write = Client.write h;
        writev = (fun base_path -> List.iter (fun (k, v) -> Client.write h (base_path ^ "/" ^ k) k));
        mkdir = Client.mkdir h;
        rm = Client.rm h;
        setperms = Client.setperms h;
        getdomainpath = Client.getdomainpath h;
        watch = Client.watch h;
        unwatch = Client.unwatch h;
        introduce = Client.introduce h;
    }
    let with_xs f = Client.with_xs client (fun h -> f (ops h))
    let wait f = Client.wait client (fun h -> f (ops h))
	let transaction _ f = Client.with_xst client (fun h -> f (ops h))

	let daemon_open _ = failwith "daemon_open"

	let close _ = failwith "close"

	let getdomainpath _ _ = failwith "getdomainpath"
end

module Xst = Xs
