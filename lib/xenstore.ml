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

module Client = Xs_client_unix.Client(Xs_transport_unix_client)
let make_client () =
  try
    Client.set_logger (fun s -> Logs.debug (fun m -> m "Xs_client_unix: %s" s));
    Client.make ()
  with e ->
    Logs.err (fun m -> m "Failed to connect to xenstore. The raw error was: %s" (Printexc.to_string e));
    begin match e with
      | Unix.Unix_error(Unix.EACCES, _, _) ->
        Logs.err (fun m -> m "Access to xenstore was denied.");
        let euid = Unix.geteuid () in
        if euid <> 0 then begin
          Logs.err (fun m -> m "My effective uid is %d." euid);
          Logs.err (fun m -> m "Typically xenstore can only be accessed by root (uid 0).");
          Logs.err (fun m -> m "Please switch to root (uid 0) and retry.")
        end
      | Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
        Logs.err (fun m -> m "Access to xenstore was refused.");
        Logs.err (fun m -> m "This normally indicates that the service is not running.");
        Logs.err (fun m -> m "Please start the xenstore service and retry.")
      | _ -> ()
    end;
    raise e

let client = ref None

let get_client () = match !client with
  | None ->
    let c = make_client () in
    client := Some c;
    c
  | Some c -> c

let forget_client () = client := None

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

    (* Compound operations not corresponding one-to-one with those of Client *)
    mkdirperms : string -> Xs_protocol.ACL.t -> unit;
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

    mkdirperms = (fun path -> (Client.mkdir h path; Client.setperms h path));
  }
  let with_xs f = Client.immediate (get_client ()) (fun h -> f (ops h))
  let wait f = Client.wait (get_client ()) (fun h -> f (ops h))
  let transaction _ f = Client.transaction (get_client ()) (fun h -> f (ops h))

end

module Xst = Xs
include Xs
