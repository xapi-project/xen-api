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
let _ = Debug.disable "foo"
module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

open Xs_protocol
module Client = Xs_client_unix.Client(Xs_transport_unix_client)

let client =
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

