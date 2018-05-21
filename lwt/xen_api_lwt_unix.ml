(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Xen_api
open Lwt

module Lwt_unix_IO = struct

  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return
  let (>>) m n = m >>= fun _ -> n

  type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel
  type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel
  type conn = Lwt_unix.file_descr

  let read_line (_, ic) = Lwt_io.read_line_opt ic

  let read (_, ic) count =
    Lwt.catch
      (fun () -> Lwt_io.read ~count ic)
      (function
        | End_of_file -> return ""
        | e -> Lwt.fail e)

  (* let read_exactly (_, ic) buf off len =
    Lwt.catch
      (fun () -> Lwt_io.read_into_exactly ic buf off len >> return true)
      (function
        | End_of_file -> return false
        | e -> Lwt.fail e)

  let read_exactly ic len =
    let buf = Bytes.create len in
    read_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None *)

  let write (_, oc) = Lwt_io.write oc

  (* let write_line (_, oc) = Lwt_io.write_line oc *)

  let flush (_, oc) = Lwt_io.flush oc

  let close ((close1, _), (close2, _)) =
    close1 () >> close2 ()

  let sslctx =
    Ssl.init ();
    Ssl.create_context Ssl.SSLv23 Ssl.Client_context

  let open_connection uri =
    let domain_addr_t = match Uri.host uri with
      | Some host ->
        Lwt.catch
          (fun () ->
             Lwt_unix.gethostbyname host >>= fun host_entry ->
             return (host_entry.Lwt_unix.h_addrtype, host_entry.Lwt_unix.h_addr_list.(0)))
          (fun _ -> fail (Failed_to_resolve_hostname host))
      | None -> fail (Failed_to_resolve_hostname "") in
    (match Uri.scheme uri with
     | Some "http" -> return false
     | Some "https" -> return true
     | Some "file" -> return false
     | Some x -> fail (Unsupported_scheme x)
     | None -> fail (Unsupported_scheme "")) >>= fun ssl ->
    let port = match Uri.port uri with
      | Some x -> x
      | None -> if ssl then 443 else 80 in
    (match Uri.scheme uri with
     | Some "file" ->
       return (Unix.PF_UNIX, Unix.ADDR_UNIX (Uri.path uri))
     | Some "http" | Some "https" ->
       domain_addr_t >>= fun (domain, addr) ->
       return (domain, Unix.ADDR_INET(addr, port))
     | _ -> assert false) >>= fun (domain, sockaddr) ->

    if ssl then begin
      let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt.catch
        (fun () ->
           Lwt.catch (fun () ->
               Lwt_unix.connect fd sockaddr
             ) (fun e ->
               Lwt_unix.close fd >>= fun () -> Lwt.fail e
             )
           >>= fun () ->
           Lwt_ssl.ssl_connect fd sslctx >>= fun sock ->
           let ic = Lwt_ssl.in_channel_of_descr sock in
           let oc = Lwt_ssl.out_channel_of_descr sock in
           return (Ok ((return, ic), ((fun () -> Lwt_ssl.close sock), oc))))
        (fun e -> return (Error e))
    end else begin
      let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt.catch
        (fun () ->
           Lwt.catch (fun () ->
               Lwt_unix.connect fd sockaddr
             ) (fun e ->
               Lwt_unix.close fd >>= fun () -> Lwt.fail e
             )
           >>= fun () ->
           let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input fd in
           let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.output fd in
           return (Ok (((fun () -> Lwt_io.close ic), ic), ((fun () -> Lwt_io.close oc), oc))))
        (fun e -> return (Error e))
    end

  let sleep = Lwt_unix.sleep

  let gettimeofday = Unix.gettimeofday
end

module M = Make(Lwt_unix_IO)

let exn_to_string = function
  | Api_errors.Server_error(code, params) ->
    Printf.sprintf "%s %s" code (String.concat " " params)
  | e -> Printexc.to_string e

let do_it uri string =
  let uri = Uri.of_string uri in
  let connection = M.make uri in
  Lwt.finalize
    (fun () ->
       M.rpc connection string >>= fun result ->
       match result with
       | Ok x -> return x
       | Error e ->
         Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
         fail e)
    (fun () -> M.disconnect connection)

(* TODO: modify do_it to accept the timeout and remove the warnings *)

[@@@ocaml.warning "-27"]
let make ?(timeout=30.) uri call =
  let string = Xmlrpc.string_of_call call in
  do_it uri string >>= fun result ->
  Lwt.return (Xmlrpc.response_of_string result)

[@@@ocaml.warning "-27"]
let make_json ?(timeout=30.) uri call =
  let string = Jsonrpc.string_of_call call in
  do_it uri string >>= fun result ->
  Lwt.return (Jsonrpc.response_of_string result)

module Client = Client.ClientF(Lwt)
include Client

