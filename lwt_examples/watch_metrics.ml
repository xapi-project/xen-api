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

open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"
let start = ref 0
let interval = ref 5

let exn_to_string = function
  | Api_errors.Server_error(code, params) ->
    Printf.sprintf "%s %s" code (String.concat " " params)
  | e -> Printexc.to_string e

let main () =
  let rpc = make !uri in
  Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.0" ~originator:"watch_metrics"
  >>= fun session_id ->
  Lwt.finalize
    (fun () ->
       Host.get_all ~rpc ~session_id >>= fun hosts ->
       let host = List.hd hosts in
       Host.get_data_sources ~rpc ~session_id ~host >>= fun dsl ->

       let rec loop start =
         let open Cohttp_lwt_unix in
         let uri = Xen_api_metrics.Updates.uri
             ~host:(Uri.of_string !uri) ~authentication:(`UserPassword(!username, !password))
             ~start:(Int64.to_int start) ~interval:(`Other !interval)
             ~include_host:true () in
         let b = Cohttp.Auth.string_of_credential (`Basic (!username, !password)) in
         let headers = Cohttp.Header.of_list ["authorization", b] in

         Client.call ~headers `GET uri >>= fun (res, body) ->
         let headers = Response.headers res in
         Cohttp.Header.iter
           (fun k v -> List.iter (Printf.eprintf "%s: %s\n%!" k) v) headers;
         Cohttp_lwt_body.to_string body
         >>= fun s ->
         let update = Xen_api_metrics.Updates.parse s in
         Printf.eprintf "%s\n%!" (Rrd_updates.string_of update);
         Array.iter (fun legend ->
             match Xen_api_metrics.Legend.of_string legend with
             | `Ok ((_name, _cf, `Host, _uuid) as legend') ->
               if Xen_api_metrics.Legend.find_data_source dsl legend' = None
               then Printf.fprintf stderr "Failed to find host data source: %s\n" legend
             | `Ok _ -> ()
             | `Error (`Msg x) ->
               Printf.fprintf stderr "Failed to parse legend: %s\n" x
           ) update.Rrd_updates.legend;
         Lwt_unix.sleep 5.
         >>= fun () ->
         loop update.Rrd_updates.end_time in
       loop (Int64.of_int !start))
    (fun () -> Session.logout ~rpc ~session_id)

let _ =
  Arg.parse [
    "-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
    "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
    "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
    "-start", Arg.Set_int start, (Printf.sprintf "Time since epoc to fetch updates from (default %d)" !start);
    "-interval", Arg.Set_int interval, (Printf.sprintf "Preferred sampling interval (default %d)" !interval);
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Simple example which watches metrics updates from a host";

  Lwt_main.run (main ())
