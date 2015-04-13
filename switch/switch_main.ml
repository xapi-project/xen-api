(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std
open Lwt
open Cohttp
open Logging
open Clock
open Switch

module Config = struct
  type t = {
    port: int;
    ip: string;
    daemonize: bool;
    pidfile: string option;
    configfile: string option;
    statedir: string option;
  } with sexp

  let default = {
    port = 8080;
    ip = "0.0.0.0";
    daemonize = false;
    pidfile = None;
    configfile = None;
    statedir = None;
  }

  let make daemonize port ip pidfile configfile statedir =
    (* First load any config file *)
    let config = match configfile with
    | None -> default
    | Some filename -> Sexplib.Sexp.load_sexp filename |> t_of_sexp in
    let d a = function None -> a | Some b -> b in
    (* Second apply any command-line overrides *)
    let port = d config.port port in
    let ip = d config.ip ip in
    let daemonize = config.daemonize || daemonize in
    { daemonize; port; ip; pidfile; configfile; statedir }

  let term =
    let open Cmdliner in
    let daemon =
      let doc = "Detach from the terminal and run as a daemon" in
      Arg.(value & flag & info [ "daemon" ] ~doc) in
    let port =
      let doc = "Port to listen on" in
      Arg.(value & opt (some int) None & info [ "port" ] ~doc) in
    let ip =
      let doc = "IP address to bind to" in
      Arg.(value & opt (some string) None & info [ "ip" ] ~doc) in
    let pidfile =
      let doc = "PID file to write" in
      Arg.(value & opt (some string) None & info [ "pidfile" ] ~doc) in
    let configfile =
      let doc = "Path to a config file" in
      Arg.(value & opt (some string) default.configfile & info [ "config" ] ~doc) in
    let statedir =
      let doc = "Directory containing state files" in
      Arg.(value & opt (some string) default.statedir & info [ "statedir" ] ~doc) in
    Term.(pure make $ daemon $ port $ ip $ pidfile $ configfile $ statedir)
end

open Cohttp_lwt_unix

let make_server config =
  let open Config in
  info "Started server on localhost:%d" config.port;

  let (_: 'a) = logging_thread () in

  (* (Response.t * Body.t) Lwt.t *)
  let callback (_, conn_id) req body =
    let conn_id_s = Cohttp.Connection.to_string conn_id in
    let open Protocol in
    lwt body = Cohttp_lwt_body.to_string body in
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    match In.of_request body (Cohttp.Request.meth req) path with
    | None ->
      error "<- [unparsable request; path = %s; body = %s]" path ("\"" ^ body ^ "\"");
      error "-> 404 [Not_found]";
      Cohttp_lwt_unix.Server.respond_not_found ~uri ()
    | Some request ->
      debug "<- %s [%s]" path body;
      let session = Connections.get_session conn_id_s in
      lwt response = process_request conn_id_s session request in
      let status, body = Out.to_response response in
      debug "-> %s [%s]" (Cohttp.Code.string_of_status status) body;
      Cohttp_lwt_unix.Server.respond_string ~status ~body ()
  in
  let conn_closed (_, conn_id) =
    let conn_id_s = Cohttp.Connection.to_string conn_id in
    let session = Connections.get_session conn_id_s in
    Connections.remove conn_id_s;
    match session with
    | None -> ()
    | Some session ->
      if not(Connections.is_session_active session) then begin
        info "Session %s cleaning up" session;
        Transient_queue.remove session
      end in

  info "Message switch starting";
  let t = Cohttp_lwt_unix.Server.make ~conn_closed ~callback () in
  Cohttp_lwt_unix.Server.create ~mode:(`TCP(`Port config.port)) t

exception Not_a_directory of string
exception Does_not_exist of string

let main ({ Config.daemonize; port; ip; pidfile } as config) =
  info "Starting with configuration:";
  info "%s" (Sexplib.Sexp.to_string (Config.sexp_of_t config));
  try
    ( match config.Config.statedir with
      | Some dir ->
        if Sys.file_exists dir then begin
          if not(Sys.is_directory dir)
          then raise (Not_a_directory dir)
        end else raise (Does_not_exist dir)
      | _ ->
        () );
    if daemonize
    then Lwt_daemon.daemonize ();

    let (_ : unit Lwt.t) =
      match pidfile with
      | None -> return ()
      | Some x ->
        Lwt_io.with_file ~flags:[Unix.O_WRONLY; Unix.O_CREAT] ~perm:0o0644
          ~mode:Lwt_io.output x (fun oc ->
              lwt () = Lwt_io.write oc (Printf.sprintf "%d" (Unix.getpid ())) in
              Lwt_io.flush oc
            ) in

    Lwt_unix.run (make_server config);
    `Ok ()
  with
  | Not_a_directory dir ->
    `Error(false, Printf.sprintf "%s is not a directory" dir)
  | Does_not_exist dir ->
    `Error(false, Printf.sprintf "%s does not exist" dir)

open Cmdliner

let cmd =
  let doc = "Simple single-host persistent message switch" in
  let man = [
    `S "DESCRIPTION";
    `P "The switch stores a set of named queues, each containing an ordered sequence of messages. Clients may enqueue and acknowledge messages using simple HTTP requests.";
  ] in

  Term.(ret(pure main $ Config.term)),
  Term.info "main" ~doc ~man

let _ =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
