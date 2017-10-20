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
open Mswitch

module Config = struct
  type t = {
    path: string;
    pidfile: string option;
    configfile: string option;
    statedir: string option;
  } [@@deriving sexp]

  let default = {
    path = "/var/run/message-switch/sock";
    pidfile = None;
    configfile = None;
    statedir = None;
  }

  let make path pidfile configfile statedir =
    (* First load any config file *)
    let config = match configfile with
    | None -> default
    | Some filename -> Sexplib.Sexp.load_sexp filename |> t_of_sexp in
    let d a = function None -> a | Some b -> b in
    (* Second apply any command-line overrides *)
    let path = d config.path path in
    { path; pidfile; configfile; statedir }

  let term =
    let open Cmdliner in
    let path =
      let doc = "Path to create the listening Unix domain socket" in
      Arg.(value & opt (some string) None & info [ "path" ] ~doc) in
    let pidfile =
      let doc = "PID file to write" in
      Arg.(value & opt (some string) None & info [ "pidfile" ] ~doc) in
    let configfile =
      let doc = "Path to a config file" in
      Arg.(value & opt (some string) default.configfile & info [ "config" ] ~doc) in
    let statedir =
      let doc = "Directory containing state files" in
      Arg.(value & opt (some string) default.statedir & info [ "statedir" ] ~doc) in
    Term.(pure make $ path $ pidfile $ configfile $ statedir)
end

(* Let's try to adopt the conventions of Rresult.R *)
let get_ok = function | Result.Ok x -> x | Result.Error _ -> failwith "Expecting OK, got Error"

module Lwt_result = struct
  let (>>=) m f = m >>= fun x -> f (get_ok x)
end

open Cohttp_lwt_unix

let make_server config =
  let open Config in
  info "Started server on %s" config.path;

  let (_: 'a) = logging_thread () in

  let _dump_file = "queues.sexp" in
  let _redo_log = "redo-log" in
  let redo_log_size = 1024 * 1024 in

  let with_file path flags mode f =
    Lwt_unix.openfile path flags mode
    >>= fun fd ->
    Lwt.catch (fun () ->
      f fd
      >>= fun result ->
      Lwt_unix.close fd
      >>= fun () ->
      return result
    ) (fun e ->
      Lwt_unix.close fd
      >>= fun () ->
      fail e) in

  let save statedir qs =
    let txt = Sexplib.Sexp.to_string (Q.sexp_of_queues qs) in
    let final_path = Filename.concat statedir _dump_file in
    let temp_path = final_path ^ ".tmp" in
    with_file temp_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600
      (fun fd ->
        let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
        let n = String.length txt in
        Lwt_io.write_from_exactly oc txt 0 n
        >>= fun () ->
        Lwt_io.flush oc
      )
    >>= fun () ->
    Lwt_unix.rename temp_path final_path in

  let load statedir =
    let final_path = Filename.concat statedir _dump_file in
    Lwt_unix.stat final_path
    >>= fun stats ->
    let txt = String.make stats.Unix.st_size '\000' in
    with_file final_path [ Unix.O_RDONLY ] 0o600
      (fun fd ->
        let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
        Lwt_io.read_into_exactly ic txt 0 stats.Unix.st_size
      )
    >>= fun () ->
    return (Q.queues_of_sexp (Sexplib.Sexp.of_string txt)) in

  let module Redo_log = Shared_block.Journal.Make(Logging.Lwt_logger)(Block)(Time)(Clock)(Q.Op) in

  (* In-memory cache *)
  let queues = ref Q.empty in

  (* We'll wake up all threads blocked in transfer on every queue update.
     Not very scalable, but at least it is correct. *)
  let queues_c = Lwt_condition.create () in

  let on_disk_queues = ref Q.empty in
  let process_redo_log statedir ops =
    let on_disk_queues' = List.fold_left Q.do_op !on_disk_queues ops in
    save statedir on_disk_queues'
    >>= fun () ->
    on_disk_queues := on_disk_queues';
    return (Result.Ok ()) in

  let redo_log = match config.statedir with
    | None -> return None
    | Some statedir ->
      let redo_log_path = Filename.concat statedir _redo_log in
      let dump_path = Filename.concat statedir _dump_file in
      ( if not(Sys.file_exists redo_log_path) || (not (Sys.file_exists dump_path)) then begin
        info "Writing an empty set of queues to %s" dump_path;
        save statedir Q.empty
        >>= fun () ->
        info "Writing an empty redo-log to %s" redo_log_path;
        Lwt.catch (fun () -> Lwt_unix.unlink redo_log_path) (fun _ -> return ())
        >>= fun () ->
        Lwt_unix.openfile redo_log_path [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY; Lwt_unix.O_TRUNC ] 0o0666
        >>= fun fd ->
        Lwt_unix.lseek fd (redo_log_size - 1) Lwt_unix.SEEK_CUR >>= fun _ ->
        let byte = String.make 1 '\000' in
        Lwt_unix.write fd byte 0 1
        >>= fun n ->
        if n <> 1 then begin
          error "Failed to create redo-log at %s" redo_log_path;
          fail End_of_file
        end else begin
          Lwt_unix.close fd
        end
        end else return () ) >>= fun () ->
      info "Reading the queues from %s" dump_path;
      load statedir
      >>= fun qs ->
      on_disk_queues := qs;
      info "Reading the redo-log from %s" redo_log_path;
      Block.connect ~buffered:true redo_log_path
      >>= fun block ->
      let open Lwt_result in
      Redo_log.start ~flush_interval:5. block (process_redo_log statedir)
      >>= fun redo_log ->
      info "Redo-log playback complete: everything should be in sync";
      queues := !on_disk_queues; (* everything should be in sync *)
      return (Some redo_log) in

  let perform ops =
    let queues' = List.fold_left Q.do_op !queues ops in
    ( redo_log
      >>= function
      | None ->
        return ()
      | Some redo_log ->
        let rec loop = function
          | [] ->
            return ()
          | op :: ops ->
            ( Redo_log.push redo_log op
              >>= function
              | Result.Ok _waiter -> loop ops
              | Result.Error (`Msg txt) ->
                error "Failed to push to redo-log: %s" txt;
                fail (Failure "Failed to push to redo-log") )in
        loop ops
    )  >>= fun () ->
    queues := queues';
    Lwt_condition.broadcast queues_c ();
    return () in

  (* (Response.t * Body.t) Lwt.t *)
  let callback (_, conn_id) req body =
    (* Make sure we replay the log before processing requests *)
    redo_log
    >>= fun _ ->
    let conn_id_s = Cohttp.Connection.to_string conn_id in
    let open Message_switch_core.Protocol in
    Cohttp_lwt.Body.to_string body >>= fun body ->
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

      (* If it's the "blocking poll", block first and then call the implementation
         with the new queue state post-blocking *)
      ( match session, request with
        | Some session, In.Transfer { In.from = from; timeout = timeout; queues = names } ->
          let time = Int64.add (ns ()) (Int64.of_float (timeout *. 1e9)) in
          List.iter (record_transfer time) names;
          let from = match from with None -> -1L | Some x -> Int64.of_string x in
          let rec wait () =
            if Q.transfer !queues from names = [] then begin
              let timeout = max 0. (Int64.(to_float (sub time (ns ())))) /. 1e9 in
              Lwt.pick [ Lwt_unix.sleep timeout; Lwt_condition.wait queues_c ]
              >>= fun () ->
              if ns () > time
              then return ()
              else wait ()
            end else return () in
          wait ()
        | _, _ -> return () )
      >>= fun () ->

      Lwt_mutex.with_lock Switch_main_helper.m
        (fun () ->
          process_request conn_id_s !queues session request
          >>= fun (op_opt, response) ->
          let op = match op_opt with None -> [] | Some x -> [x] in
          perform op
          >>= fun () ->
          return response
        ) >>= fun response ->

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
        let (_: unit Lwt.t) =
          info "Session %s cleaning up" session;
          let qs = Q.owned_queues !queues session in
          let ops = Q.StringSet.fold (fun x ops -> Q.Directory.remove !queues x :: ops) qs [] in
          Lwt_mutex.with_lock Switch_main_helper.m
            (fun () ->
              perform ops
            ) in
        ()
      end in

  info "Message switch starting";
  let t = Cohttp_lwt_unix.Server.make ~conn_closed ~callback () in
  Lwt.catch
    (fun () -> Lwt_unix.unlink config.path)
    (function
     | Unix.Unix_error(Unix.ENOENT, _, _) -> return ()
     | e -> fail e)
  >>= fun () ->
  (* see ocaml-cohttp/issues/511 for additional context *)
  let on_exn = function
    | Unix.Unix_error (Unix.EPIPE, _, _) ->
        (* This is the common client disconnection error - no need to log it*)
        ()
    | Unix.Unix_error (error, func, arg) ->
      let msg = Printf.sprintf "Client connection error %s: %s(%S)" (Unix.error_message error) func arg in
      Lwt_log.ign_warning msg
    | exn -> Lwt_log.ign_error ~exn "Unhandled exception"
  in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode:(`Unix_domain_socket (`File config.path)) t

exception Not_a_directory of string
exception Does_not_exist of string

let main ({ Config.path; pidfile } as config) =
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

    info "Logs redirected to syslog";
    Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();

    let (_ : unit Lwt.t) =
      match pidfile with
      | None -> return ()
      | Some x ->
        Lwt_io.with_file ~flags:[Unix.O_WRONLY; Unix.O_CREAT] ~perm:0o0644
          ~mode:Lwt_io.output x (fun oc ->
              Lwt_io.write oc (Printf.sprintf "%d" (Unix.getpid ())) >>= fun () ->
              Lwt_io.flush oc
            ) in

    Lwt_main.run (make_server config);
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
