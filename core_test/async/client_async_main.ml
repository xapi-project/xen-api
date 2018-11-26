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

module P = Printf

open Core
open Async

open Message_switch_core.Protocol
open Message_switch_async.Protocol_async

let path = ref "/var/run/message-switch/sock"
let name = ref "server"
let payload = ref "hello"
let timeout = ref None
let shutdown = "shutdown"

let (>>|=) m f = m >>= function
  | `Ok x -> f x
  | `Error y ->
    let b = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer b in
    Client.pp_error fmt y;
    Format.pp_print_flush fmt ();
    raise (Failure (Buffer.contents b))

let main () =
  Client.connect ~switch:!path () >>|= fun t ->
  let counter = ref 0 in
  let one () =
    incr counter;
    Client.rpc ~t ~queue:!name ~body:!payload () >>|= fun _ ->
    return () in
  let start = Time.now () in
  ( match !timeout with
    | None -> one ()
    | Some t ->
      let rec loop () =
        let sofar = Time.diff (Time.now()) start in
        if Time.Span.(sofar > (of_sec t))
        then return ()
        else begin
          one () >>= fun () ->
          loop ()
        end in
      loop ()
  ) >>= fun () ->
  let time = Time.diff (Time.now()) start in
  P.printf "Finished %d RPCs in %.02f\n%!" !counter (Time.Span.to_sec time);
  Client.rpc ~t ~queue:!name ~body:shutdown () >>|= fun _ ->
  Shutdown.exit 0

let _ =
  Arg.parse [
    "-path", Arg.Set_string path, (Printf.sprintf "path broker listens on (default %s)" !path);
    "-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
    "-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);
    "-secs", Arg.String (fun x -> timeout := Some (Float.of_string x)), (Printf.sprintf "number of seconds to repeat the same message for (default %s)" (match !timeout with None -> "None" | Some x -> Float.to_string x));
  ] (fun x -> P.fprintf stderr "Ignoring unexpected argument: %s" x)
    "Send a message to a name, optionally waiting for a response";
  let (_: 'a Deferred.t) = main () in
  never_returns (Scheduler.go ())
