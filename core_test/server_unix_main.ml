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

let path = ref "/var/run/message-switch/sock"
let name = ref "server"

let process = function
  | "shutdown" ->
    let (_: Thread.t) = Thread.create (fun () ->
      Thread.delay 1.;
      exit 0
    ) () in
    "ok"
  | x -> x

let main () =
  let _ = Message_switch_unix.Protocol_unix.Server.listen ~process ~switch:!path ~queue:!name () in
  let rec forever () =
    Thread.delay 3600.;
    forever () in
  forever ()

let _ =
  Arg.parse [
    "-path", Arg.Set_string path, (Printf.sprintf "path switch listens on (default %s)" !path);
    "-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
  ] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
    "Respond to RPCs on a name";

  main ()
