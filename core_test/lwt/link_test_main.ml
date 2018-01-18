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

open Cohttp_lwt_unix
open Lwt
open Message_switch_core.Protocol
open Message_switch_lwt.Protocol_lwt

let basedir = ref "/tmp/link_test"

let rpc_req = { Message.payload = "hello"; kind = Message.Request "reply to" }
let rpc_res = { Message.payload = "hello"; kind = Message.Response ("q", 1L) }

let in_frames =
  let open In in [
    "login", Login "hello";
    "persistent", CreatePersistent "service";
    "transient", CreateTransient "client";
    "request", Send("service", rpc_req);
    "reply", Send("service", rpc_res);
    "transfer", Transfer { from = Some "3"; timeout = 5.; queues = ["one"; "two"]};
    "ack", Ack ("q", 3L);
  ]

let out_frames =
  let open Out in [
    "create.reply", Create "service";
    "transfer.reply", Transfer { messages = [
        ("q", 1L), rpc_req;
        ("q2", 2L), rpc_res;
      ]; next = "0" }
  ]

let make_file name f =
  Lwt_unix.openfile (Filename.concat !basedir name) [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 >>= fun fd ->
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.catch
    (fun () ->
       f oc >>= fun () ->
       Lwt_io.flush oc >>= fun () ->
       Lwt_unix.close fd)
    (fun _ ->
       Lwt_unix.close fd)

let main () =
  Lwt_list.iter_s
    (fun (name, in_frame) ->
       make_file name
         (fun oc ->
            let body, meth, uri = In.to_request in_frame in
            let body = match body with None -> "" | Some x -> x in
            let lines = [
              Printf.sprintf "%s %s HTTP/1.1" (Cohttp.Code.string_of_method meth) (Uri.to_string uri);
              Printf.sprintf "Content-Length: %d" (String.length body);
              "";
              body
            ] in
            Lwt_io.write oc (String.concat "\r\n" lines)
         )
    ) in_frames >>= fun () ->
  Lwt_list.iter_s
    (fun (name, out_frame) ->
       make_file name
         (fun oc ->
            let code, body = Out.to_response out_frame in
            let lines = [
              Printf.sprintf "HTTP/1.1 %s" (Cohttp.Code.string_of_status code);
              Printf.sprintf "Content-Length: %d" (String.length body);
              "";
              body
            ] in
            Lwt_io.write oc (String.concat "\r\n" lines)
         )
    ) out_frames

let _ =
  Arg.parse [
    "-dir", Arg.Set_string basedir, "Directory to place protocol fragments";
  ] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
    "Test the parser/printer for the link-layer protocol";

  Lwt_main.run (main ())
