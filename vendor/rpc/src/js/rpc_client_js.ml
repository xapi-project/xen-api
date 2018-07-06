(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Jon Ludlam <jonathan.ludlam@eu.citrix.com>
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Js

let do_rpc enc dec content_type ~url call =
  let method_ = "POST" in
  let contents = enc call in
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in

  req##_open (Js.string method_) (Js.string url) Js._true;
  req##setRequestHeader (Js.string "Content-type") (Js.string content_type);
  req##.onreadystatechange := Js.wrap_callback
      (fun _ ->
         (match req##.readyState with
          | XmlHttpRequest.DONE ->
            Lwt.wakeup w (dec (Js.to_string req##.responseText))
          | _ -> ()));

  req##send (Js.some (Js.string contents));

  Lwt.on_cancel res (fun () -> req##abort);
  res

let do_xml_rpc = do_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string "text/xml"
let do_json_rpc = do_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string "text/json"
let do_json_rpc_opt = do_rpc Rpc_client_js_helper.string_of_call Rpc_client_js_helper.response_of_string "text/json"
