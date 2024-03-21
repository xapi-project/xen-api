(*
 * Copyright (c) 2012 Citrix Systems
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

let ( let* ) = Lwt.bind

type 'a t = {stream: 'a Lwt_stream.t; capacity: int; size: int ref}

let create capacity =
  let stream, stream_push = Lwt_stream.create () in
  let t = {stream; capacity; size= ref 0} in
  let push = function
    | Some _ when !(t.size) > t.capacity ->
        None
    | None ->
        stream_push None ; Some ()
    | elem ->
        stream_push elem ; incr t.size ; Some ()
  in
  (t, push)

let size {size; _} = !size

let get_available t =
  let all = Lwt_stream.get_available t.stream in
  t.size := !(t.size) - List.length all ;
  all

let get t =
  let* elem = Lwt_stream.get t.stream in
  decr t.size ; Lwt.return elem

let nget n t =
  let* all = Lwt_stream.nget n t.stream in
  t.size := !(t.size) - List.length all ;
  Lwt.return all
