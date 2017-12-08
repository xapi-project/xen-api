(*
 * Copyright (c) 2012 Citrix Inc
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
 *
 *)

type 'a t = 'a Lwt.t
let iter fn x = Lwt_list.iter_s fn x
let return = Lwt.return
let (>>=) = Lwt.bind
let (>>) m n = m >>= fun _ -> n

(** Use as few really_{read,write} calls as we can (for efficiency) without
    explicitly buffering the stream beyond the HTTP headers. This will
    allow us to consume the headers and then pass the file descriptor
    safely to another process *)

type ic = {
  mutable header_buffer: string option; (** buffered headers *)
  mutable header_buffer_idx: int;       (** next char within the buffered headers *)
  c: Data_channel.t;
}

let make_input c =
  let header_buffer = None in
  let header_buffer_idx = 0 in
  { header_buffer; header_buffer_idx; c }

type oc = Data_channel.t
type conn = Data_channel.t

let really_read_into c buf ofs len =
  let tmp = Cstruct.create len in
  c.Data_channel.really_read tmp >>= fun () ->
  Cstruct.blit_to_string tmp 0 buf ofs len;
  return ()

let read_http_headers c =
  let buf = Buffer.create 128 in
  (* We can safely read everything up to this marker: *)
  let end_of_headers = "\r\n\r\n" in
  let tmp = String.make (String.length end_of_headers) '\000' in
  let module Scanner = struct
    type t = {
      marker: string;
      mutable i: int;
    }
    let make x = { marker = x; i = 0 }
    let input x c =
      if c = x.marker.[x.i] then x.i <- x.i + 1 else x.i <- 0
    let remaining x = String.length x.marker - x.i
    let matched x = x.i = String.length x.marker
    (* let to_string x = Printf.sprintf "%d" x.i *)
  end in
  let marker = Scanner.make end_of_headers in

  let rec loop () =
    if not(Scanner.matched marker) then begin
      (* We may be part way through reading the end of header marker, so
         be pessimistic and only read enough bytes to read until the end of
         the marker. *)
      let safe_to_read = Scanner.remaining marker in

      really_read_into c tmp 0 safe_to_read >>= fun () ->

      for j = 0 to safe_to_read - 1 do
        Scanner.input marker tmp.[j];
        Buffer.add_char buf tmp.[j]
      done;
      loop ()
    end else return () in
  loop () >>= fun () ->
  return (Buffer.contents buf)

let crlf = Re_str.regexp_string "\r\n"

(* We assume read_line is only used to read the HTTP header *)
let rec read_line ic = match ic.header_buffer, ic.header_buffer_idx with
  | None, _ ->
    read_http_headers ic.c >>= fun str ->
    ic.header_buffer <- Some str;
    read_line ic
  | Some buf, i when i < (String.length buf) ->
    begin
      try
        let eol = Re_str.search_forward crlf buf i in
        let line = String.sub buf i (eol - i) in
        ic.header_buffer_idx <- eol + 2;
        return (Some line)
      with Not_found -> return (Some "")
    end
  | Some _, _ ->
    return (Some "")

let read_into_exactly ic buf ofs len =
  really_read_into ic.c buf ofs len >>= fun () ->
  return true

let read_exactly ic len =
  let buf = Bytes.create len in
  read_into_exactly ic buf 0 len >>= function
  | true -> return (Some buf)
  | false -> return None

let read ic n =
  let buf = String.make n '\000' in
  really_read_into ic.c buf 0 n >>= fun () ->
  return buf

let write oc x =
  let buf = Cstruct.create (String.length x) in
  Cstruct.blit_from_string x 0 buf 0 (String.length x);
  oc.Data_channel.really_write buf

let flush _oc =
  return ()
