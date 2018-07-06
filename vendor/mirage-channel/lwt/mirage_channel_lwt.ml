(*
 * Copyright (c) 2011-2016 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2015 Mindy Preston
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Buffered reading and writing over the Flow API *)

open Lwt.Infix
open Result

module type S = Mirage_channel.S
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t

let src = Logs.Src.create "channel"
    ~doc:"Buffered reading and writing over the Flow API"

module Log = (val Logs.src_log src : Logs.LOG)

module Make(Flow: Mirage_flow_lwt.S) = struct

  type flow = Flow.flow
  type buffer = Cstruct.t
  type +'a io = 'a Lwt.t

  type error = [`Read_zero | `Flow of Flow.error]
  type write_error = Flow.write_error

  let pp_error ppf = function
  | `Flow e    -> Flow.pp_error ppf e
  | `Read_zero ->
      Fmt.string ppf
        "FLOW.read returned 0 bytes in violation of the specification"

  let pp_write_error = Flow.pp_write_error

  type t = {
    flow: flow;
    mutable ibuf: Cstruct.t option; (* Queue of incoming buf *)
    mutable obufq: Cstruct.t list;  (* Queue of completed writebuf *)
    mutable obuf: Cstruct.t option; (* Active write buffer *)
    mutable opos: int;                 (* Position in active write buffer *)
  }

  let create flow =
    let ibuf = None in
    let obufq = [] in
    let obuf = None in
    let opos = 0 in
    { ibuf; obuf; flow; obufq; opos }

  let to_flow { flow; _ } = flow

  let ibuf_refill t =
    Flow.read t.flow >|= function
    | Ok (`Data buf) when Cstruct.len buf = 0 ->
        Log.err (fun l -> l "%a" pp_error `Read_zero);
        Error `Read_zero
    | Ok (`Data buf) ->
        t.ibuf <- Some buf;
        Ok (`Data buf)
    | Ok `Eof -> Ok `Eof
    | Error e -> Error (`Flow e)

  let bind v fn =
   v >>= function
   | Ok (`Data buf) -> fn buf
   | Ok `Eof -> Lwt.return (Ok `Eof)
   | Error e -> Lwt.return (Error e)

  let (>>=~) = bind

  let rec get_ibuf t =
    match t.ibuf with
    | None -> ibuf_refill t >>=~ fun _ -> get_ibuf t
    | Some buf when Cstruct.len buf = 0 -> ibuf_refill t >>=~ fun _ -> get_ibuf t
    | Some buf -> Lwt.return (Ok (`Data buf))

  (* Read one character from the input channel *)
  let read_char t =
    get_ibuf t (* the fact that we returned means we have at least 1 char *)
    >>=~ fun buf ->
    let c = Cstruct.get_char buf 0 in
    t.ibuf <- Some (Cstruct.shift buf 1); (* advance read buffer, possibly to
                                             EOF *)
    Lwt.return (Ok (`Data c))

  (* Read up to len characters from the input channel
     and at most a full view. If not specified, read all *)
  let read_some ?len t =
    (* get_ibuf potentially throws EOF-related exceptions *)
    get_ibuf t >>=~ fun buf ->
    let avail = Cstruct.len buf in
    let len = match len with |Some len -> len |None -> avail in
    if len < avail then begin
      let hd,tl = Cstruct.split buf len in
      t.ibuf <- Some tl; (* leave some in the buffer; next time, we won't do a
                            blocking read *)
      Lwt.return (Ok (`Data hd))
    end else begin
      t.ibuf <- None;
      Lwt.return (Ok (`Data buf))
    end

  let read_exactly ~len t =
    let rec loop acc = function
      | 0 ->
        Lwt.return (Ok (`Data (List.rev acc)))
      | len ->
        read_some ~len t
        >>=~ fun buffer ->
        loop (buffer :: acc) (len - (Cstruct.len buffer)) in
    loop [] len

  (* Read until a character is found *)
  let read_until t ch =
    get_ibuf t >>=~ fun buf ->
    let len = Cstruct.len buf in
    let rec scan off =
      if off = len then None
      else if Cstruct.get_char buf off = ch then Some off else scan (off+1)
    in
    match scan 0 with
    | None -> (* not found, return what we have until EOF *)
       t.ibuf <- None; (* basically guaranteeing that next read is EOF *)
       Lwt.return (Ok (`Not_found buf))
    | Some off -> (* found, so split the buffer *)
       let hd = Cstruct.sub buf 0 off in
       t.ibuf <- Some (Cstruct.shift buf (off+1));
       Lwt.return (Ok (`Found hd))

  (* This reads a line of input, which is terminated either by a CRLF
     sequence, or the end of the channel (which counts as a line).
     @return Returns a stream of views that terminates at EOF. *)
  let read_line t =
    let rec get acc =
      read_until t '\n' >>= function
      | Error e -> Lwt.return (Error e)
      | Ok `Eof -> Lwt.return (Ok (`Data acc))
      | Ok (`Not_found buf) when Cstruct.len buf = 0 -> Lwt.return (Ok (`Data acc))
      | Ok (`Not_found buf) -> get (buf::acc)
      | Ok (`Found buf) ->
          (* chop the CR if present *)
          let buflen = Cstruct.len buf in
          let buf =
            if buflen > 0 && (Cstruct.get_char buf (buflen-1) = '\r') then
              Cstruct.sub buf 0 (buflen-1) else buf
          in
          Lwt.return (Ok (`Data (buf :: acc)))
    in
    get [] >>=~ fun bits -> Lwt.return (Ok (`Data (List.rev bits)))

  (* Output functions *)

  let alloc_obuf t =
    let buf = Io_page.to_cstruct (Io_page.get 1) in
    t.obuf <- Some buf;
    t.opos <- 0;
    buf

  (* Queue the active write buffer onto the write queue, resizing the
   * view if necessary to the correct size. *)
  let queue_obuf t =
    match t.obuf with
    |None -> ()
    |Some buf when Cstruct.len buf = t.opos -> (* obuf is full *)
      t.obufq <- buf :: t.obufq;
      t.obuf <- None
    |Some _ when t.opos = 0 -> (* obuf wasnt ever used, so discard *)
      t.obuf <- None
    |Some buf -> (* partially filled obuf, so resize *)
      let buf = Cstruct.sub buf 0 t.opos in
      t.obufq <- buf :: t.obufq;
      t.obuf <- None

  (* Get an active output buffer, which will allocate it if needed.
   * The position to write into is stored in t.opos *)
  let get_obuf t =
    match t.obuf with
    |None -> alloc_obuf t
    |Some buf when Cstruct.len buf = t.opos -> queue_obuf t; alloc_obuf t
    |Some buf -> buf

  (* Non-blocking character write, since Io page allocation never blocks.
   * That may change in the future... *)
  let write_char t ch =
    let buf = get_obuf t in
    Cstruct.set_char buf t.opos ch;
    t.opos <- t.opos + 1

  (* This is zero copy; flush current IO page and queue up the incoming
   * buffer directly. *)
  let write_buffer t buf =
    queue_obuf t;
    t.obufq <- buf :: t.obufq

  let rec write_string t s off len =
    let buf = get_obuf t in
    let avail = Cstruct.len buf - t.opos in
    if avail < len then begin
      Cstruct.blit_from_string s off buf t.opos avail;
      t.opos <- t.opos + avail;
      write_string t s (off+avail) (len-avail)
    end else begin
      Cstruct.blit_from_string s off buf t.opos len;
      t.opos <- t.opos + len
    end

  let write_line t buf =
    write_string t buf 0 (String.length buf);
    write_char t '\n'

  let flush t =
    queue_obuf t;
    let l = List.rev t.obufq in
    t.obufq <- [];
    Flow.writev t.flow l

  let close t =
    Lwt.finalize (fun () -> flush t) (fun () -> Flow.close t.flow)

end
