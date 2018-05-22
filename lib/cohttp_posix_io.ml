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

module Unbuffered_IO = struct
  (** Use as few Unix.{read,write} calls as we can (for efficiency) without
      explicitly buffering the stream beyond the HTTP headers. This will
      allow us to consume the headers and then pass the file descriptor
      safely to another process *)

  type 'a t = 'a

  let (>>=) x f = f x
  let (>>) m n = m >>= fun _ -> n

  let return x = x

  let iter = List.iter

  type ic = {
    mutable header_buffer: string option; (** buffered headers *)
    mutable header_buffer_idx: int;       (** next char within the buffered headers *)
    fd: Unix.file_descr;                  (** the underlying file descriptor *)
  }

  type oc = Unix.file_descr

  type conn = unit

  let read_http_headers fd =
    let buf = Buffer.create 128 in
    (* We can safely read everything up to this marker: *)
    let end_of_headers = (Bytes.of_string "\r\n\r\n") in
    let tmp = Bytes.make (Bytes.length end_of_headers) '\000' in
    let module Scanner = struct
      type t = {
        marker: bytes;
        mutable i: int;
      }
      let make x = { marker = x; i = 0 }
      let input x c =
        if c = Bytes.get x.marker x.i then x.i <- x.i + 1 else x.i <- 0
      let remaining x = Bytes.length x.marker - x.i
      let matched x = x.i = Bytes.length x.marker
    end in
    let marker = Scanner.make end_of_headers in

    while not(Scanner.matched marker) do
      (* We may be part way through reading the end of header marker, so
         be pessimistic and only read enough bytes to read until the end of
         the marker. *)
      let safe_to_read = Scanner.remaining marker in

      let n = Unix.read fd tmp 0 safe_to_read in
      if n = 0 then raise End_of_file;

      for j = 0 to n - 1 do
        Scanner.input marker (Bytes.get tmp j);
        Buffer.add_char buf (Bytes.get tmp j)
      done;
    done;
    Buffer.contents buf


  (* Raises Not_found if there's no crlf *)
  let rec find_crlf str from = 
    let cr = String.index_from str from '\r' in
    let lf = String.index_from str cr '\n' in
    if lf=cr+1 then cr else find_crlf str cr

  (* We assume read_line is only used to read the HTTP header *)
  let rec read_line ic = match ic.header_buffer, ic.header_buffer_idx with
  | None, _ ->
    ic.header_buffer <- Some (read_http_headers ic.fd);
    read_line ic
  | Some buf, i when i < (String.length buf) ->
    begin
      try
        let eol = find_crlf buf i in
        let line = String.sub buf i (eol - i) in
        ic.header_buffer_idx <- eol + 2;
        Some line
      with Not_found -> Some ""
    end
  | Some _, _ ->
    Some ""

  let rec read_into_exactly ic buf ofs len =
    let n = Unix.read ic.fd buf ofs len in
    let remaining = len - n in
    remaining = 0 || (n > 0 && (read_into_exactly ic buf (ofs + n) (len - n)))

  let read_exactly ic len =
    let buf = Bytes.create len in
    read_into_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None

  let read ic n =
    let buf = Bytes.make n '\000' in
    let actually_read = Unix.read ic.fd buf 0 n in
    if actually_read = n
    then Bytes.unsafe_to_string buf
    else Bytes.sub_string buf 0 actually_read

  let write oc x =
    Unix.write oc (Bytes.unsafe_of_string x) 0 (String.length x)
    |> ignore

  let flush _oc = ()
end

module Buffered_IO = struct
  type 'a t = 'a

  let (>>=) x f = f x
  let (>>) m n = m >>= fun _ -> n

  let return x = x

  let iter = List.iter

  type ic = in_channel
  type oc = out_channel
  type conn = unit

  let read_line ic =
    try
      Some (match input_line ic with
      | "" -> ""
      | x when x.[String.length x - 1] = '\r' -> String.sub x 0 (String.length x - 1)
      | x -> x)
    with End_of_file -> None

  let read_into_exactly ic buf ofs len = try really_input ic buf ofs len; true with _ -> false

  let read_exactly ic len =
    let buf = Bytes.create len in
    read_into_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None

  let read ic n =
    let buf = Bytes.make n '\000' in
    let actually_read = input ic buf 0 n in
    if actually_read = n
    then Bytes.unsafe_to_string buf
    else Bytes.sub_string buf 0 actually_read

  let write oc x = output_string oc x; flush oc

  let flush oc = flush oc
end
