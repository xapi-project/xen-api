(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Xapi_stdext_std

(* TODO:
   1. Check for overflow in UInt32/UInt64
*)

exception Truncated

let _marshal (x: int list) = Xstringext.String.implode (List.map char_of_int (List.map (fun x -> x land 0xff) x))
let _unmarshal (x: string) = List.map int_of_char (Xstringext.String.explode x)

let blit src srcoff dst dstoff len =
  (* Printf.printf "blit src_len=%d srcoff=%d dst_len=%d dstoff=%d len=%d\n" (String.length src) srcoff (String.length dst) dstoff len;  *)
  String.blit src srcoff dst dstoff len

module UInt16 = struct
  type t = int

  let (||) = (lor)
  let (<<) = (lsl)
  let (>>) = (lsr)
  let (&&) = (land)

  let marshal (x: t) : string =
    _marshal [ x >> 8; x ]
  let marshal_at (buf: string) (off: int) (x: t) =
    let raw = marshal x in
    blit raw 0 buf off 2;
    off + 2
  let unmarshal (x: string) : t = match _unmarshal x with
    | [ msb; lsb ] -> (msb << 8) || lsb
    | _ -> raise Truncated

  let prettyprint = string_of_int
  let to_int x = x
  let of_int x = x
end

module UInt32 = struct
  type t = int32

  let (||) = Int32.logor
  let (<<) = Int32.shift_left
  let (>>) = Int32.shift_right
  let (&&) = Int32.logand

  let marshal (x: t) : string =
    _marshal (List.map Int32.to_int [ x >> 24; x >> 16; x >> 8; x ])
  let marshal_at (buf: string) (off: int) (x: t) =
    let raw = marshal x in
    blit raw 0 buf off 4;
    off + 4
  let unmarshal (x: string) : t = match List.map Int32.of_int (_unmarshal x) with
    | [ a; b; c; d ] -> (a << 24) || (b << 16) || (c << 8) || d
    | _ -> raise Truncated

  let prettyprint = string_of_int
  let to_int32 x = x
  let of_int32 x = x
end

module UInt64 = struct
  type t = int64

  let (||) = Int64.logor
  let (<<) = Int64.shift_left
  let (>>) = Int64.shift_right
  let (&&) = Int64.logand

  let marshal (x: t) : string =
    _marshal (List.map Int64.to_int [ x >> 56; x >> 48; x >> 40; x >> 32; x >> 24; x >> 16; x >> 8; x ])
  let unmarshal (x: string) : t = match List.map Int64.of_int (_unmarshal x) with
    | [ a; b; c; d; e; f; g; h ] -> (a << 56 ) || (b << 48) || (c << 40) || (d << 32) || (e << 24) || (f << 16) || (g << 8) || h
    | _ -> raise Truncated

  let prettyprint = Int64.to_string
  let to_int64 x = x
  let of_int64 x = x
end

(** Really read, raising End_of_file if no more data *)
let really_read fd n =
  let buf = String.make n '\000' in
  let rec rread fd buf ofs len =
    let n = Unix.read fd buf ofs len in
    if n = 0 then raise End_of_file;
    if n < len then rread fd buf (ofs + n) (len - n) in
  rread fd buf 0 n;
  buf
let really_write fd buf =
(*
  Printf.printf "About to write %d bytes [ %s ]\n"
    (String.length buf) (String.concat " " (List.map (fun x -> Printf.sprintf "%02x" (int_of_char x)) (String.explode buf)));
(*Unix.sleep 2; *)
*)
  let len = Unix.write fd buf 0 (String.length buf) in
  if len <> String.length buf then raise End_of_file

module ProtocolVersion = struct
  type t = { major: int; minor: int }

  exception Unmarshal_failure

  let marshal (x: t) = Printf.sprintf "RFB %03x.%03x\n" x.major x.minor
  let unmarshal (s: Unix.file_descr) =
    let x = really_read s 12 in
    if not(Xstringext.String.startswith "RFB " x)
    then raise Unmarshal_failure;
    let major = int_of_string (String.sub x 4 3)
    and minor = int_of_string (String.sub x 8 3) in
    { major = major; minor = minor }

  let prettyprint (x: t) =
    Printf.sprintf "ProtocolVersion major = %d minor = %d" x.major x.minor
end

module Error = struct
  type t = string

  let marshal (x: t) = UInt32.marshal (Int32.of_int (String.length x)) ^ x
  let unmarshal (s: Unix.file_descr) =
    let len = UInt32.unmarshal (really_read s 4) in
    really_read s (Int32.to_int len)
end

(* 3.3 *)
module SecurityType = struct
  type t = Failed of string | NoSecurity | VNCAuth

  exception Unmarshal_failure

  let marshal (x: t) = match x with
    | Failed x -> UInt32.marshal 0l ^ (Error.marshal x)
    | NoSecurity -> UInt32.marshal 1l
    | VNCAuth -> UInt32.marshal 2l

  let unmarshal (s: Unix.file_descr) =
    match UInt32.unmarshal (really_read s 4) with
    | 0l -> Failed (Error.unmarshal s)
    | 1l -> NoSecurity
    | 2l -> VNCAuth
    | _ -> raise Unmarshal_failure
end

module ClientInit = struct
  type t = bool (* shared-flag *)

  let marshal (x: t) = if x then "x" else "\000"
  let unmarshal (s: Unix.file_descr) =
    match (really_read s 1).[0] with
    | '\000' -> false
    | _ -> true
end

module PixelFormat = struct
  type t = { bpp: int;
             depth: int;
             big_endian: bool;
             true_colour: bool;
             red_max: int;
             green_max: int;
             blue_max: int;
             red_shift: int;
             green_shift: int;
             blue_shift: int }
  let true_colour_default big_endian = {
    bpp = 32; depth = 24; big_endian = big_endian;
    true_colour = true;
    (* rest are unused *)
    red_max = 0; green_max = 0; blue_max = 0;
    red_shift = 0; green_shift = 0; blue_shift = 0;
  }
  let marshal (x: t) =
    let bpp = String.make 1 (char_of_int x.bpp) in
    let depth = String.make 1 (char_of_int x.depth) in
    let big_endian = if x.big_endian then "x" else "\000" in
    let true_colour = if x.true_colour then "x" else "\000" in
    let red_max = UInt16.marshal x.red_max in
    let green_max = UInt16.marshal x.green_max in
    let blue_max = UInt16.marshal x.blue_max in
    let red_shift = String.make 1 (char_of_int x.red_shift) in
    let green_shift = String.make 1 (char_of_int x.green_shift) in
    let blue_shift = String.make 1 (char_of_int x.blue_shift) in
    bpp ^ depth ^ big_endian ^ true_colour ^
    red_max ^ green_max ^ blue_max ^ red_shift ^ green_shift ^ blue_shift ^
    "   " (* padding *)
  let unmarshal (s: Unix.file_descr) =
    let buf = really_read s 16 in
    { bpp = int_of_char buf.[0];
      depth = int_of_char buf.[1];
      big_endian = buf.[2] <> '\000';
      true_colour = buf.[3] <> '\000';
      red_max = UInt16.unmarshal (String.sub buf 4 2);
      green_max = UInt16.unmarshal (String.sub buf 6 2);
      blue_max = UInt16.unmarshal (String.sub buf 8 2);
      red_shift = int_of_char buf.[10];
      green_shift = int_of_char buf.[11];
      blue_shift = int_of_char buf.[12];
      (* ignoring padding *)
    }
end

module ServerInit = struct
  type t = { width: int; height: int;
             name: string;
             pixelformat: PixelFormat.t }

  let marshal (x: t) =
    let width = UInt16.marshal x.width in
    let height = UInt16.marshal x.height in
    let pixel = PixelFormat.marshal x.pixelformat in
    let name_length = UInt32.marshal (Int32.of_int (String.length x.name)) in
    width ^ height ^ pixel ^ name_length ^ x.name
end

module SetPixelFormat = struct
  type t = PixelFormat.t
  let marshal (x: t) =
    let ty = "\000" in
    let padding = "\000\000\000" in
    ty ^ padding ^ (PixelFormat.marshal x)

  let unmarshal (s: Unix.file_descr) =
    ignore(really_read s 3);
    PixelFormat.unmarshal s

  let prettyprint (x: t) =
    Printf.sprintf "SetPixelFormat (bpp=%d depth=%d)"
      x.PixelFormat.bpp x.PixelFormat.depth
end

module SetEncodings = struct
  type t = UInt32.t list

  let unmarshal (s: Unix.file_descr) =
    ignore(really_read s 1); (* padding *)
    let num = UInt16.unmarshal (really_read s 2) in
    let encodings = ref [] in
    for i = 1 to num do
      encodings := UInt32.unmarshal (really_read s 4) :: !encodings
    done;
    List.rev !encodings

  let prettyprint (x: t) =
    Printf.sprintf "SetEncodings (num=%d)" (List.length x)
end

module FramebufferUpdateRequest = struct
  type t = { incremental: bool;
             x: int; y: int;
             width: int; height: int }

  let unmarshal (s: Unix.file_descr) =
    let buf = really_read s 9 in
    { incremental = buf.[0] <> '\000';
      x = UInt16.unmarshal (String.sub buf 1 2);
      y = UInt16.unmarshal (String.sub buf 3 2);
      width = UInt16.unmarshal (String.sub buf 5 2);
      height = UInt16.unmarshal (String.sub buf 7 2);
    }
  let prettyprint (x: t) =
    Printf.sprintf "FrameBufferUpdateRequest (incr=%b x=%d y=%d width=%d height=%d)" x.incremental x.x x.y x.width x.height
end

module FramebufferUpdate = struct
  module Raw = struct
    (* width * height * bpp *)
    type t = { buffer: string }
    let sizeof (x: t) = String.length x.buffer
    let marshal (x: t) = x.buffer
    let marshal_at (buf: string) (off: int) (x: t) =
      let length = sizeof x in
      blit x.buffer 0 buf off length;
      off + length
  end
  module CopyRect = struct
    type t = { x: int; y: int }
    let sizeof (x: t) = 2 + 2
    let marshal (x: t) =
      UInt16.marshal x.x ^ (UInt16.marshal x.y)
    let marshal_at (buf: string) (off: int) (x: t) =
      let off = UInt16.marshal_at buf off x.x in
      UInt16.marshal_at buf off x.y
    let prettyprint (x: t) =
      Printf.sprintf "{ x = %d; y = %d }" x.x x.y
  end
  module Encoding = struct
    type t =
      | Raw of Raw.t
      | CopyRect of CopyRect.t
      | DesktopSize
    let sizeof (x: t) = match x with
      | Raw x -> 4 + Raw.sizeof x
      | CopyRect x -> 4 + CopyRect.sizeof x
      | DesktopSize -> 4
    let marshal (x: t) = match x with
      | Raw x -> UInt32.marshal 0l ^ (Raw.marshal x)
      | CopyRect x -> UInt32.marshal 1l ^ (CopyRect.marshal x)
      | DesktopSize -> UInt32.marshal (-223l)
    let marshal_at (buf: string) (off: int) (x: t) = match x with
      | Raw x ->
        let off = UInt32.marshal_at buf off 0l in
        Raw.marshal_at buf off x
      | CopyRect x ->
        let off = UInt32.marshal_at buf off 1l in
        CopyRect.marshal_at buf off x
      | DesktopSize ->
        UInt32.marshal_at buf off (-223l)
    let prettyprint = function
      | Raw _ -> "Raw"
      | CopyRect x -> "CopyRect " ^ (CopyRect.prettyprint x)
      | DesktopSize -> "DesktopSize"
  end
  type t = { x: int; y: int; w: int; h: int; encoding: Encoding.t }
  let sizeof (xs: t list) =
    let one (one: t) = 2 + 2 + 2 + 2 + (Encoding.sizeof one.encoding) in
    2 (* \000\000 *) + 2 (* length *) + (List.fold_left (+) 0 (List.map one xs))
  let marshal_at (buf: string) (off: int) (xs: t list) =
    let off = UInt16.marshal_at buf off 0 in
    let off = UInt16.marshal_at buf off (List.length xs) in
    let update (buf: string) (off: int) (one: t) =
      let off = UInt16.marshal_at buf off one.x in
      let off = UInt16.marshal_at buf off one.y in
      let off = UInt16.marshal_at buf off one.w in
      let off = UInt16.marshal_at buf off one.h in
      Encoding.marshal_at buf off one.encoding in
    List.fold_left (fun off x -> update buf off x) off xs
  let marshal (xs: t list) =
    let update (one: t) =
      let x = UInt16.marshal one.x and y = UInt16.marshal one.y in
      let w = UInt16.marshal one.w and h = UInt16.marshal one.h in
      x ^ y ^ w ^ h ^ (Encoding.marshal one.encoding) in
    let length = UInt16.marshal (List.length xs) in
    "\000\000" ^ length ^ (String.concat "" (List.map update xs))
end

module SetColourMapEntries = struct
  type t = { first_colour: int;
             map: (int * int * int) list }
  let marshal (x: t) =
    let first_colour = UInt16.marshal x.first_colour in
    let length = UInt16.marshal (List.length x.map) in
    let colour (r, g, b) =
      UInt16.marshal r ^ (UInt16.marshal g) ^ (UInt16.marshal b) in
    "\001\000" ^ first_colour ^ length ^
    (String.concat "" (List.map colour x.map))
end

module KeyEvent = struct
  type t = { down: bool; key: UInt32.t }

  let unmarshal (s: Unix.file_descr) =
    let buf = really_read s 7 in
    { down = buf.[0] <> '\000';
      key = UInt32.unmarshal (String.sub buf 3 4) }
  let prettyprint (x: t) =
    Printf.sprintf "KeyEvent { down = %b; key = %s }"
      x.down (Int32.to_string x.key)
end

module PointerEvent = struct
  type t = { mask: int; x: int; y: int }

  let unmarshal (s: Unix.file_descr) =
    let buf = really_read s 5 in
    { mask = int_of_char buf.[0];
      x = UInt16.unmarshal (String.sub buf 1 2);
      y = UInt16.unmarshal (String.sub buf 3 2);
    }
  let prettyprint (x: t) =
    Printf.sprintf "PointerEvent { mask = %d; x = %d; y = %d }"
      x.mask x.x x.y
end

module ClientCutText = struct
  type t = string

  let unmarshal (s: Unix.file_descr) =
    let buf = really_read s 7 in
    let length = UInt32.unmarshal (String.sub buf 3 4) in
    really_read s (Int32.to_int length)
  let prettyprint (x: t) =
    Printf.sprintf "ClientCutText { %s }" x
end

module Request = struct
  type t =
    | SetPixelFormat of SetPixelFormat.t
    | SetEncodings of SetEncodings.t
    | FrameBufferUpdateRequest of FramebufferUpdateRequest.t
    | KeyEvent of KeyEvent.t
    | PointerEvent of PointerEvent.t
    | ClientCutText of ClientCutText.t

  let prettyprint = function
    | SetPixelFormat x -> SetPixelFormat.prettyprint x
    | SetEncodings x -> SetEncodings.prettyprint x
    | FrameBufferUpdateRequest x -> FramebufferUpdateRequest.prettyprint x
    | KeyEvent x -> KeyEvent.prettyprint x
    | PointerEvent x -> PointerEvent.prettyprint x
    | ClientCutText x -> ClientCutText.prettyprint x

  let unmarshal (s: Unix.file_descr) =
    match int_of_char (really_read s 1).[0] with
    | 0 ->
      SetPixelFormat (SetPixelFormat.unmarshal s)
    | 2 ->
      SetEncodings (SetEncodings.unmarshal s)
    | 3 ->
      FrameBufferUpdateRequest (FramebufferUpdateRequest.unmarshal s)
    | 4 ->
      KeyEvent (KeyEvent.unmarshal s)
    | 5 ->
      PointerEvent (PointerEvent.unmarshal s)
    | 6 ->
      ClientCutText (ClientCutText.unmarshal s)
    | x ->
      failwith (Printf.sprintf "Unknown message type: %d" x)
end

let white = (255, 255, 255)
let black = (0, 0, 0)

let handshake w h (s: Unix.file_descr) =
  let ver = { ProtocolVersion.major = 3; minor = 3 } in
  really_write s (ProtocolVersion.marshal ver);
  let ver' = ProtocolVersion.unmarshal s in
  print_endline (ProtocolVersion.prettyprint ver');
  really_write s (SecurityType.marshal SecurityType.NoSecurity);
  let ci = ClientInit.unmarshal s in
  if ci then print_endline "Client requests a shared display"
  else print_endline "Client requests a non-shared display";
  let si = { ServerInit.name = "dave's desktop";
             width = w; height = h;
             pixelformat = PixelFormat.true_colour_default false } in
  really_write s (ServerInit.marshal si)

