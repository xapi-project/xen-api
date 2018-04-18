(*
 * Copyright (C) 2010 Citrix Systems Inc.
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
(** Utility functions for reading and writing disk blocks to/from a network stream.
 * @group Import and Export
*)

module Unmarshal = struct
  let int64 (s, offset) =
    let (<<<) a b = Int64.shift_left a b
    and (|||) a b = Int64.logor a b in
    let a = Int64.of_int (int_of_char (s.[offset + 0]))
    and b = Int64.of_int (int_of_char (s.[offset + 1]))
    and c = Int64.of_int (int_of_char (s.[offset + 2]))
    and d = Int64.of_int (int_of_char (s.[offset + 3]))
    and e = Int64.of_int (int_of_char (s.[offset + 4]))
    and f = Int64.of_int (int_of_char (s.[offset + 5]))
    and g = Int64.of_int (int_of_char (s.[offset + 6]))
    and h = Int64.of_int (int_of_char (s.[offset + 7])) in
    (a <<< 0) ||| (b <<< 8) ||| (c <<< 16) ||| (d <<< 24) ||| (e <<< 32) ||| (f <<< 40) ||| (g <<< 48) ||| (h <<< 56),
    (s, offset + 8)
  let int32 (s, offset) =
    let (<<<) a b = Int32.shift_left a b
    and (|||) a b = Int32.logor a b in
    let a = Int32.of_int (int_of_char (s.[offset + 0]))
    and b = Int32.of_int (int_of_char (s.[offset + 1]))
    and c = Int32.of_int (int_of_char (s.[offset + 2]))
    and d = Int32.of_int (int_of_char (s.[offset + 3])) in
    (a <<< 0) ||| (b <<< 8) ||| (c <<< 16) ||| (d <<< 24), (s, offset + 4)
end

module Marshal = struct
  let int64 x =
    let (>>>) a b = Int64.shift_right_logical a b
    and (&&&) a b = Int64.logand a b in
    let a = (x >>> 0) &&& 0xffL
    and b = (x >>> 8) &&& 0xffL
    and c = (x >>> 16) &&& 0xffL
    and d = (x >>> 24) &&& 0xffL
    and e = (x >>> 32) &&& 0xffL
    and f = (x >>> 40) &&& 0xffL
    and g = (x >>> 48) &&& 0xffL
    and h = (x >>> 56) &&& 0xffL in
    let result = Bytes.make 8 '\000' in
    List.iteri (fun i v ->
      Bytes.set result i (char_of_int @@ Int64.to_int v)
    ) [a; b; c; d; e; f; g; h];
    Bytes.unsafe_to_string result
  let int32 x =
    let (>>>) a b = Int32.shift_right_logical a b
    and (&&&) a b = Int32.logand a b in
    let a = (x >>> 0) &&& 0xffl
    and b = (x >>> 8) &&& 0xffl
    and c = (x >>> 16) &&& 0xffl
    and d = (x >>> 24) &&& 0xffl in
    let result = Bytes.make 4 '\000' in
    List.iteri (fun i v ->
      Bytes.set result i (char_of_int @@ Int32.to_int v)
    ) [a; b; c; d];
    Bytes.unsafe_to_string result

end

module Chunk = struct
  (** Represents an single block of data to write *)
  type t = {
    start: int64;
    data: string;
  }

  let really_write fd offset buf off len =
    let n = Unix.write fd buf off len in
    if n < len
    then failwith (Printf.sprintf "Short write: attempted to write %d bytes at %Ld, only wrote %d" len offset n)

  (** Writes a single block of data to the output device *)
  let write fd x =
    ignore(Unix.LargeFile.lseek fd x.start Unix.SEEK_SET);
    really_write fd x.start x.data 0 (String.length x.data)

  (** Reads a type t from a file descriptor *)
  let unmarshal fd =
    let buf = String.make 12 '\000' in
    Stdext.Unixext.really_read fd buf 0 (String.length buf);
    let stream = (buf, 0) in
    let start, stream = Unmarshal.int64 stream in
    let len, stream = Unmarshal.int32 stream in
    let payload = String.make (Int32.to_int len) '\000' in
    Stdext.Unixext.really_read fd payload 0 (String.length payload);
    { start = start; data = payload }

  (** Writes a type t from a file descriptor *)
  let marshal fd x =
    let start' = Marshal.int64 x.start in
    let len' = Marshal.int32 (Int32.of_int (String.length x.data)) in
    really_write fd 0L start' 0 (String.length start');
    really_write fd 8L len' 0 (String.length len');
    really_write fd 12L x.data 0 (String.length x.data)

  (** Fold [f] across all ts unmarshalled from [fd] *)
  let rec fold f init fd =
    let x = unmarshal fd in
    if x.data = ""
    then init
    else fold f (f init x) fd
end

