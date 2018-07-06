(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray
type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

let timestamper log_buffer ofs =
  let ns = Mtime.to_uint64_ns @@ Mtime_clock.now () in
  EndianBigstring.LittleEndian.set_int64 log_buffer ofs ns

let mmap_buffer ~size path =
  let fd = Unix.(openfile path [O_RDWR; O_CREAT; O_TRUNC] 0o644) in
  Unix.set_close_on_exec fd;
  Unix.ftruncate fd size;
  let ba = Array1.map_file fd char c_layout true size in
  Unix.close fd;
  ba
