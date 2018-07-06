(* Copyright (C) 2014, Thomas Leonard *)

(** Trace processes on Unix, keeping the results in a file. *)

open Bigarray
type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

val timestamper : log_buffer -> int -> unit

val mmap_buffer : size:int -> string -> log_buffer
(** [mmap_buffer ~size path] is a trace buffer that is backed by the
    file at [path] (which is created if it doesn't already exist). *)
