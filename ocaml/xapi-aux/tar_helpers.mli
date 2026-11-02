val write_block :
  Tar.Header.t -> (Unix.file_descr -> unit) -> Unix.file_descr -> unit

val write_end : Unix.file_descr -> unit

val skip : Unix.file_descr -> int -> unit

val copy_n : Unix.file_descr -> Unix.file_descr -> int64 -> unit

type probe =
  | Header of {hdr: Tar.Header.t; bytes_read: int}
      (** [bytes_read]: bytes taken off the fd for the header, including a pax
          extended-header record *)
  | Not_tar of Cstruct.t  (** the first block, to sniff and replay *)

val probe_header : Unix.file_descr -> probe
(** Read the first tar header of a stream that cannot be rewound, such as a
    socket. The first block decides the format: if the parse fails before any
    byte past it is read, return [Not_tar] with that block; if it fails later,
    raise. *)
