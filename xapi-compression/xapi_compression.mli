module type ALGORITHM = sig
  val executable : string
end

module Make : functor (Algorithm : ALGORITHM) -> sig
  (** Returns whether this compression algorithm is available *)
  val available: unit -> bool

  (** Runs a compression process which is fed from a pipe whose entrance is passed to 'f'
      and whose output is 'ofd' *)
  val compress: Unix.file_descr -> (Unix.file_descr -> unit) -> unit

  (** Runs a decompression process which is fed from a pipe whose entrance is passed to 'f'
      and whose output is 'ofd' *)
  val decompress: Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a

  (* Experimental decompressor which is fed from an fd and writes to a pipe *)
  val decompress_passive: Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
end
