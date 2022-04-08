module type ALGORITHM = sig
  val executable : string
end

module Make : functor (Algorithm : ALGORITHM) -> sig
  val available : unit -> bool
  (** Returns whether this compression algorithm is available *)

  val compress : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
  (** Runs a compression process which is fed from a pipe whose entrance is passed to 'f'
      and whose output is 'ofd' *)

  val decompress : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
  (** Runs a decompression process which is fed from a pipe whose entrance is passed to 'f'
      and whose output is 'ofd' *)

  (* Experimental decompressor which is fed from an fd and writes to a pipe *)
  val decompress_passive : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
end
