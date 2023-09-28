module type ALGORITHM = sig
  val executable : string

  val compress_options : string list

  val decompress_options : string list
end

module type COMPRESSOR = sig
  val available : unit -> bool
  (** Returns whether this compression algorithm is available *)

  val compress : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
  (** Runs a compression process which is fed from a pipe whose entrance
  is passed to 'f' and whose output is 'ofd' *)

  val decompress : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
  (** Runs a decompression process which is fed from a pipe whose
  entrance is passed to 'f' and whose output is 'ofd' *)

  val decompress_passive : Unix.file_descr -> (Unix.file_descr -> 'a) -> 'a
  (** Experimental decompressor which is fed from an fd and writes to a pipe *)

  val compress_file :
       (Unix.file_descr -> (Unix.file_descr -> unit) -> 'a)
    -> file_path:string
    -> file_ext:string
    -> unit
  (** Uses the compress function given to compress the specified file.
     Raises ENOENT if the file cannot be found and EACCES if it does not have access *)
end

module Make : functor (_ : ALGORITHM) -> COMPRESSOR
