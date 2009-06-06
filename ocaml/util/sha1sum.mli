
(** Takes a function which is supplied with an fd representing the input to the
    sha1sum and returns the checksum as a string *)
val sha1sum: (Unix.file_descr -> unit) -> string
