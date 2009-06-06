
exception InvalidSignature

val with_signed_cleartext: string -> (string option -> Unix.file_descr -> 'a) -> 'a

val with_detached_signature: string -> string -> Int64.t -> (string option -> Unix.file_descr -> 'a) -> 'a

val simple_checksum: string -> string
