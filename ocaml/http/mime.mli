type t
val mime_of_file : string -> t
val string_of_mime : t -> string
val mime_of_ext : t -> string -> string
val mime_of_file_name : t -> string -> string

