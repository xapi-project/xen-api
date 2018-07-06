val passwd_file : string

type t = {
  name   : string;
  passwd : string;
  uid    : int;
  gid    : int;
  gecos  : string;
  dir    : string;
  shell  : string;
}

val to_string : t -> string

type db = t list

val db_to_string : db -> string

type file_descr
val file_descr : file_descr Ctypes.typ

val fopen : string -> string -> file_descr
val fclose : file_descr -> unit

val getpwnam : string -> t option
val getpwuid : int -> t option
val getpwent : unit -> t option
val setpwent : unit -> unit
val endpwent : unit -> unit
val putpwent : file_descr -> t -> unit

val get_db : unit -> db
val update_db : db -> t -> db
val write_db : ?file:string -> db -> unit

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
