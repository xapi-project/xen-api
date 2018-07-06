val shadow_file : string

type t = {
  name     : string;
  passwd   : string;
  last_chg : int64;
  min      : int64;
  max      : int64;
  warn     : int64;
  inact    : int64;
  expire   : int64;
  flag     : int;
}

val to_string : t -> string

type db = t list

module Mem : sig
  type mem

  val to_mem : t -> mem
  val from_mem : mem -> t
end

val db_to_string : db -> string

val getspnam : string -> t option
val getspent : unit -> t option

val setspent : unit -> unit
val endspent : unit -> unit
val putspent : Passwd.file_descr -> t -> unit

(** Note that the simple locking functionality provided here is not
    suitable for multi-threaded applications: there is no protection
    against direct access of the shadow password file. Only programs
    that use [lckpwdf] will notice the lock. *)
val lckpwdf : unit -> bool
val ulckpwdf : unit -> bool

val with_lock : (unit -> 'a) -> 'a

val shadow_enabled : unit -> bool

val get_db : unit -> db
val update_db : db -> t -> db
val write_db : ?file:string -> db -> unit

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
