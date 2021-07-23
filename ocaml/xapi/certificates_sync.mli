val update :
  __context:Context.t -> (unit, [> `Msg of string * 'a list]) Result.result
(** inspect the host's certificate in /etx/xensource/xapi_ssl.pem
(default) and update it's database entry in case it has changed. In
effect this creates a new entry and removes the stale entry. *)

val host_certs_of :
  __context:Context.t -> API.ref_host -> API.ref_Certificate list

val eject_certs_from_db :
  __context:Context.t -> API.ref_Certificate list -> unit
(** remove a cert from the database. A cert has public and private parts,
   which are covered by the same entry in the database *)

val eject_certs_from_fs_for : __context:Context.t -> API.ref_host -> unit
(** remove the public (distributed) part of a certficate from the file
   system. This is the public part of certificate from another host that
   is stored in the filesystem of the host where this code is eexecuted.  *)
