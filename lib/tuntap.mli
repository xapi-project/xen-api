(*
 * Copyright Citrix
 *)

val tap_open: string -> Unix.file_descr
(** [tap_open ifname] opens /dev/net/tun for interface [ifname]. *)

(** [with_tap ifname fn] applies [fn] to the file descriptor for [ifname]
 * and closes the file descriptor subsequently even in the presence of
 * exceptions *)
val with_tap: string -> fn:(Unix.file_descr -> 'a) -> 'a
