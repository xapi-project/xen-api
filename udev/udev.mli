external socket_netlink_udev : unit -> Unix.file_descr
  = "stub_socket_netlink_udev"
external bind_netlink_udev : Unix.file_descr -> unit
  = "stub_bind_netlink_udev"
external receive_events_udev : Unix.file_descr -> string = "stub_receive_events_udev"

exception Timeout

val wait_for : string -> string -> float -> unit
