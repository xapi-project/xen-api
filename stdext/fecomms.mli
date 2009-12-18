val open_unix_domain_sock : unit -> Unix.file_descr
val open_unix_domain_sock_server : string -> Unix.file_descr
val open_unix_domain_sock_client : string -> Unix.file_descr
val read_raw_rpc : Unix.file_descr -> Fe.ferpc
val write_raw_rpc : Unix.file_descr -> Fe.ferpc -> unit
exception Connection_closed
val receive_named_fd : Unix.file_descr -> Unix.file_descr * string
val send_named_fd : Unix.file_descr -> string -> Unix.file_descr -> unit
