(* event loop helper wrapping the poll syscall *)
external poll : (Unix.file_descr * Xenlight.event list) list -> int -> (Xenlight.event list list * int) = "stub_poll"

