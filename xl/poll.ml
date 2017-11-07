(* event loop helper wrapping the poll syscall *)
external poll : (Unix.file_descr * Xenlight.event list) list -> int -> (Xenlight.event list list * int) = "stub_poll"

let poll (interrupt_in, fds_in) timeout_ms =
  match poll (interrupt_in :: fds_in) timeout_ms with
  | [], _ -> failwith "poll binding returned empty list" (* impossible *)
  | (interrupt_out :: fds_out), rc ->  (interrupt_out, fds_out), rc
