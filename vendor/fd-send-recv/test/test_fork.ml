open Unix
open Fd_send_recv

let t_receivefd fd =
  Printf.printf "[receiver] t_receivedfd thread started!\n%!";
  let buf = Bytes.of_string "**" in
  let nb_read, remote_saddr, fd_recv = recv_fd fd buf 0 2 [] in
  Printf.printf "[receiver] Received %d bytes, received fd = %d\n%!" nb_read (int_of_fd fd_recv);
  let message = "[receiver] I'm the receiver, and I'm writing into the fd you passed to me :p\n" in
  let nb_written = write_substring fd_recv message 0 (String.length message) in
  assert (nb_written = String.length message)

let t_sendfd fd =
  let fd_to_send = stdout in
  let buf = "  " in
  let nb_sent = send_fd_substring fd buf 0 2 [] fd_to_send in
  Printf.printf "[sender] sent %d bytes, sent fd = %d\n%!" nb_sent (int_of_fd fd_to_send)

let main () =
  let s1, s2 = socketpair PF_UNIX SOCK_STREAM 0 in
  let cpid = Unix.fork () in
  if cpid = 0 (* Child code *)
  then
    begin
      t_receivefd s2
    end
  else (* Parent code *)
    begin
      t_sendfd s1;
      let cpid_, status = waitpid [] cpid in assert (cpid_ = cpid);
      Printf.printf "[sender]: Child died, exiting.\n%!"
    end

(* Entry point *)
let _ = main ()
