open Unix
open Fd_send_recv

let t_receivefd fd =
  Printf.printf "[receiver] t_receivedfd thread started!\n%!";
  let buf = Bytes.make (Tuntap.get_ifnamsiz ()) '\000' in
  let nb_read, remote_saddr, fd_recv = recv_fd fd buf 0 (Bytes.length buf) [] in
  Printf.printf "[receiver] Received %d bytes [%s], received fd = %d\n%!" 
    nb_read (Bytes.sub_string buf 0 nb_read) (int_of_fd fd_recv)

let t_sendfd fd =
  let fd_to_send, iface_name = Tuntap.opentap () in
  let nb_sent = send_fd_substring fd iface_name 0 (String.length iface_name) [] fd_to_send in
  Printf.printf "[sender] sent %d bytes [%s], sent fd = %d\n%!" nb_sent
    (String.sub iface_name 0 nb_sent) (int_of_fd fd_to_send)

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
