open Lwt
open Cohttp

let port = ref 8080
let connections_at_a_time = 16

let root = Store.Node.create "" ()

let handle_one srvsockaddr (fd, sockaddr) =
  try_lwt
    let ic = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.input fd in
    Lwt_io.printf "handle_one\n" >>
    let t, u = Lwt.wait () in
    lwt req = Request.init_request ~clisockaddr:sockaddr ~srvsockaddr u ic in
    Lwt_io.printf "  %s\n" (Request.path req) >>
    return ()
  finally
    Lwt_unix.close fd

let background (x: 'a Lwt.t) = ()

let (++) f g x = f (g x)

let main () =
  Arg.parse [
    "-port", Arg.Set_int port, "port to listen on"
  ] (fun _ -> ())
  "Start listening for requests";

  let sockaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, !port) in
  let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket 5;
  while_lwt true do
    lwt accepted, exn_opt = Lwt_unix.accept_n socket connections_at_a_time in
    List.iter (background ++ (handle_one sockaddr)) accepted;
    match exn_opt with
    | Some exn ->
      Lwt_io.printf "Exception: %s" (Printexc.to_string exn)
    | None -> return ()
  done >> return ()

let _ = Lwt_main.run (main ())