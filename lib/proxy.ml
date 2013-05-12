open Lwt

exception Short_write of int * int
exception End_of_file

let proxy a b =
  let copy id src dst =
    let buffer = String.make 16384 '\000' in
      try_lwt
        while_lwt true do
          lwt n = Lwt_unix.read src buffer 0 (String.length buffer) in
          if n = 0
          then raise_lwt End_of_file
          else
            lwt m = Lwt_unix.write dst buffer 0 n in
            if n <> m then raise_lwt (Short_write(m, n))
            else return ()
        done
      with e ->
        (try Lwt_unix.shutdown src Lwt_unix.SHUTDOWN_RECEIVE with _ -> ());
        (try Lwt_unix.shutdown dst Lwt_unix.SHUTDOWN_SEND with _ -> ());
        return () in
    let ts = [ copy "ab" a b; copy "ba" b a ] in
    lwt () = Lwt.join ts in
    return ()

let file_descr_of_int (x: int) : Unix.file_descr =
  Obj.magic x (* Keep this in sync with ocaml's file_descr type *)

let proxy_socket = ref None
let ip = ref "127.0.0.1"
let unix = ref "/tmp"

let main () =
  Arg.parse [
    "-ip", Arg.Set_string ip, (Printf.sprintf "IP address to bind to (default %s)" !ip);
    "-unix", Arg.Set_string unix, (Printf.sprintf "Path to bind a unix domain socket in (default %s)" !unix);
    "-proxy", Arg.Int (fun x -> proxy_socket := Some x), "file-descriptor to proxy I/O to";
  ] (fun x -> Printf.fprintf stderr "Unknown argument: %s" x)
    "accept one connection and proxy I/O to a second file-desriptor";

  let proxy_socket = match !proxy_socket with
  | Some x ->
    file_descr_of_int x
  | None ->
    Printf.fprintf stderr "You must provide a -proxy argument\n%!";
    exit 1 in

  let s_ip = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.bind s_ip (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0));
  Lwt_unix.listen s_ip 5;
  let port = match Lwt_unix.getsockname s_ip with
  | Unix.ADDR_INET(_, port) -> port
  | _ -> assert false in

  let s_unix = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in

  (* Try to avoid polluting the filesystem with unused unix domain sockets *)
  let path = Printf.sprintf "%s/%s.%d" !unix
    (Filename.basename Sys.argv.(0)) (Unix.getpid ()) in
  if Sys.file_exists path then Unix.unlink path;
  Lwt_unix.bind s_unix (Lwt_unix.ADDR_UNIX path);
  List.iter (fun signal ->
    ignore(Lwt_unix.on_signal signal (fun _ -> Unix.unlink path; exit 1))
  ) [ Sys.sigterm; Sys.sigint ];

  Lwt_unix.listen s_unix 5;

  let token = "token" in
  let protocols =
    let open Xcp_channel in
    [
      TCP_proxy(!ip, port);
      Unix_sendmsg(0, path, token);
    ] in
    Printf.fprintf stdout "%s\n%!" (Jsonrpc.to_string (Xcp_channel.rpc_of_protocols protocols));

  let t_ip =
    lwt fd, peer = Lwt_unix.accept s_ip in
    lwt () = Lwt_unix.close s_ip in
    proxy fd (Lwt_unix.of_unix_file_descr proxy_socket) in
  let t_unix =
    lwt fd, peer = Lwt_unix.accept s_unix in
    let buffer = String.make (String.length token) '\000' in
    let io_vector = Lwt_unix.io_vector ~buffer ~offset:0 ~length:(String.length buffer) in
    lwt n, fds = Lwt_unix.recv_msg ~socket:fd ~io_vectors:[io_vector] in
    List.iter Unix.close fds;
    let token' = String.sub buffer 0 n in
    let io_vector' = Lwt_unix.io_vector ~buffer:token' ~offset:0 ~length:(String.length token') in
    if token = token'
    then
      lwt _ = Lwt_unix.send_msg ~socket:fd ~io_vectors:[io_vector'] ~fds:[proxy_socket] in
      return ()
    else return () in
  lwt () = Lwt.pick [ t_ip; t_unix ] in
  Unix.unlink path;
  return ()

let _ =
  Lwt_main.run (main ())
