let project_url = "https://github.com/xen-org/xcp-idl"

open Lwt

let my_domid = 0 (* TODO: figure this out *)

exception Short_write of int * int
exception End_of_file
exception No_useful_protocol

let copy_all src dst =
  let buffer = String.make 16384 '\000' in
  while_lwt true do
    lwt n = Lwt_unix.read src buffer 0 (String.length buffer) in
    if n = 0
    then raise_lwt End_of_file
    else
      lwt m = Lwt_unix.write dst buffer 0 n in
      if n <> m then raise_lwt (Short_write(m, n))
      else return ()
  done

let proxy a b =
  let copy id src dst =
      try_lwt
        copy_all src dst
      with e ->
        (try Lwt_unix.shutdown src Lwt_unix.SHUTDOWN_RECEIVE with _ -> ());
        (try Lwt_unix.shutdown dst Lwt_unix.SHUTDOWN_SEND with _ -> ());
        return () in
    let ts = [ copy "ab" a b; copy "ba" b a ] in
    lwt () = Lwt.join ts in
    return ()

let file_descr_of_int (x: int) : Unix.file_descr =
  Obj.magic x (* Keep this in sync with ocaml's file_descr type *)

let ip = ref "127.0.0.1"
let unix = ref "/tmp"

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
    port: int;
  } with rpc

  let make verbose debug port =
    { verbose; debug; port }

  let to_string x = Jsonrpc.to_string (rpc_of_t x)
end

let _common_options = "COMMON OPTIONS"

open Cmdliner

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in 
  let port = 
    let doc = Printf.sprintf "Specify port to connect to the message switch." in
    Arg.(value & opt int 8080 & info ["port"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ port)

(* Help sections common to all commands *)
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Commands *)
let advertise_t common_options_t proxy_socket =

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
      Unix_sendmsg(my_domid, path, token);
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

let advertise common_options_t fd = match fd with
  | Some x ->
    Lwt_main.run (advertise_t common_options_t (file_descr_of_int x));
    `Ok ()
  | None ->
    `Error(true, "you must provide a file descriptor to proxy")

let advertise_cmd =
  let doc = "advertise a given channel represented as a file-descriptor" in
  let man = [
    `S "DESCRIPTION";
    `P "Advertises a given channel over as many protocols as possible, and waits for someone to connect.";
  ] @ help in
  let fd =
    let doc = Printf.sprintf "File descriptor to advertise" in
    Arg.(value & pos 0 (some int) None & info [] ~docv:"FD" ~doc) in
  Term.(ret(pure advertise $ common_options_t $ fd)),
  Term.info "advertise" ~sdocs:_common_options ~doc ~man

let connect_t common_options_t =
  lwt advertisement = match_lwt Lwt_io.read_line_opt Lwt_io.stdin with None -> return "" | Some x -> return x in
  let open Xcp_channel in
  let protocols = protocols_of_rpc (Jsonrpc.of_string advertisement) in
  let weight = function
  | TCP_proxy(_, _) -> 2
  | Unix_sendmsg(domid, _, _) -> if my_domid = domid then 2 else 0
  | V4V_proxy(_, _) -> 0 in
  lwt protocol = match List.sort (fun a b -> compare (weight a) (weight b)) protocols with
  | [] ->
    Printf.fprintf stderr "the advertisement included zero protocols\n";
    fail No_useful_protocol
  | best :: _ ->
    if weight best = 0 then begin
      Printf.fprintf stderr "none of the advertised protocols will work\n";
      fail No_useful_protocol
    end else return best in
  lwt fd = match protocol with
  | V4V_proxy(_, _) -> assert false (* weight is 0 above *)
  | TCP_proxy(ip, port) ->
    let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect s (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ip, port)) in
    return s
  | Unix_sendmsg(_, path, token) ->
    let s = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect s (Lwt_unix.ADDR_UNIX path) in
    let buffer = String.make (String.length token) '\000' in
    let io_vector = Lwt_unix.io_vector ~buffer ~offset:0 ~length:(String.length buffer) in
    lwt _ = Lwt_unix.send_msg ~socket:s ~io_vectors:[io_vector] ~fds:[] in
    lwt _, fds = Lwt_unix.recv_msg ~socket:s ~io_vectors:[io_vector] in
    if fds = [] then begin
      Printf.fprintf stderr "received no file descriptors in recv_msg\n";
      fail Not_found
    end else return (Lwt_unix.of_unix_file_descr (List.hd fds)) in
  let a = copy_all Lwt_unix.stdin fd in
  let b = copy_all fd Lwt_unix.stdout in
  Lwt.join [a; b] 

let connect common_options_t =
  Lwt_main.run(connect_t common_options_t);
  `Ok ()

let connect_cmd =
  let doc = "connect to a channel and proxy to the terminal" in
  let man = [
    `S "DESCRIPTION";
    `P "Connect to a channel which has been advertised and proxy I/O to the console. The advertisement will be read from stdin as a single line of text.";
  ] @ help in
  Term.(ret(pure connect $ common_options_t)),
  Term.info "connect" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "channel (file-descriptor) passing helper program" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "proxy" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [advertise_cmd; connect_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
