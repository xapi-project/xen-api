let project_url = "https://github.com/xen-org/xcp-idl"

open Lwt

let my_domid = 0 (* TODO: figure this out *)

exception Short_write of int * int
exception End_of_file
exception No_useful_protocol

let copy_all src dst =
  let buffer = Bytes.make 16384 '\000' in
  let rec loop () =
    Lwt_unix.read src buffer 0 (Bytes.length buffer) >>= fun n ->
    if n = 0
    then Lwt.fail End_of_file
    else
      Lwt_unix.write dst buffer 0 n >>= fun m ->
      if n <> m then Lwt.fail (Short_write(m, n))
      else loop ()
  in loop ()

let proxy a b =
  let copy _id src dst =
      Lwt.catch (fun () -> copy_all src dst)
        (fun _e ->
          (try Lwt_unix.shutdown src Lwt_unix.SHUTDOWN_RECEIVE with _ -> ());
          (try Lwt_unix.shutdown dst Lwt_unix.SHUTDOWN_SEND with _ -> ());
          return ()) in
  let ts = [ copy "ab" a b; copy "ba" b a ] in
  Lwt.join ts

let file_descr_of_int (x: int) : Unix.file_descr =
  Obj.magic x (* Keep this in sync with ocaml's file_descr type *)

let ip = ref "127.0.0.1"
let unix = ref "/tmp"

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
    port: int;
  } [@@deriving rpc]

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
let advertise_t _common_options_t proxy_socket =

  let s_ip = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  (* INET socket, can't block *)
  Lwt_unix.bind s_ip (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0)) >>= fun () ->
  Lwt_unix.listen s_ip 5;
  let port = match Lwt_unix.getsockname s_ip with
  | Unix.ADDR_INET(_, port) -> port
  | _ -> assert false in

  let s_unix = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in

  (* Try to avoid polluting the filesystem with unused unix domain sockets *)
  let path = Printf.sprintf "%s/%s.%d" !unix
    (Filename.basename Sys.argv.(0)) (Unix.getpid ()) in
  if Sys.file_exists path then Unix.unlink path;
  Lwt_unix.bind s_unix (Lwt_unix.ADDR_UNIX path) >>= fun () ->
  List.iter (fun signal ->
    ignore(Lwt_unix.on_signal signal (fun _ -> Unix.unlink path; exit 1))
  ) [ Sys.sigterm; Sys.sigint ];

  Lwt_unix.listen s_unix 5;

  let token = "token" in
  let protocols =
    let open Xcp_channel_protocol in
    [
      TCP_proxy(!ip, port);
      Unix_sendmsg(my_domid, path, token);
    ] in
    Printf.fprintf stdout "%s\n%!" (Jsonrpc.to_string (Xcp_channel.rpc_of_protocols protocols));

  let t_ip =
    Lwt_unix.accept s_ip >>= fun (fd, _peer) ->
    Lwt_unix.close s_ip >>= fun () ->
    proxy fd (Lwt_unix.of_unix_file_descr proxy_socket) in
  let t_unix =
    Lwt_unix.accept s_unix >>= fun (fd, _peer) ->
    let buffer = Bytes.make (String.length token) '\000' in
    let io_vector = Lwt_unix.io_vector ~buffer:(Bytes.unsafe_to_string buffer) ~offset:0 ~length:(Bytes.length buffer) in
    Lwt_unix.recv_msg ~socket:fd ~io_vectors:[io_vector] >>= fun (n, fds) ->
    List.iter Unix.close fds;
    let token' = Bytes.sub_string buffer 0 n in
    let io_vector' = Lwt_unix.io_vector ~buffer:token' ~offset:0 ~length:(String.length token') in
    if token = token'
    then
      Lwt_unix.send_msg ~socket:fd ~io_vectors:[io_vector'] ~fds:[proxy_socket] >>= fun _ -> return ()
    else return () in
  Lwt.pick [ t_ip; t_unix ] >>= fun () ->
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

let connect_t _common_options_t =
  Lwt_io.read_line_opt Lwt_io.stdin >>= (function | None -> return "" | Some x -> return x) >>= fun advertisement ->
  let open Xcp_channel in
  let fd = Lwt_unix.of_unix_file_descr (file_descr_of_t (t_of_rpc (Jsonrpc.of_string advertisement))) in
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
