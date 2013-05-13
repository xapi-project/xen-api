
exception Channel_setup_failed

type t = Unix.file_descr

let file_descr_of_t t = t
let t_of_file_descr t = t

type protocol =
  | TCP_proxy of string * int             (** IP, port *)
  | V4V_proxy of int * int                (** domid, port *)
  | Unix_sendmsg of int * string * string (** domid, path, token *)
with rpc

type protocols = protocol list with rpc

let int_of_file_descr (fd: Unix.file_descr) : int = Obj.magic fd

let export fd =
  let ic = Unix.open_process_in (Printf.sprintf "%s advertise %d" !Xcp_service.channel_helper (int_of_file_descr fd)) in
  let json = input_line ic in
  close_in ic;
  protocols_of_rpc (Jsonrpc.of_string json)

let rpc_of_t fd =
  (* Advertise the fd's availability over a list of protocols *)
  rpc_of_protocols (export fd)

let my_domid = 0 (* TODO *)

let t_of_rpc x =
  let protocols = protocols_of_rpc x in
  (* Choose the best transport mechanism from the list of options *)
  let weight = function
  | TCP_proxy(_, _) -> 2
  | Unix_sendmsg(domid, _, _) -> if my_domid = domid then 3 else 0
  | V4V_proxy(_, _) -> 0 in
  let protocol = match List.sort (fun a b -> compare (weight b) (weight a)) protocols with
  | [] ->
    Printf.fprintf stderr "the advertisement included zero protocols\n";
    raise Channel_setup_failed
  | best :: _ ->
    if weight best = 0 then begin
      Printf.fprintf stderr "none of the advertised protocols will work\n";
      raise Channel_setup_failed
    end else best in
  match protocol with
  | V4V_proxy(_, _) -> assert false (* weight is 0 above *)
  | TCP_proxy(ip, port) ->
    Printf.fprintf stderr "Attempting to connect to %s:%d\n%!" ip port;
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_of_string ip, port));
    s
  | Unix_sendmsg(_, path, token) ->
    Printf.fprintf stderr "Attempting to exchange a fd over %s\n%!" path;
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect s (Unix.ADDR_UNIX path);
    let (_: int) = Unix.send s token 0 (String.length token) [] in
    let (_, _, fd) = Fd_send_recv.recv_fd s token 0 (String.length token) [] in
    fd
