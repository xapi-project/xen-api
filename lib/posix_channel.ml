let my_domid = 0 (* TODO: figure this out *)

exception Short_write of int * int
exception End_of_file
exception No_useful_protocol
exception Channel_setup_failed

module CBuf = struct
  (** A circular buffer constructed from a string *)
  type t = {
    mutable buffer: string; 
    mutable len: int;       (** bytes of valid data in [buffer] *)
    mutable start: int;     (** index of first valid byte in [buffer] *)
    mutable r_closed: bool; (** true if no more data can be read due to EOF *)
    mutable w_closed: bool; (** true if no more data can be written due to EOF *)
  }

  let empty length = {
    buffer = String.create length;
    len = 0;
    start = 0;
    r_closed = false;
    w_closed = false;
  }

  let drop (x: t) n =
    if n > x.len then failwith (Printf.sprintf "drop %d > %d" n x.len);
    x.start <- (x.start + n) mod (String.length x.buffer);
    x.len <- x.len - n

  let should_read (x: t) =
    not x.r_closed && (x.len < (String.length x.buffer - 1))
  let should_write (x: t) =
    not x.w_closed && (x.len > 0)

  let end_of_reads (x: t) = x.r_closed && x.len = 0
  let end_of_writes (x: t) = x.w_closed

  let write (x: t) fd =
    (* Offset of the character after the substring *)
    let next = min (String.length x.buffer) (x.start + x.len) in
    let len = next - x.start in
    let written = try Unix.single_write fd x.buffer x.start len with e -> x.w_closed <- true; len in
    drop x written

  let read (x: t) fd =
    (* Offset of the next empty character *)
    let next = (x.start + x.len) mod (String.length x.buffer) in
    let len = min (String.length x.buffer - next) (String.length x.buffer - x.len) in
    let read = Unix.read fd x.buffer next len in
    if read = 0 then x.r_closed <- true;
    x.len <- x.len + read    
end

let proxy_and_close (a: Unix.file_descr) (b: Unix.file_descr) =
  let size = 64 * 1024 in
  (* [a'] is read from [a] and will be written to [b] *)
  (* [b'] is read from [b] and will be written to [a] *)
  let a' = CBuf.empty size and b' = CBuf.empty size in
  Unix.set_nonblock a;
  Unix.set_nonblock b;

  try
    while true do
      let r = (if CBuf.should_read a' then [ a ] else []) @ (if CBuf.should_read b' then [ b ] else []) in
      let w = (if CBuf.should_write a' then [ b ] else []) @ (if CBuf.should_write b' then [ a ] else []) in

      (* If we can't make any progress (because fds have been closed), then stop *)
      if r = [] && w = [] then raise End_of_file;

      let r, w, _ = Unix.select r w [] (-1.0) in
      (* Do the writing before the reading *)
      List.iter (fun fd -> if a = fd then CBuf.write b' a else CBuf.write a' b) w;
      List.iter (fun fd -> if a = fd then CBuf.read a' a else CBuf.read b' b) r;
      (* If there's nothing else to read or write then signal the other end *)
      List.iter
        (fun (buf, fd) ->
          if CBuf.end_of_reads buf then Unix.shutdown fd Unix.SHUTDOWN_SEND;
          if CBuf.end_of_writes buf then Unix.shutdown fd Unix.SHUTDOWN_RECEIVE
        ) [ a', b; b', a ]
    done
  with _ ->
    (try Unix.clear_nonblock a with _ -> ());
    (try Unix.clear_nonblock b with _ -> ());
    (try Unix.close a with _ -> ());
    (try Unix.close b with _ -> ())


let file_descr_of_int (x: int) : Unix.file_descr =
  Obj.magic x (* Keep this in sync with ocaml's file_descr type *)

let ip = ref "127.0.0.1"
let unix = ref "/tmp"

let send proxy_socket =

  let s_ip = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind s_ip (Unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0));
  Unix.listen s_ip 5;
  let port = match Unix.getsockname s_ip with
  | Unix.ADDR_INET(_, port) -> port
  | _ -> assert false in

  let s_unix = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in

  let path = Filename.temp_file "channel" "" in
  if Sys.file_exists path then Unix.unlink path;
  Unix.bind s_unix (Unix.ADDR_UNIX path);
  List.iter (fun signal ->
    Sys.set_signal signal (Sys.Signal_handle (fun _ ->
      Unix.unlink path;
      exit 1
    ))
  ) [ Sys.sigterm; Sys.sigint ];

  Unix.listen s_unix 5;

  let token = "token" in
  let protocols =
    let open Xcp_channel in
    [
      TCP_proxy(!ip, port);
      Unix_sendmsg(my_domid, path, token);
    ] in
  let (_: Thread.t) = Thread.create (fun () ->
    let readable, _, _ = Unix.select [ s_ip; s_unix ] [] [] (-1.0) in
    if List.mem t_unix readable then begin
      let fd, peer = Unix.accept s_unix in
      let buffer = String.make (String.length token) '\000' in
      let n = Unix.recv fd buffer 0 (String.length buffer( [] in
      let token' = String.sub buffer 0 n in
      if token = token' then begin
        Fd_send_recv.send_fd fd token 0 (String.length token) [] proxy_socket;
      end;
      Unix.close fd;
      Unix.close proxy_socket
    end else if List.mem t_ip then begin
      let fd, peer = Unix.accept s_ip in
      Unix.close s_ip;
      proxy_and_close fd proxy_socket
    end else assert false (* can never happen *)
  ) in
  protocols

let receive protocols =
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

