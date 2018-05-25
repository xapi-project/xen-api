let my_domid = 0 (* TODO: figure this out *)

exception End_of_file
exception Channel_setup_failed

module CBuf = struct
  (** A circular buffer constructed from a string *)
  type t = {
    mutable buffer: bytes; 
    mutable len: int;       (** bytes of valid data in [buffer] *)
    mutable start: int;     (** index of first valid byte in [buffer] *)
    mutable r_closed: bool; (** true if no more data can be read due to EOF *)
    mutable w_closed: bool; (** true if no more data can be written due to EOF *)
  }

  let empty length = {
    buffer = Bytes.create length;
    len = 0;
    start = 0;
    r_closed = false;
    w_closed = false;
  }

  let drop (x: t) n =
    if n > x.len then failwith (Printf.sprintf "drop %d > %d" n x.len);
    x.start <- (x.start + n) mod (Bytes.length x.buffer);
    x.len <- x.len - n

  let should_read (x: t) =
    not x.r_closed && (x.len < (Bytes.length x.buffer - 1))
  let should_write (x: t) =
    not x.w_closed && (x.len > 0)

  let end_of_reads (x: t) = x.r_closed && x.len = 0
  let end_of_writes (x: t) = x.w_closed

  let write (x: t) fd =
    (* Offset of the character after the substring *)
    let next = min (Bytes.length x.buffer) (x.start + x.len) in
    let len = next - x.start in
    let written = try Unix.single_write fd x.buffer x.start len with _e -> x.w_closed <- true; len in
    drop x written

  let read (x: t) fd =
    (* Offset of the next empty character *)
    let next = (x.start + x.len) mod (Bytes.length x.buffer) in
    let len = min (Bytes.length x.buffer - next) (Bytes.length x.buffer - x.len) in
    let read = Unix.read fd x.buffer next len in
    if read = 0 then x.r_closed <- true;
    x.len <- x.len + read
end

let proxy (a: Unix.file_descr) (b: Unix.file_descr) =
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
    (try Unix.clear_nonblock b with _ -> ())

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let ip = ref "127.0.0.1"

let send proxy_socket =
  let to_close = ref [] in
  let to_unlink = ref [] in

  finally
    (fun () ->
      let s_ip = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      to_close := s_ip :: !to_close;
      Unix.bind s_ip (Unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0));
      Unix.listen s_ip 5;
      let port = match Unix.getsockname s_ip with
      | Unix.ADDR_INET(_, port) -> port
      | _ -> assert false in

      let s_unix = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      to_close := s_unix :: !to_close;
      let path = Filename.temp_file "channel" "" in
      to_unlink := path :: !to_unlink;
      if Sys.file_exists path then Unix.unlink path;
      Unix.bind s_unix (Unix.ADDR_UNIX path);
      Unix.listen s_unix 5;

      let token = "token" in
      let protocols =
      let open Xcp_channel_protocol in
      [
        TCP_proxy(!ip, port);
        Unix_sendmsg(my_domid, path, token);
      ] in

      (* We need to hang onto a copy of the proxy_socket so we can
         run a proxy in a background thread, allowing the caller to
         close their copy. *)
      let proxy_socket = Unix.dup proxy_socket in
      to_close := proxy_socket :: !to_close;

      let (_: Thread.t) = Thread.create (fun (fds, paths) ->
        (* The thread takes over management of the listening sockets *)
        let to_close = ref fds in
        let to_unlink = ref paths in

        let close fd =
          if List.mem fd !to_close then begin
            to_close := List.filter (fun x -> x <> fd) !to_close;
            Unix.close fd;
          end in

        finally
          (fun () -> 
            let readable, _, _ = Unix.select [ s_ip; s_unix ] [] [] (-1.0) in
            if List.mem s_unix readable then begin
              let fd, _peer = Unix.accept s_unix in
              to_close := fd :: !to_close;
              let buffer = Bytes.make (String.length token) '\000' in
              let n = Unix.recv fd buffer 0 (Bytes.length buffer) [] in
              let token' = Bytes.sub_string buffer 0 n in
              if token = token' then begin
                let (_: int) = Fd_send_recv.send_fd_substring fd token 0 (String.length token) [] proxy_socket in
                ()
              end
            end else if List.mem s_ip readable then begin
              let fd, _peer = Unix.accept s_ip in

              List.iter close !to_close;
              to_close := fd :: !to_close;
              proxy fd proxy_socket
            end else assert false (* can never happen *)
         ) (fun () ->
           List.iter close !to_close;
           List.iter Unix.unlink !to_unlink;
         )
      ) (!to_close, !to_unlink) in
      (* Handover of listening sockets successful *)
      to_close := [];
      to_unlink := [];
      protocols
    ) (fun () ->
      List.iter Unix.close !to_close;
      List.iter Unix.unlink !to_unlink;
    )

let receive protocols =
  let open Xcp_channel_protocol in
  let weight = function
  | TCP_proxy(_, _) -> 2
  | Unix_sendmsg(domid, _, _) -> if my_domid = domid then 3 else 0
  | V4V_proxy(_, _) -> 0 in
  let protocol = match List.sort (fun a b -> compare (weight b) (weight a)) protocols with
  | [] ->
    raise Channel_setup_failed
  | best :: _ ->
    if weight best = 0 then begin
      raise Channel_setup_failed
    end else best in
  match protocol with
  | V4V_proxy(_, _) -> assert false (* weight is 0 above *)
  | TCP_proxy(ip, port) ->
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    begin
      try
        Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_of_string ip, port));
        s
      with e ->
        Unix.close s;
        raise e
    end
  | Unix_sendmsg(_, path, token) ->
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    finally
      (fun () ->
        Unix.connect s (Unix.ADDR_UNIX path);
        let (_: int) = Unix.send_substring s token 0 (String.length token) [] in
        let buf = Bytes.create (String.length token) in
        let (_, _, fd) = Fd_send_recv.recv_fd s buf 0 (Bytes.length buf) [] in
        fd
      ) (fun () -> Unix.close s)

