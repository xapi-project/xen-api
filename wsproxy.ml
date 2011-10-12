
let get_dir_path () = Printf.sprintf "/var/xapi/" 

(* Websockets protocol here *)

module Wsprotocol (IO : Iteratees.MonadIO) = struct

  module I = Iteratees.Iteratee(IO)
  open I

  let base64encode s = modify Base64.encode s
  let base64decode s = modify Base64.decode s
    
  let wsframe s = modify (fun s ->
    let l = String.length s in
    if l < 126 
    then 
      Printf.sprintf "%c%c%s" (char_of_int 0x81) (char_of_int l) s
    else if l < 65535 
    then
      Printf.sprintf "%c%c%s%s" (char_of_int 0x81) (char_of_int 126)
	(Helpers.marshal_int16 l) s
    else
      Printf.sprintf "%c%c%s%s" (char_of_int 0x81) (char_of_int 127)
	(Helpers.marshal_int32 (Int32.of_int l)) s) s
    
  let rec wsunframe x = 
    let read_sz =
      read_int8 >>= fun sz ->
      return (sz >= 128, sz land 0x7f)
    in
    let read_size sz = 
      if sz < 126 
      then return sz
      else if sz = 126 then
	read_int16
      else if sz = 127 then
	read_int32 >>= fun x -> return (Int32.to_int x)
      else
	ie_errM "Invalid length specifier"
    in  
    let read_mask has_mask =
      if has_mask 
      then readn 4
      else return "\x00\x00\x00\x00" 
    in
    let rec inner acc s = 
      match s with 
	| IE_cont (None, k) ->
	  begin
	    read_int8                    >>= fun op ->
	    read_sz                      >>= fun (has_mask, sz) ->
	    read_size sz                 >>= fun size ->
	    read_mask has_mask           >>= fun mask -> 
	    readn size                   >>= fun str ->
	    
	    let real_str = Helpers.unmask mask str in
	    if not (op land 0x80 = 0x80)
	    then inner (acc ^ real_str) s 
	    else 
              liftI (IO.bind (k (Iteratees.Chunk (acc ^ real_str))) (fun (i, _) ->
		IO.return (wsunframe i)))
	  end	
	| _ -> return s
    in inner "" x
end  


module LwtWsIteratee = Wsprotocol(Lwt)

let wsencode s = 
  let open LwtWsIteratee in 
      base64encode (wsframe s)

let wsdecode s =
  let open LwtWsIteratee in 
      wsunframe (base64decode s)
  

let start path handler =
  let dir_path = get_dir_path () in
  let fd_sock_path = Printf.sprintf "%s%s" dir_path path in
  let fd_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  lwt () = (try_lwt Lwt_unix.unlink fd_sock_path with _ -> Lwt.return ()) in
  let () = Lwt_unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path) in
  let () = Lwt_unix.listen fd_sock 5 in
  
  let rec loop () =
    lwt (fd_sock2,_) = Lwt_unix.accept fd_sock in
    let buffer = String.make 16384 '\000' in
    let iov = Lwt_unix.io_vector buffer 0 16384 in
    lwt (len,newfds) = 
      try
	Lwt_unix.recv_msg fd_sock2 [iov] 
      with e ->
	lwt () = Lwt_unix.close fd_sock2 in
	Lwt.return (0,[])
    in
    let msg = String.sub buffer 0 len in
    lwt _ = Lwt_unix.close fd_sock2 in
    List.iter (fun fd -> Printf.printf "got fd: %d" (Obj.magic fd)) newfds;
    ignore(handler (Lwt_unix.of_unix_file_descr (List.hd newfds)) msg);
    loop ()
  in
  loop ()

let ($) f x = f x

let proxy (fd : Lwt_unix.file_descr) localport =
  let open LwtWsIteratee in
  let open Lwt_support in
  let (>>=) = Lwt.bind in
  open_connection_fd "localhost" localport >>= fun localfd -> 
  let thread1 = lwt_fd_enumerator localfd $ wsencode (I.writer $ really_write fd) >>= fun _ -> Lwt.return () in
  let thread2 = lwt_fd_enumerator fd $ wsdecode (I.writer $ really_write localfd) >>= fun _ -> Lwt.return () in
  Lwt.join [thread1; thread2] >>= (fun _ -> Lwt_unix.close fd)

let handler sock msg =
  lwt _ = Lwt_io.printf "Got msg: %s\n" msg in
  match Stringext.String.split ':' msg with
    | [ty;sport] ->
      let port = int_of_string sport in
      proxy sock port
    | _ -> 
      Lwt_unix.close sock

let _ = 
  Lwt_main.run (start "wsproxy" handler)
