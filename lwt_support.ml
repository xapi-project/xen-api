
module LwtIteratee=Iteratees.Iteratee(Lwt)

let really_write fd str =
  let len = String.length str in
  let (>>=) = Lwt.bind in
  let rec inner written =
    Lwt_unix.write fd str written (len-written) >>= (fun n ->
      if n < (len-written) then inner (written+n) else Lwt.return ())
  in inner 0

let lwt_fd_enumerator fd =
  let (>>=) = Lwt.bind in
  let blocksize = 1024 in
  let str = String.create blocksize in
  let rec go = function
    | LwtIteratee.IE_cont (None,x) -> 
      Lwt_unix.read fd str 0 blocksize                               >>= fun n ->
      (if n=0 then x (Iteratees.Eof None) else x (Iteratees.Chunk (String.sub str 0 n))) >>= fun x -> 
      Lwt.return (fst x)                                             >>= fun x ->
      go x
	| x -> Lwt.return x 
  in go 
  
let lwt_enumerator file iter =
  let (>>=) = Lwt.bind in
  Lwt_unix.openfile file [Lwt_unix.O_RDONLY] 0o777 >>= fun fd -> 
  lwt_fd_enumerator fd iter

exception Host_not_found of string
    
let open_connection_fd host port =
  let (>>=) = Lwt.bind in
  let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  if Array.length he.Lwt_unix.h_addr_list = 0
  then (Lwt_unix.close s; Lwt.fail (Host_not_found host))
  else let ip = he.Unix.h_addr_list.(0) in
       let addr = Unix.ADDR_INET(ip, port) in
       Lwt_unix.connect s addr;
       Lwt.return s

