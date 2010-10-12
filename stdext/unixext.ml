(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Pervasiveext

exception Unix_error of int

external _exit : int -> unit = "unix_exit"

(** remove a file, but doesn't raise an exception if the file is already removed *)
let unlink_safe file =
	try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

(** create a directory but doesn't raise an exception if the directory already exist *)
let mkdir_safe dir perm =
	try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist *)
let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "." 
		then p_mkdir p_name;
		mkdir_safe dir perm in
	p_mkdir dir

(** write a pidfile file *)
let pidfile_write filename =
	let fd = Unix.openfile filename
	                       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
			       0o640 in
	finally
	(fun () ->
		let pid = Unix.getpid () in
		let buf = string_of_int pid ^ "\n" in
		let len = String.length buf in
		if Unix.write fd buf 0 len <> len 
		then failwith "pidfile_write failed";
	)
	(fun () -> Unix.close fd)

(** read a pidfile file, return either Some pid or None *)
let pidfile_read filename =
	let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o640 in
	finally
	(fun () ->
		try
			let buf = String.create 80  in
			let rd = Unix.read fd buf 0 (String.length buf) in
			if rd = 0 then
				failwith "pidfile_read failed";
			Scanf.sscanf (String.sub buf 0 rd) "%d" (fun i -> Some i)
		with exn -> None)
	(fun () -> Unix.close fd)

(** daemonize a process *)
(* !! Must call this before spawning any threads !! *)
let daemonize () =
	match Unix.fork () with
	| 0 ->
		if Unix.setsid () == -1 then
			failwith "Unix.setsid failed";

		begin match Unix.fork () with
		| 0 ->
			let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
			begin try
				Unix.close Unix.stdin;
				Unix.dup2 nullfd Unix.stdout;
				Unix.dup2 nullfd Unix.stderr;
			with exn -> Unix.close nullfd; raise exn
			end;
			Unix.close nullfd
		| _ -> exit 0
		end
	| _ -> exit 0

exception Break

let file_lines_fold f start file_path =
	let input = open_in file_path in
	let rec fold accumulator =
		let line =
			try Some (input_line input)
			with End_of_file -> None in
		match line with
			| Some line -> (try fold (f accumulator line) with Break -> accumulator)
			| None -> accumulator in
	finally
		(fun () -> fold start)
		(fun () -> close_in input)

let file_lines_iter f file_path = file_lines_fold (fun () line -> ignore(f line)) () file_path

let readfile_line = file_lines_iter

(** open a file, and make sure the close is always done *)
let with_file file mode perms f =
	let fd = Unix.openfile file mode perms in
	let r =
		try f fd
		with exn -> Unix.close fd; raise exn
		in
	Unix.close fd;
	r

(** [fd_blocks_fold block_size f start fd] folds [f] over blocks (strings)
    from the fd [fd] with initial value [start] *)
let fd_blocks_fold block_size f start fd = 
	let block = String.create block_size in
	let rec fold acc = 
		let n = Unix.read fd block 0 block_size in
		(* Consider making the interface explicitly use Substrings *)
		let s = if n = block_size then block else String.sub block 0 n in
		if n = 0 then acc else fold (f acc s) in
	fold start

let with_directory dir f =
	let dh = Unix.opendir dir in
	let r =
		try f dh
		with exn -> Unix.closedir dh; raise exn
		in
	Unix.closedir dh;
	r

let buffer_of_fd fd = 
	fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

let bigbuffer_of_fd fd = 
	fd_blocks_fold 1024 (fun b s -> Bigbuffer.append_string b s; b) (Bigbuffer.make ()) fd

let string_of_fd fd = Buffer.contents (buffer_of_fd fd)

let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

let bigbuffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 bigbuffer_of_fd

let string_of_file file_path = Buffer.contents (buffer_of_file file_path)

(** Opens a temp file, applies the fd to the function, when the function completes, renames the file
    as required. *)
let atomic_write_to_file fname perms f =
  let tmp = Filenameext.temp_file_in_dir fname in
  Unix.chmod tmp perms;
  Pervasiveext.finally
    (fun () ->
      let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT] perms (* ignored since the file exists *) in
      let result = Pervasiveext.finally
	(fun () -> f fd)
	(fun () -> Unix.close fd) in
      Unix.rename tmp fname; (* Nb this only happens if an exception wasn't raised in the application of f *)
      result)
    (fun () -> unlink_safe tmp)


(** Atomically write a string to a file *)
let write_string_to_file fname s =
  atomic_write_to_file fname 0o644 (fun fd ->
    let len = String.length s in
    let written = Unix.write fd s 0 len in
    if written <> len then (failwith "Short write occured!"))


let execv_get_output cmd args =
	let (pipe_exit, pipe_entrance) = Unix.pipe () in
	let r = try Unix.set_close_on_exec pipe_exit; true with _ -> false in
	match Unix.fork () with
	| 0 ->
		Unix.dup2 pipe_entrance Unix.stdout;
		Unix.close pipe_entrance;
		if not r then
			Unix.close pipe_exit;
		begin try Unix.execv cmd args with _ -> exit 127 end
	| pid ->
		Unix.close pipe_entrance;
		pid, pipe_exit

(** Copy all data from an in_channel to an out_channel,
 * returning the total number of bytes *)
let copy_file ?limit ifd ofd =
	let buffer = String.make 65536 '\000' in
	let buffer_len = Int64.of_int (String.length buffer) in
	let finished = ref false in
	let total_bytes = ref 0L in
	let limit = ref limit in
	while not(!finished) do
		let requested = min (Opt.default buffer_len !limit) buffer_len in
		let num = Unix.read ifd buffer 0 (Int64.to_int requested) in
		let num64 = Int64.of_int num in

		limit := Opt.map (fun x -> Int64.sub x num64) !limit;
		ignore_int (Unix.write ofd buffer 0 num);
		total_bytes := Int64.add !total_bytes num64;
		finished := num = 0 || !limit = Some 0L;
	done;
	!total_bytes

let file_exists file_path =
	try Unix.access file_path [Unix.F_OK]; true
	with _ -> false

let touch_file file_path =
	let fd = Unix.openfile file_path
		[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_NOCTTY; Unix.O_NONBLOCK] 0o666 in
	Unix.close fd;
	Unix.utimes file_path 0.0 0.0

let is_empty_file file_path =
	try
		let stats = Unix.stat file_path in
		stats.Unix.st_size = 0
	with Unix.Unix_error (Unix.ENOENT, _, _) ->
		false

let delete_empty_file file_path =
	if is_empty_file file_path
	then (Sys.remove file_path; true)
	else (false)

(** Create a new file descriptor, connect it to host:port and return it *)
exception Host_not_found of string
let open_connection_fd host port =
	let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	try 
	  let he =
	    try
	      Unix.gethostbyname host
	    with
		Not_found -> raise (Host_not_found host) in
	  if Array.length he.Unix.h_addr_list = 0
	  then failwith (Printf.sprintf "Couldn't resolve hostname: %s" host);
	  let ip = he.Unix.h_addr_list.(0) in
	  let addr = Unix.ADDR_INET(ip, port) in
	  Unix.connect s addr;
	  s
	with e -> Unix.close s; raise e


let open_connection_unix_fd filename =
	let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	try
	  let addr = Unix.ADDR_UNIX(filename) in
	  Unix.connect s addr;
	  s
	with e -> Unix.close s; raise e

type endpoint = { fd: Unix.file_descr; mutable buffer: string; mutable buffer_len: int }

let make_endpoint fd = {
	fd = fd;
	buffer = String.make 4096 '\000';
	buffer_len = 0
}

exception Process_still_alive

let kill_and_wait ?(signal = Sys.sigterm) ?(timeout=10.) pid =
	let proc_entry_exists pid =
		try Unix.access (Printf.sprintf "/proc/%d" pid) [ Unix.F_OK ]; true
		with _ -> false
		in
	if pid > 0 && proc_entry_exists pid then (
		let loop_time_waiting = 0.03 in
		let left = ref timeout in
		let readcmdline pid =
			try string_of_file (Printf.sprintf "/proc/%d/cmdline" pid)
			with _ -> ""
			in
		let reference = readcmdline pid and quit = ref false in
		Unix.kill pid signal;

		(* We cannot do a waitpid here, since we might not be parent of
		   the process, so instead we are waiting for the /proc/%d to go
		   away. Also we verify that the cmdline stay the same if it's still here
		   to prevent the very very unlikely event that the pid get reused before
		   we notice it's gone *)
		while proc_entry_exists pid && not !quit && !left > 0.
		do
			let cmdline = readcmdline pid in
			if cmdline = reference then (
				(* still up, let's sleep a bit *)
				ignore (Unix.select [] [] [] loop_time_waiting);
				left := !left -. loop_time_waiting
			) else (
				(* not the same, it's gone ! *)
				quit := true
			)
		done;
		if !left <= 0. then
			raise Process_still_alive;
	)

let proxy (a: Unix.file_descr) (b: Unix.file_descr) =
	let a' = make_endpoint a and b' = make_endpoint b in
	Unix.set_nonblock a;
	Unix.set_nonblock b;

	let can_read x =
		x.buffer_len < (String.length x.buffer - 1) in
	let can_write x =
		x.buffer_len > 0 in
	let write_from x fd =
		let written = Unix.single_write fd x.buffer 0 x.buffer_len in
		String.blit x.buffer written x.buffer 0 (x.buffer_len - written);
		x.buffer_len <- x.buffer_len - written in
	let read_into x =
		let read = Unix.read x.fd x.buffer x.buffer_len (String.length x.buffer - x.buffer_len) in
		if read = 0 then raise End_of_file;
		x.buffer_len <- x.buffer_len + read in

	try
	while true do
		let r = (if can_read a' then [ a ] else []) @ (if can_read b' then [ b ] else []) in
		let w = (if can_write a' then [ b ] else []) @ (if can_write b' then [ a ] else []) in

		let r, w, _ = Unix.select r w [] (-1.0) in
		(* Do the writing before the reading *)
		List.iter (fun fd -> if a = fd then write_from b' a else write_from a' b) w;
		List.iter (fun fd -> if a = fd then read_into a' else read_into b') r
	done
	with _ ->
		(try Unix.clear_nonblock a with _ -> ());
		(try Unix.clear_nonblock b with _ -> ());
		(try Unix.close a with _ -> ());
		(try Unix.close b with _ -> ())

let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then raise End_of_file;
    really_read fd string (off+m) (n-m)

let really_read_string fd length =
  let buf = String.make length '\000' in
  really_read fd buf 0 length;
  buf

let really_read_bigbuffer fd bigbuf n =
	let chunk = 4096 in
	let s = String.make chunk '\000' in
	let written = ref 0L in
	while !written < n do
		let remaining = Int64.sub n !written in
		let to_write = min remaining (Int64.of_int chunk) in
		really_read fd s 0 (Int64.to_int to_write);
		Bigbuffer.append_substring bigbuf s 0 (Int64.to_int to_write);
		written := Int64.add !written to_write;
	done

let really_write fd string off n =
	let written = ref 0 in
	while !written < n
	do
		let wr = Unix.write fd string (off + !written) (n - !written) in
		written := wr + !written
	done

(* Ideally, really_write would be implemented with optional arguments ?(off=0) ?(len=String.length string) *)
let really_write_string fd string =
	really_write fd string 0 (String.length string)

(* --------------------------------------------------------------------------------------- *)
(* Functions to read and write to/from a file descriptor with a given latest response time *)

exception Timeout

(* Write as many bytes to a file descriptor as possible from data before a given clock time. *)
(* Raises Timeout exception if the number of bytes written is less than the specified length. *)
(* Writes into the file descriptor at the current cursor position. *)
let time_limited_write filedesc length data target_response_time =
  let total_bytes_to_write = length in
  let bytes_written = ref 0 in
  let now = ref (Unix.gettimeofday()) in
  while !bytes_written < total_bytes_to_write && !now < target_response_time do
    let remaining_time = target_response_time -. !now in
    let (_, ready_to_write, _) = Unix.select [] [filedesc] [] remaining_time in (* Note: there is a possibility that the storage could go away after the select and before the write, so the write would block. *)
    if List.mem filedesc ready_to_write then begin
      let bytes_to_write = total_bytes_to_write - !bytes_written in
      let bytes = (try Unix.write filedesc data !bytes_written bytes_to_write with Unix.Unix_error(Unix.EAGAIN,_,_) | Unix.Unix_error(Unix.EWOULDBLOCK,_,_) -> 0) in (* write from buffer=data from offset=bytes_written, length=bytes_to_write *)
      bytes_written := bytes + !bytes_written;
    end;
    now := Unix.gettimeofday()
  done;
  if !bytes_written = total_bytes_to_write then () else (* we ran out of time *) raise Timeout

(* Read as many bytes to a file descriptor as possible before a given clock time. *)
(* Raises Timeout exception if the number of bytes read is less than the desired number. *)
(* Reads from the file descriptor at the current cursor position. *)
let time_limited_read filedesc length target_response_time =
  let total_bytes_to_read = length in
  let bytes_read = ref 0 in
  let buf = String.make total_bytes_to_read '\000' in
  let now = ref (Unix.gettimeofday()) in
  while !bytes_read < total_bytes_to_read && !now < target_response_time do
    let remaining_time = target_response_time -. !now in
    let (ready_to_read, _, _) = Unix.select [filedesc] [] [] remaining_time in
    if List.mem filedesc ready_to_read then begin
      let bytes_to_read = total_bytes_to_read - !bytes_read in
      let bytes = (try Unix.read filedesc buf !bytes_read bytes_to_read with Unix.Unix_error(Unix.EAGAIN,_,_) | Unix.Unix_error(Unix.EWOULDBLOCK,_,_) -> 0) in (* read into buffer=buf from offset=bytes_read, length=bytes_to_read *)
      if bytes = 0 then raise End_of_file (* End of file has been reached *)
      else bytes_read := bytes + !bytes_read
    end;
    now := Unix.gettimeofday()
  done;
  if !bytes_read = total_bytes_to_read then buf else (* we ran out of time *) raise Timeout

(* --------------------------------------------------------------------------------------- *)

(* Read a given number of bytes of data from the fd, or stop at EOF, whichever comes first. *)
(* A negative ~max_bytes indicates that all the data should be read from the fd until EOF. This is the default. *)
let read_data_in_chunks (f : string -> int -> unit) ?(block_size = 1024) ?(max_bytes = -1) from_fd =
  let buf = String.make block_size '\000' in
  let rec do_read acc =
    let remaining_bytes = max_bytes - acc in
    if remaining_bytes = 0 then acc (* we've read the amount requested *)
    else begin
      let bytes_to_read = (if max_bytes < 0 || remaining_bytes > block_size then block_size else remaining_bytes) in
      let bytes_read = Unix.read from_fd buf 0 bytes_to_read in
      if bytes_read = 0 then acc (* we reached EOF *)
      else begin
        f (String.sub buf 0 bytes_read) bytes_read;
        do_read (acc + bytes_read)
      end
    end in
  do_read 0

let spawnvp ?(pid_callback=(fun _ -> ())) cmd args =
	match Unix.fork () with
	| 0 ->
		Unix.execvp cmd args
	| pid ->
		begin try pid_callback pid with _ -> () end;
		snd (Unix.waitpid [] pid)

let double_fork f =
	match Unix.fork () with
	| 0 ->
		begin match Unix.fork () with
		  (* NB: use _exit (calls C lib _exit directly) to avoid
		     calling at_exit handlers and flushing output channels
		     which wouild cause intermittent deadlocks if we
		     forked from a threaded program *)
		| 0 -> (try f () with _ -> ()); _exit 0
		| _ -> _exit 0
		end
	| pid -> ignore(Unix.waitpid [] pid)

external set_tcp_nodelay : Unix.file_descr -> bool -> unit = "stub_unixext_set_tcp_nodelay"

external fsync : Unix.file_descr -> unit = "stub_unixext_fsync"
external blkgetsize64 : Unix.file_descr -> int64 = "stub_unixext_blkgetsize64"

external get_max_fd : unit -> int = "stub_unixext_get_max_fd"

let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x
let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x

(** Forcibly closes all open file descriptors except those explicitly passed in as arguments.
    Useful to avoid accidentally passing a file descriptor opened in another thread to a
    process being concurrently fork()ed (there's a race between open/set_close_on_exec).
    NB this assumes that 'type Unix.file_descr = int' 
*)
let close_all_fds_except (fds: Unix.file_descr list) =
  (* get at the file descriptor within *)
  let fds' = List.map int_of_file_descr fds in
  let close' (x: int) = 
    try Unix.close(file_descr_of_int x) with _ -> () in

  let highest_to_keep = List.fold_left max (-1) fds' in
  (* close all the fds higher than the one we want to keep *)
  for i = highest_to_keep + 1 to get_max_fd () do close' i done;
  (* close all the rest *)
  for i = 0 to highest_to_keep - 1 do
    if not(List.mem i fds') then close' i
  done


(** Remove "." and ".." from paths (NB doesn't attempt to resolve symlinks) *)
let resolve_dot_and_dotdot (path: string) : string = 
  let of_string (x: string): string list = 
    let rec rev_split path = 
      let basename = Filename.basename path 
      and dirname = Filename.dirname path in
      let rest = if Filename.dirname dirname = dirname then [] else rev_split dirname in
      basename :: rest in
    let abs_path path = 
      if Filename.is_relative path 
      then Filename.concat "/" path (* no notion of a cwd *)
      else path in
    rev_split (abs_path x) in
  
  let to_string (x: string list) = List.fold_left Filename.concat "/" (List.rev x) in
  
  (* Process all "." and ".." references *)
  let rec remove_dots (n: int) (x: string list) = 
    match x, n with
    | [], _ -> []
    | "." :: rest, _ -> remove_dots n rest (* throw away ".", don't count as parent for ".." *)
    | ".." :: rest, _ -> remove_dots (n + 1) rest (* note the number of ".." *)
    | x :: rest, 0 -> x :: (remove_dots 0 rest)
    | x :: rest, n -> remove_dots (n - 1) rest (* munch *) in
  to_string (remove_dots 0 (of_string path))

(** Seek to an absolute offset within a file descriptor *)
let seek_to fd pos =
  Unix.lseek fd pos Unix.SEEK_SET

(** Seek to an offset within a file descriptor, relative to the current cursor position *)
let seek_rel fd diff =
  Unix.lseek fd diff Unix.SEEK_CUR
  
(** Return the current cursor position within a file descriptor *)
let current_cursor_pos fd =
  (* 'seek' to the current position, exploiting the return value from Unix.lseek as the new cursor position *)
  Unix.lseek fd 0 Unix.SEEK_CUR 

module Fdset = struct
	type t
	external of_list : Unix.file_descr list -> t = "stub_fdset_of_list"
	external is_set : t -> Unix.file_descr -> bool = "stub_fdset_is_set"
	external is_set_and_clear : t -> Unix.file_descr -> bool = "stub_fdset_is_set_and_clear"
	external is_empty : t -> bool = "stub_fdset_is_empty"
	external set : t -> Unix.file_descr -> unit = "stub_fdset_set"
	external clear : t -> Unix.file_descr -> unit = "stub_fdset_clear"
	external _select : t -> t -> t -> float -> t * t * t = "stub_fdset_select"
	external _select_ro : t -> float -> t = "stub_fdset_select_ro"
	external _select_wo : t -> float -> t = "stub_fdset_select_wo"
	let select r w e t = _select r w e t
	let select_ro r t = _select_ro r t
	let select_wo w t = _select_wo w t
end

let wait_for_path path delay timeout =
  let rec inner ttl =
    if ttl=0 then failwith "No path!";
    try 
      ignore(Unix.stat path)
    with _ ->
      delay 0.5;
      inner (ttl - 1)
  in
  inner (timeout * 2)
	

let _ = Callback.register_exception "unixext.unix_error" (Unix_error (0))

(* HTTP helpers *)
module Http =
struct
	exception Parse_error
	exception Unknown_file of string
	exception File_already_exists of string

	let http_response_code x =
		match Stringext.String.split ' ' x with
		| _:: code:: _ -> int_of_string code
		| _ -> raise Parse_error

	let rec read_rest_of_headers ic =
		let hdrs = ["content-length"; "cookie"; "connection"; "transfer-encoding"; "authorization"; "location"] in

		let strip_cr r =
			if String.length r = 0 || r.[String.length r - 1] <> '\r' then
				raise Parse_error
			else
				String.sub r 0 ((String.length r)-1) in

		try
			let line = input_line ic in
			let r = strip_cr line in
			if r = "" then
				[]
			else begin
				let hdr = List.find (fun s -> Stringext.String.startswith (s^": ") (String.lowercase r)) hdrs in
				let value = Stringext.String.sub_to_end r (String.length hdr + 2) in
				(hdr,value) :: read_rest_of_headers ic
			end
		with
		| Not_found -> read_rest_of_headers ic    
		| _ -> []

	let rec get ~open_tcp ~uri ~filename ~server =

		(* Check if the filename is valid *)
		if filename <> "" && Sys.file_exists filename then
			raise (File_already_exists filename);
		let fd = 
			if filename = "" then
				Unix.dup Unix.stdout
			else
				Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600 in

		let ic, oc = open_tcp ~server in
		(* Send a GET request to the HTTP server *)
		Printf.fprintf oc "GET %s HTTP/1.0\r\n\r\n" uri;
		flush oc;
		(* Get the result header immediately *)
		let result_line = input_line ic in

		match http_response_code result_line with
		| 200 ->
			(* Copy from channel to the file descriptor *)
			let finished = ref false in
			while not !finished do
				finished := input_line ic = "\r";
			done;

			let buffer = String.make 65536 '\000' in

			let finished = ref false in
			while not(!finished) do
				let num = input ic buffer 0 (String.length buffer) in
				really_write fd buffer 0 num;
				finished := num = 0;
			done;

			Unix.close fd;
			(try close_in ic with _ -> ()); (* Nb. Unix.close_connection only requires the in_channel *)

		| 302 ->
			let headers = read_rest_of_headers ic in
			let new_loc = List.assoc "location" headers in
			(try close_in ic with _ -> ()); (* Nb. Unix.close_connection only requires the in_channel *)
			get ~open_tcp ~uri ~filename ~server:new_loc

		| _ -> failwith "Unhandled response code"

	let rec put ~open_tcp ~uri ~filename ~server =

		(* Check if the filename is valid *)
		if not (Sys.file_exists filename) then
			raise (Unknown_file filename);
		let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in

		let ic, oc = open_tcp ~server in
		(* Send a PUT request to the HTTP server *)
		Printf.fprintf oc "PUT %s HTTP/1.0\r\n\r\n" uri;
		flush oc;
		(* Get the result header immediately *)
		let resultline = input_line ic in

		match http_response_code resultline with
		| 200 -> 
			let oc_fd = Unix.descr_of_out_channel oc in
			let bytes = copy_file fd oc_fd in
			Unix.close fd;
			Unix.shutdown oc_fd Unix.SHUTDOWN_SEND;

		| 302 ->
			let headers = read_rest_of_headers ic in
			let newloc = List.assoc "location" headers in
			put ~open_tcp ~uri ~filename ~server:newloc

		| _ -> failwith "Unhandled response code"
end

let http_get = Http.get
let http_put = Http.put

external send_fd : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> Unix.file_descr -> int = "stub_unix_send_fd_bytecode" "stub_unix_send_fd"
external recv_fd : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr * Unix.file_descr = "stub_unix_recv_fd"
