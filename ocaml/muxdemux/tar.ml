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

open Stringext
open Unixext

(** Process and create tar file headers *)
module Header = struct
  (** Map of field name -> (start offset, length) taken from wikipedia:
      http://en.wikipedia.org/w/index.php?title=Tar_%28file_format%29&oldid=83554041 *)

  let offset_size_table = [ "file_name", (0, 100);
			    "file_mode", (100, 8);
			    "user_id",   (108, 8);
			    "group_id",  (116, 8);
			    "file_size", (124, 12);
			    "mod_time",  (136, 12);
			    "chksum",    (148, 8);
			    "link",      (156, 1);
			    "link_name", (157, 100); ]
  
  (** Extract the raw string corresponding to field named 'name' *)
  let getfield (x: string) (name: string) = 
    if not(List.mem_assoc name offset_size_table) 
    then failwith (Printf.sprintf "Unknown tar header field: %s" name);
    let start, length = List.assoc name offset_size_table in
    String.sub x start length

  (** Set the raw data corresponding to the field named 'name' *)
  let setfield (x: string) (name: string) (data: string) = 
    if not(List.mem_assoc name offset_size_table) 
    then failwith (Printf.sprintf "Unknown tar header field: %s" name);
    let start, length = List.assoc name offset_size_table in
    if String.length data > length 
    then failwith (Printf.sprintf "Data for field %s too large" name);
    String.blit data 0 x start (String.length data)

  (** Return the size of the field named 'name' *)
  let fieldsize (name: string) = 
    if not(List.mem_assoc name offset_size_table) 
    then failwith (Printf.sprintf "Unknown tar header field: %s" name);
    snd(List.assoc name offset_size_table)

  (** Represents a standard (non-USTAR) archive (note checksum not stored) *)
  type t = { file_name: string;
	     file_mode: int;
	     user_id: int;
	     group_id: int;
	     file_size: int64;
	     mod_time: int64;
	     link: bool;
	     link_name: int;
	   }

  (** Helper function to make a simple header *)
  let make ?(file_mode=0) ?(user_id=0) ?(group_id=0) ?(mod_time=0L) ?(link=false) ?(link_name=0) file_name file_size = 
    { file_name = file_name;
      file_mode = file_mode;
      user_id = user_id;
      group_id = group_id;
      file_size = file_size;
      mod_time = mod_time;
      link = link;
      link_name = link_name }

  (** Length of a header block *)
  let length = 512

  (** A blank header block (two of these in series mark the end of the tar) *)
  let zero_block = String.make length '\000'

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the LHS *)
  let pad_left (x: string) (n: int) (c: char) = 
    if String.length x >= n then x
    else let buffer = String.make n c in
         String.blit x 0 buffer (n - (String.length x)) (String.length x);
         buffer

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the RHS *)
  let pad_right (x: string) (n: int) (c: char) = 
    if String.length x >= n then x
    else let buffer = String.make n c in
         String.blit x 0 buffer 0 (String.length x);
         buffer

  (** Pretty-print the header record *)
  let to_detailed_string (x: t) = 
    let table = [ "file_name", x.file_name;
		  "file_mode", string_of_int x.file_mode;
		  "user_id",   string_of_int x.user_id;
		  "group_id",  string_of_int x.group_id;
		  "file_size", Int64.to_string x.file_size;
		  "mod_time",  Int64.to_string x.mod_time;
		  "link",      string_of_bool x.link;
		  "link_name", string_of_int x.link_name ] in
    "{\n" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

  (** Make a single line summary which looks like the output of tar -tv *)
  let to_summary_string (x: t) = 
    (* -rw-r--r-- *)
    let mode = Printf.sprintf "%010d" x.file_mode in
    (* root/root *)
    let usergroup = Printf.sprintf "%d/%d" x.user_id x.group_id in
    let size = pad_right (Int64.to_string x.file_size) 8 ' ' in
    let time = Unix.gmtime (Int64.to_float x.mod_time) in
    let time = Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" 
      (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
      time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec in
    Printf.sprintf "%s %s %s %s %s" mode usergroup size time x.file_name

  (** For debugging: pretty-print a string as hex *)
  let to_hex (x: string) : string = 
    let chars = List.map (Printf.sprintf "%02x") (List.map int_of_char (String.explode x)) in
    String.concat " " chars

  (** Marshal an integer field of size 'n' *)
  let marshal_int (x: int) (n: int) = 
    let octal = Printf.sprintf "%o" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an int64 field of size 'n' *)
  let marshal_int64 (x: int64) (n: int) = 
    let octal = Printf.sprintf "%Lo" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an string field of size 'n' *)
  let marshal_string (x: string) (n: int) = x

  (** Return the first part of a field, before the predicate is true *)
  let trim (p: char -> bool) (x: string) : string = match String.split_f p x with
    | [] -> ""
    | first::_ -> first

  (** Return the first part of a numerical field, before any spaces or NULLs *)
  let trim_numerical (x: string) : string = trim (fun c -> c = '\000' || c = ' ') x 
  (** Return the first part of a string field, before any NULLs *)
  let trim_string (x: string) : string = trim (fun c -> c = '\000') x

  (** Unmarshal an integer field (stored as 0-padded octal) *)
  let unmarshal_int (x: string) : int = 
    let tmp = "0o0" ^ (trim_numerical x) in
    try
      int_of_string tmp
    with Failure "int_of_string" as e -> 
      Printf.eprintf "Failed to parse integer [%s] == %s\n" tmp (to_hex tmp);
      raise e

  (** Unmarshal an int64 field (stored as 0-padded octal) *)
  let unmarshal_int64 (x: string) : int64 = 
    let tmp = "0o0" ^ (trim_numerical x) in
    Int64.of_string tmp

  (** Unmarshal a string *)
  let unmarshal_string (x: string) : string = trim_string x

  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch

  (** From an already-marshalled block, compute what the checksum should be *)
  let checksum (x: string) : int64 = 
    (* Sum of all the byte values of the header with the checksum field taken
       as 8 ' ' (spaces) *)
    let x' = String.copy x in
    let start, length = List.assoc "chksum" offset_size_table in
    for i = start to start + length - 1 do
      x'.[i] <- ' '
    done;
    List.fold_left Int64.add 0L (List.map (fun x -> Int64.of_int (int_of_char x)) (String.explode x'))

  (** Unmarshal a header block, returning None if it's all zeroes *)
  let unmarshal (x: string) : t option = 
    (* Check if the string is full of zeros *)
    if x = zero_block then None
    else 
      let chksum = unmarshal_int64  (getfield x "chksum") in
      if checksum x <> chksum then raise Checksum_mismatch
      else Some { file_name = unmarshal_string (getfield x "file_name");
		  file_mode = unmarshal_int    (getfield x "file_mode");
		  user_id   = unmarshal_int    (getfield x "user_id");
		  group_id  = unmarshal_int    (getfield x "group_id");
		  file_size = unmarshal_int64  (getfield x "file_size");
		  mod_time  = unmarshal_int64  (getfield x "mod_time");
		  link      = getfield x "link" = "1";
		  link_name = unmarshal_int    (getfield x "link_name");
		}

  (** Marshal a header block, computing and inserting the checksum *)
  let marshal (x: t) : string = 
    let buffer = String.make length '\000' in
    setfield buffer "file_name" x.file_name;
    setfield buffer "file_mode" (marshal_int x.file_mode (fieldsize "file_mode"));
    setfield buffer "user_id"   (marshal_int x.user_id (fieldsize "user_id"));
    setfield buffer "group_id"  (marshal_int x.group_id (fieldsize "group_id"));
    setfield buffer "file_size" (marshal_int64 x.file_size (fieldsize "file_size"));    
    setfield buffer "mod_time"  (marshal_int64 x.mod_time (fieldsize "mod_time"));  
    (* leave out link, link_name (zero-filled = unused) *)
    (* Finally, compute the checksum *)
    let chksum = checksum buffer in
    setfield buffer "chksum"    (marshal_int64 chksum (fieldsize "chksum"));
    buffer

  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream

  (** Returns the next header block or throws End_of_stream if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block. Unix.End_of_file is thrown if the stream
      unexpectedly fails *)
  let get_next_header (ifd: Unix.file_descr) : t = 
    let next () = 
      let buffer = String.make length '\000' in
      really_read ifd buffer 0 length;
      unmarshal buffer 
    in
    match next () with
      | Some x -> x
      | None -> 
	  begin match next () with
	    | Some x -> x
	    | None -> raise End_of_stream
	  end
	    
  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  let compute_zero_padding_length (x: t) : int = 
    (* round up to next whole number of block lengths *)
    let length = Int64.of_int length in
    let lenm1 = Int64.sub length Int64.one in
    let next_block_length = (Int64.mul length (Int64.div (Int64.add x.file_size lenm1) length)) in
    Int64.to_int (Int64.sub next_block_length x.file_size)

  (** Return the required zero-padding as a string *)
  let zero_padding (x: t) : string = 
    let zero_padding_len = compute_zero_padding_length x in
    String.make zero_padding_len '\000' 

  (** Return the header needed for a particular file on disk *)
  let of_file (file: string) : t =
    let stat = Unix.stat file in
    let size = Int64.of_int stat.Unix.st_size in
    { file_name   = file;
      file_mode   = stat.Unix.st_perm;
      user_id     = stat.Unix.st_uid;
      group_id    = stat.Unix.st_gid;
      file_size   = size;
      mod_time    = Int64.of_float stat.Unix.st_mtime;
      link        = false;
      link_name   = 0 }
end


let write_string fd str = 
  let written = Unix.write fd str 0 (String.length str) in
  if str <> "" && String.length str > written then failwith "Truncated write"

let write_bigbuffer fd buf =
	Bigbuffer.to_fct buf (write_string fd)

let write_block (header: Header.t) (body: Unix.file_descr -> unit) (fd : Unix.file_descr) = 
  write_string fd (Header.marshal header);
  body fd;
  write_string fd (Header.zero_padding header)

let write_end (fd: Unix.file_descr) =
  write_string fd Header.zero_block;
  write_string fd Header.zero_block

(** Utility functions for operating over whole tar archives *)
module Archive = struct

  (** Skip 'n' bytes from input channel 'ifd' *)
  let skip (ifd: Unix.file_descr) (n: int) = 
    let buffer = String.make 4096 '\000' in
    let rec loop (n: int) = 
      if n <= 0 then ()
      else 
	let amount = min n (String.length buffer) in
	let m = Unix.read ifd buffer 0 amount in
	if m = 0 then raise End_of_file;
	loop (n - m) in
    loop n

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  let with_next_file (fd: Unix.file_descr) (f: Unix.file_descr -> Header.t -> 'a) = 
    let hdr = Header.get_next_header fd in
    (* NB if the function 'f' fails we're boned *)
    Pervasiveext.finally (fun () -> f fd hdr) 
      (fun () -> skip fd (Header.compute_zero_padding_length hdr))


  (** Multicast 'n' bytes from input fd 'ifd' to output fds 'ofds'. NB if one deadlocks
      they all stop.*)
  let multicast_n ?(buffer_size=1024*1024) (ifd: Unix.file_descr) (ofds: Unix.file_descr list) (n: int64) = 
    let buffer = String.make buffer_size '\000' in
    let rec loop (n: int64) = 
      if n <= 0L then ()
      else 
	let amount = Int64.to_int (min n (Int64.of_int(String.length buffer))) in
	let read = Unix.read ifd buffer 0 amount in
	if read = 0 then raise End_of_file;
	List.iter (fun ofd -> ignore(Unix.write ofd buffer 0 read)) ofds;
	loop (Int64.sub n (Int64.of_int read)) in
    loop n

  let multicast_n_string buffer ofds n =
    List.iter (fun ofd -> ignore(Unix.write ofd buffer 0 n)) ofds

  (** Copy 'n' bytes from input fd 'ifd' to output fd 'ofd' *)
  let copy_n ifd ofd n = multicast_n ifd [ ofd ] n

  (** List the contents of a tar to stdout *)
  let list fd = 
      try
	while true do
	  let hdr = Header.get_next_header fd in
	  print_endline (Header.to_summary_string hdr);
	  skip fd (Int64.to_int hdr.Header.file_size);
	  skip fd (Header.compute_zero_padding_length hdr)
	done
      with 
      | End_of_file ->
	print_endline "Unexpected end of file while reading stream"
      | Header.End_of_stream -> ()

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd = 
      try
	while true do
	  let hdr = Header.get_next_header ifd in
	  let filename = dest ^ "/" ^ hdr.Header.file_name in
	  print_endline filename;
	  let ofd = Unix.openfile filename [Unix.O_WRONLY] 0644 in
	  copy_n ifd ofd hdr.Header.file_size;
	  skip ifd (Header.compute_zero_padding_length hdr)
	done
      with 
      | End_of_file ->
	print_endline "Unexpected end of file while reading stream"
      | Header.End_of_stream -> ()

  (** Create a tar on file descriptor fd from the filename list 'files' *)
  let create files ofd = 
    let file filename = 
      let stat = Unix.stat filename in
      if stat.Unix.st_kind <> Unix.S_REG 
      then Printf.eprintf "Skipping %s: not a regular file\n" filename
      else 
	let hdr = Header.of_file filename in
	write_block hdr (fun ofd ->
			   let ifd = Unix.openfile filename [Unix.O_RDONLY] 0644 in
			   copy_n ifd ofd hdr.Header.file_size) ofd;
    in
    List.iter file files;
    (* Add two empty blocks *)
    write_end ofd

  
end



