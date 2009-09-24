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
module type PROTOCOL =
  sig
    type t
    val to_string : t -> string
    val of_string : string -> t
  end

module Make_Protocol = functor ( T : sig type t end ) ->
  struct
    type t=T.t
    let to_string (x:t) = Marshal.to_string x [Marshal.Closures]
    let of_string s = (Marshal.from_string s 0 : t)
  end

let rec really_read s string off n =
  if n=0 then () else
    let m = Unix.read s string off n in
    (if m = 0 then raise End_of_file);
    really_read s string (off+m) (n-m)
      
module Com = functor (P : PROTOCOL) ->
  struct 
    let send fd m =
      let mes = P.to_string m in
      let l = (string_of_int (String.length mes)) in
      let buffer = String.make 12 ' ' in
      for i=0 to (String.length l)-1 do buffer.[i] <- l.[i] done;
      ignore(Unix.write fd buffer 0 12);
      ignore(Unix.write fd mes 0 (String.length mes))
      
    let receive fd =
      let buffer = String.make 12 ' '
      in
      try
	ignore (really_read fd buffer 0 12);
	let l=
	  let i=ref 0 in 
	  while(buffer.[!i]<>' ') do incr i done;
	  int_of_string (String.sub buffer 0 !i)
	in
	Printf.fprintf stderr "Length of message: %d\n" l;
	let buffer = String.create l
	in 
	(try
	  ignore (really_read fd buffer 0 l);
	with
	    e -> 
	      Printf.fprintf stderr "Exception: %s\nGot '%s' so far (%d bytes)" (Printexc.to_string e) buffer (String.length buffer);
	      failwith "error");
	P.of_string buffer
      with e -> 
	Printf.fprintf stderr "%s\n" (Printexc.to_string e);
	raise (Failure ("Problem interpreting response: got '"^buffer^"'"))
  end

module Server = functor (P : PROTOCOL) ->
  struct 
    module Com = Com(P)
	
    class virtual ['a] server p np =
      object(s)
	constraint 'a = P.t
	val port_num = p
	val nb_pending = np
	val sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

	method start =
	  Printf.printf "Starting...\n";
	  let sock_addr = Unix.ADDR_INET(Unix.inet_addr_any,port_num) in
	  Unix.bind sock sock_addr;
	  Unix.listen sock nb_pending;
	  while true do
	    let (service_sock, client_sock_addr) = Unix.accept sock
	    in ignore (s#process service_sock)
	  done
	method send = Com.send
	method receive = Com.receive
	method virtual process : Unix.file_descr -> unit
      end
  end

module Client = functor (P : PROTOCOL) ->
  struct
    module Com = Com(P)
	
    let connect addr port =
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
      and in_addr = (Unix.gethostbyname addr).Unix.h_addr_list.(0)
      in Unix.connect sock (Unix.ADDR_INET(in_addr, port));
      sock

    let emit_simple addr port mes =
      let sock = connect addr port
      in Com.send sock mes; 
      Unix.close sock
     
    let emit_answer addr port mes =
      let sock = connect addr port
      in Com.send sock mes;
      let res = Com.receive sock
      in Unix.close sock; res
  end
