(*
 * Copyright (C) Citrix Systems Inc.
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

(* Websockets protocol here *)

module Wsprotocol (IO : Iteratees.Monad) = struct

  module I = Iteratees.Iteratee(IO)
  open I

  type 'a t = 'a I.t 

  let base64encode s = modify Base64.encode s
  let base64decode s = modify Base64.decode s
    
  let writer = I.writer

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

  let wsframe_old s = modify (fun s -> 
    Printf.printf "frame: got %s\n" s; Printf.sprintf "\x00%s\xff" s) s
    
  let rec wsunframe x = 
    let read_sz =
      read_int8 >>= fun sz ->
      return (sz >= 128, sz land 0x7f)
    in
    let rec read_size sz = 
      if sz < 126 
      then return sz
      else if sz = 126 then
	read_int16
      else (* sz = 127 *)
	read_int32 >>= fun x -> return (Int32.to_int x)
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
	    if op land 0x0f = 0x08 
	    then (* close frame *)
	      return s
	    else 
	      if not (op land 0x80 = 0x80)
	      then begin
		inner (acc ^ real_str) s 
	      end else begin
		liftI (IO.bind (k (Iteratees.Chunk (acc ^ real_str))) (fun (i, _) ->
		  IO.return (wsunframe i)))
	      end
	  end	
	| _ -> return s
    in inner "" x

  let rec wsunframe_old s =
    match s with 
      | IE_cont (None, k) ->
	begin    
	  heads "\x00" >>= fun n ->
      break ((=) '\xff') >>= fun str -> 
	  drop 1 >>= fun () -> 
	  liftI (IO.bind (k (Iteratees.Chunk str)) (fun (i,_) ->
	    IO.return (wsunframe_old i)))
	end
      | _ -> return s

end  

module TestWsIteratee = Wsprotocol(Test.StringMonad)

let test1 = "\x81\x05\x48\x65\x6c\x6c\x6f"
let test2 = "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58"
let test3 = "\x01\x03\x48\x65\x6c"
let test4 = "\x80\x02\x6c\x6f"
let test5 = test1 ^ "\x88\x00"

let testold1 = "\x00Hello\xff\x00There\xff"

let runtest () = 
  let open TestWsIteratee in
  let open I in 

  let it = wsunframe (writer Test.StringMonad.strwr "foo") in
  let itold = wsunframe_old (writer Test.StringMonad.strwr "bar") in

  let ($) f x = f x in
  let (>>=) x f = Test.StringMonad.bind x f in
  let (=<<) f x = Test.StringMonad.bind x f in

  let dump x = 
    let str = Test.StringMonad.getstr x in
    let data = Test.StringMonad.getdata x in
    Printf.printf "str='%s' state=%s\n" str (state data)
  in

  let x = enum_nchunk test1 3 $ it in dump x;
  let x = enum_nchunk test2 3 $ it in dump x;
  let mytest3 = enum_nchunk test3 3 $ it in dump mytest3;
  let x = mytest3 >>= (enum_nchunk test4 3) in dump x;
  let x = enum_eof =<< (enum_nchunk test5 3 it) in dump x;
  let x = enum_eof =<< (enum_nchunk test3 3 it) in dump x;
  Printf.printf "old style:\n";
  let x = enum_nchunk testold1 3 $ itold in dump x
						
