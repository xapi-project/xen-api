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

(* Portions of this code are distributed under the following copyright notice *) 
(*----------------------------------------------------------------------------
   Copyright (c) 2009-11, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Uuidm version 0.9.3
  ----------------------------------------------------------------------------*)

(* Websockets helper functions *)

(* A couple of short helper functions for upgrading an HTTP 
 * connection to a websockets connection 
 * See for reference:
 * http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17 
 *)

open Stringext

type protocol = | Hixie76 | Hybi10

(* Defined in the websockets protocol document *)
let ws_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" 

(* The following sha_1 code is taken from Daniel C. Bünzli 'uuidm' library *)
let sha_1 s =                            
  let sha_1_pad s = 
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = String.create mlen in 
    String.blit s 0 m 0 len;
    String.fill m len (mlen - len) '\x00';
    m.[len] <- '\x80';
    if Sys.word_size > 32 then begin
      m.[mlen - 8] <- Char.unsafe_chr (blen lsr 56 land 0xFF);
      m.[mlen - 7] <- Char.unsafe_chr (blen lsr 48 land 0xFF);
      m.[mlen - 6] <- Char.unsafe_chr (blen lsr 40 land 0xFF);
      m.[mlen - 5] <- Char.unsafe_chr (blen lsr 32 land 0xFF);
    end;
    m.[mlen - 4] <- Char.unsafe_chr (blen lsr 24 land 0xFF);
    m.[mlen - 3] <- Char.unsafe_chr (blen lsr 16 land 0xFF);
    m.[mlen - 2] <- Char.unsafe_chr (blen lsr 8 land 0xFF);
    m.[mlen - 1] <- Char.unsafe_chr (blen land 0xFF);
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to ((String.length m) / 64) - 1 do             (* For each block *) 
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do 
      let k = base + (j * 4) in
      w.(j) <- sl (Int32.of_int (Char.code m.[k])) 24 lor
               sl (Int32.of_int (Char.code m.[k + 1])) 16 lor
               sl (Int32.of_int (Char.code m.[k + 2])) 8 lor
               (Int32.of_int (Char.code m.[k + 3]))
    done;
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    for t = 0 to 79 do 
      let f, k = 
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
        if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
        if t <= 59 then 
	  (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl 
	else
        !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
	  w.(s) <- cls 1 begin 
	    w.((s + 13) &&& 0xF) lxor 
	    w.((s + 8) &&& 0xF) lxor 
	    w.((s + 2) &&& 0xF) lxor
	    w.(s)
	  end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = String.create 20 in
  let i2s h k i =
    h.[k] <- Char.unsafe_chr ((Int32.to_int (sr i 24)) &&& 0xFF);
    h.[k + 1] <- Char.unsafe_chr ((Int32.to_int (sr i 16)) &&& 0xFF);
    h.[k + 2] <- Char.unsafe_chr ((Int32.to_int (sr i 8)) &&& 0xFF);
    h.[k + 3] <- Char.unsafe_chr ((Int32.to_int i) &&& 0xFF);
  in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  h


let http_101_websocket_upgrade_76 origin host protocol uri =
  let extra = match protocol with 
    | Some x -> [ Printf.sprintf "Sec-WebSocket-Protocol: %s" x ]
    | None -> []
  in
  [ "HTTP/1.0 101 WebSocket Protocol Handshake";
    "Upgrade: WebSocket";
    "Connection: Upgrade";
    Printf.sprintf "Sec-WebSocket-Origin: %s" origin;
    Printf.sprintf "Sec-WebSocket-Location: ws://%s%s" host uri; ] 
    @ extra @ [""]


let http_101_websocket_upgrade_15 key  =
  [ "HTTP/1.0 101 Switching Protocols";
    "Upgrade: websocket";
    "Connection: Upgrade";
    Printf.sprintf "Sec-WebSocket-Accept: %s" key;
    "Sec-WebSocket-Protocol: chat"; "" ]

exception MissingHeader of string

let end_of_string s from =
  String.sub s from ((String.length s)-from) 

let find_header headers header_name =
  try 
    List.assoc header_name headers
  with _ ->
    raise (MissingHeader header_name)

let extract_numbers str = 
  let exploded = String.explode str in
  let nums = List.filter (function '0'..'9' -> true | _ -> false) exploded in
  let num = String.implode nums in
  Int64.of_string num

let count_spaces str =
  let exploded = String.explode str in
  let spaces = List.filter (function ' ' -> true | _ -> false) exploded in
  List.length spaces

let marshal_int32 x = 
  let offsets = [|3;2;1;0|] in
  let (>!>) a b = Int32.shift_right_logical a b
  and (&&) a b = Int32.logand a b in
  let a = (x >!> 0) && 0xffl 
  and b = (x >!> 8) && 0xffl
  and c = (x >!> 16) && 0xffl
  and d = (x >!> 24) && 0xffl in
  let s=String.make 4 '\000' in
  s.[offsets.(0)] <- char_of_int (Int32.to_int a);
  s.[offsets.(1)] <- char_of_int (Int32.to_int b);
  s.[offsets.(2)] <- char_of_int (Int32.to_int c);
  s.[offsets.(3)] <- char_of_int (Int32.to_int d);
  s

let v10_upgrade req s =
  let headers = req.Http.Request.additional_headers in 
  let key = find_header headers "sec-websocket-key" in
  (*let vsn = find_header headers "sec-websocket-version" in*)
  let result = sha_1 (key ^ ws_uuid) in
  let key = Base64.encode result in 
  let headers = http_101_websocket_upgrade_15 key in
  Http.output_http s headers

let hixie_v76_upgrade req s = 
  let headers = req.Http.Request.additional_headers in 
  let sec_websocket_key1 = find_header headers "sec-websocket-key1" in
  let sec_websocket_key2 = find_header headers "sec-websocket-key2" in
  let n1 = extract_numbers sec_websocket_key1 in
  let n2 = extract_numbers sec_websocket_key2 in
  let s1 = count_spaces sec_websocket_key1 in
  let s2 = count_spaces sec_websocket_key2 in
  let v1 = Int64.to_int32 (Int64.div n1 (Int64.of_int s1)) in
  let v2 = Int64.to_int32 (Int64.div n2 (Int64.of_int s2)) in
  
  let s1 = marshal_int32 v1 in
  let s2 = marshal_int32 v2 in
  let s3 = String.make 8 '\000' in
  Unixext.really_read s s3 0 8;
  let string = Printf.sprintf "%s%s%s" s1 s2 s3 in
  let digest = Digest.string string in
  
  let host = find_header headers "host" in
  let origin = find_header headers "origin" in
  let protocol = try Some (find_header headers "sec-websocket-protocol") with _ -> None in
  let real_uri = req.Http.Request.uri ^ "?" ^ (String.concat "&" (List.map (fun (x,y) -> Printf.sprintf "%s=%s" x y) req.Http.Request.query)) in
  let headers = http_101_websocket_upgrade_76 origin host protocol real_uri in
  Http.output_http s headers;
  ignore(Unix.write s digest 0 16)

let upgrade req s = 
  if List.mem_assoc "sec-websocket-key1" req.Http.Request.additional_headers 
  then (hixie_v76_upgrade req s; Hixie76)
  else (v10_upgrade req s; Hybi10)

(* The following copyright notice is relevant to the function marked above *)

(*----------------------------------------------------------------------------
  Copyright (c) 2009-11, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
