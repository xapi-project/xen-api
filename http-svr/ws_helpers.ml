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

type protocol = | Hixie76 | Hybi10

(* Defined in the websockets protocol document *)
let ws_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" 

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
    "Sec-WebSocket-Protocol: binary"; "" ]

exception MissingHeader of string


let find_header headers header_name =
  try 
    List.assoc header_name headers
  with _ ->
    raise (MissingHeader header_name)

let extract_numbers str =
  let num = Astring.String.filter (function '0'..'9' -> true | _ -> false) str in
  Int64.of_string num

let count_spaces str =
  Astring.String.fold_left (fun acc -> function ' ' -> acc + 1 | _ -> acc) 0 str

let marshal_int32 x = 
  let offsets = [|3;2;1;0|] in
  let (>!>) a b = Int32.shift_right_logical a b
  and (&&) a b = Int32.logand a b in
  let a = (x >!> 0) && 0xffl 
  and b = (x >!> 8) && 0xffl
  and c = (x >!> 16) && 0xffl
  and d = (x >!> 24) && 0xffl in
  let s = Bytes.make 4 '\000' in
  Bytes.set s offsets.(0) @@ char_of_int (Int32.to_int a);
  Bytes.set s offsets.(1) @@ char_of_int (Int32.to_int b);
  Bytes.set s offsets.(2) @@ char_of_int (Int32.to_int c);
  Bytes.set s offsets.(3) @@ char_of_int (Int32.to_int d);
  Bytes.unsafe_to_string s

let v10_upgrade req s =
  let headers = req.Http.Request.additional_headers in 
  let key = find_header headers "sec-websocket-key" in
  (*let vsn = find_header headers "sec-websocket-version" in*)
  let result = key ^ ws_uuid |> Sha1.string |> Sha1.to_bin in
  let key = Base64.encode_string result in
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
  let s3 = Bytes.make 8 '\000' in
  Xapi_stdext_unix.Unixext.really_read s s3 0 8;
  let string = Printf.sprintf "%s%s%s" s1 s2 (Bytes.unsafe_to_string s3) in
  let digest = Digest.string string in

  let host = find_header headers "host" in
  let origin = find_header headers "origin" in
  let protocol = try Some (find_header headers "sec-websocket-protocol") with _ -> None in
  let real_uri = req.Http.Request.uri ^ "?" ^ (String.concat "&" (List.map (fun (x,y) -> Printf.sprintf "%s=%s" x y) req.Http.Request.query)) in
  let headers = http_101_websocket_upgrade_76 origin host protocol real_uri in
  Http.output_http s headers;
  Unix.write s (Bytes.unsafe_of_string digest) 0 16 
  |> ignore

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
