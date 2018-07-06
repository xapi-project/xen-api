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

type t =
  | Foo of int
  | Bar of (int * float)
  [@@deriving rpc]

module M = struct
  type m = t [@@deriving rpc]
end

type 'a x = {
  foo: M.m;
  bar: string;
  gna: float list;
  f1: (int option * bool list * float list list) option;
  f2: (string * string list) array;
  f3: int32;
  f4: int64;
  f5: int [@key "type"];
  f6: (unit * char) list;
  f7: 'a list [@key "let"];
  progress: int array;
} [@@deriving rpc]

module Testable = struct
  let x = Testable.from_rpc_of_t (rpc_of_x M.rpc_of_m)
  let call = Testable.from_to_string Rpc.string_of_call
  let response = Testable.from_to_string Rpc.string_of_response
end

let run () =
  let x = {
    foo= Foo 3;
    bar= "ha          ha";
    gna=[1.; 2.; 3.; 4.; Unix.gettimeofday () ];
    f2 = [| "hi",["hi"]; "hou",["hou";"hou"]; "foo", ["b";"a";"r"] |];
    f1 = Some (None, [true], [[1.]; [2.;3.]]);
    f3 = Int32.max_int;
    f4 = Int64.max_int;
    f5 = max_int;
    f6 = [ (),'a' ; (),'b' ; (),'c'; (),'d' ; (),'e' ];
    f7 = [ Foo 1; Foo 2; Foo 3 ];
    progress = [| 0; 1; 2; 3; 4; 5 |];
  } in

  (* Testing basic marshalling/unmarshalling *)

  let rpc = rpc_of_x M.rpc_of_m x in

  let rpc_xml = Xmlrpc.to_string rpc in
  let rpc_json = Jsonrpc.to_string rpc in

  Printf.printf "\n==rpc_xml==\n%s\n" rpc_xml;
  Printf.printf "\n==json==\n%s\n" rpc_json;

  let callback fields value =
    match (fields, value) with
    | ["progress"], Rpc.Int i -> Printf.printf "Progress: %Ld\n" i
    | _                       -> () in

  let x_of_rpc = x_of_rpc M.m_of_rpc in

  let x_xml = x_of_rpc (Xmlrpc.of_string ~callback rpc_xml) in
  let x_json = x_of_rpc (Jsonrpc.of_string rpc_json) in

  Alcotest.(check Testable.x) "Sanity check x=x_xml" x x_xml;
  Alcotest.(check Testable.x) "Sanity check x=x_json" x x_json;

  (* Testing calls and responses *)

  let call = Rpc.call "foo" [ rpc ] in
  let success = Rpc.success rpc in
  let failure = Rpc.failure rpc in

  let c_xml_str = Xmlrpc.string_of_call call in
  let s_xml_str = Xmlrpc.string_of_response success in
  let f_xml_str = Xmlrpc.string_of_response failure in

  let c_json_str = Jsonrpc.string_of_call call in
  let s_json_str = Jsonrpc.string_of_response success in
  let f_json_str = Jsonrpc.string_of_response failure in

  Printf.printf "\n==call==\n %s\n%s\n" c_xml_str c_json_str;
  Printf.printf "\n==success==\n %s\n%s\n" s_xml_str s_json_str;
  Printf.printf "\n==failure==\n %s\n%s\n" f_xml_str f_json_str;

  let c_xml = Xmlrpc.call_of_string c_xml_str in
  let s_xml = Xmlrpc.response_of_string s_xml_str in
  let f_xml = Xmlrpc.response_of_string f_xml_str in

  let c1 = x_of_rpc (List.hd call.Rpc.params) in
  let c2 = x_of_rpc (List.hd c_xml.Rpc.params) in
  let s1 = x_of_rpc success.Rpc.contents in
  let s2 = x_of_rpc s_xml.Rpc.contents in
  let f1 = x_of_rpc failure.Rpc.contents in
  let f2 = x_of_rpc f_xml.Rpc.contents in

  Alcotest.(check Testable.x) "Sanity check c1=c2" c1 c2;
  Alcotest.(check Testable.x) "Sanity check s1=s2" s1 s2;
  Alcotest.(check Testable.x) "Sanity check f1=f2" f1 f2;

  let c_json = Jsonrpc.call_of_string c_json_str in
  let s_json = Jsonrpc.response_of_string s_json_str in
  let f_json = Jsonrpc.response_of_string f_json_str in

  Alcotest.(check Testable.call) "Sanity check call=c_json" call c_json;
  Alcotest.(check Testable.response) "Sanity check success=s_json" success s_json;
  Alcotest.(check Testable.response) "Sanity check failure=f_json" failure f_json

let tests =
  [ "test", `Quick, run ]
