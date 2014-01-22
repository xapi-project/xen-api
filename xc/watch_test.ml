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
open Watch
open Xenstore

module Tests = struct
  let title ?timeout name =
    Printf.(printf "Waiting%sfor %s: "
      (match timeout with Some t -> sprintf " (timeout %.1f sec) " t | _ -> " ")
      name);
    flush stdout

  let all ?timeout xs =
    title ?timeout "key /test/one to appear";
    Printf.printf "read %s\n" (wait_for ~xs ?timeout (value_to_appear "/test/one"));
    title ?timeout "key /test/one to disappear";
    wait_for ~xs ?timeout (key_to_disappear "/test/one");
    Printf.printf "OK\n";

    title ?timeout "both /test/one and /test/two to appear";
    begin 
      match wait_for ~xs ?timeout (all_of [ value_to_appear "/test/one"; value_to_appear "/test/two" ]) with
      | [ a; b ] ->
	  Printf.printf "values %s and %s\n" a b
      | _ -> failwith "arity mismatch"
    end;
    title ?timeout "either of /test/one or /test/two to disappear";
    begin
      match fst (wait_for ~xs ?timeout (any_of [ `One, key_to_disappear "/test/one"; `Two, key_to_disappear "/test/two" ])) with
      | `One -> Printf.printf "/test/one\n"
      | `Two -> Printf.printf "/test/two\n"
    end
  let go () = 
    with_xs (fun xs -> all ~timeout:5. xs)
end

let _ = Tests.go ()
