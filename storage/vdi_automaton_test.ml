(*
 * Copyright Citrix Systems Inc.
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
open Vdi_automaton

(* For any state [s] and operation [o] where [s' = s + o],
   [if s <> s' then s - s' = op] *)

let all_pairs x y =
  List.fold_left (fun acc x -> List.map (fun y -> x, y) y @ acc) [] x

let () =
  List.iter
    (fun (s, op) ->
       try
         let s' = s + op in
         let op' = List.map fst (s - s') in
         if s <> s' && [ op ] <> op' then
           failwith (Printf.sprintf "s = %s; op = %s; s + op = %s; s - (s + op) = %s"
                          (string_of_state s)
                          (string_of_op op)
                          (string_of_state s')
                          (String.concat ", " (List.map string_of_op op')))
       with Bad_transition(_, _) -> ()
    ) (all_pairs every_state every_op);
    Printf.printf "Passed."

