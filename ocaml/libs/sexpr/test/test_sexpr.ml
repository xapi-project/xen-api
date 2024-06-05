(*
 * Copyright (C) 2024 Cloud Software Group
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

module Tree = struct
  type 'a t = Node of 'a * 'a t list

  let rec show pv = function
    | Node (l, []) ->
        pv l
    | Node (l, xs) ->
        let xs = String.concat " " List.(map (show pv) xs) in
        Printf.sprintf "(%s %s)" (pv l) xs
end

module Test = struct
  let labels = [|"foo"; "bar"; "baz"|]

  let label_gen =
    QCheck.Gen.(map (Array.get labels) (int_bound (Array.length labels - 1)))

  let tree_gen =
    let open QCheck.Gen in
    let node l xs = Tree.Node (l, xs) in
    let lo, hi = (2, 6) in
    let go recur = function
      | 0 ->
          map2 node label_gen (return [])
      | depth ->
          let xs_gen = list_size (int_range 2 4) (recur (depth - 1)) in
          map2 node label_gen xs_gen
    in
    sized_size (int_range lo hi) (fix go)

  type 'a outcome =
    | Unfinished
    | Finished of 'a
    | Excepted of exn * Printexc.raw_backtrace

  let is_exceptional = function Excepted _ -> true | _ -> false

  let go n =
    Printexc.record_backtrace true ;
    let outcomes = Array.init n (Fun.const Unfinished) in
    let trees = QCheck.Gen.generate ~n tree_gen in
    let parse (i, input) =
      (* Continually parse input until ~200ms has elapsed. *)
      let go () =
        let start = Unix.gettimeofday () in
        while Unix.gettimeofday () -. start < 0.2 do
          ignore (SExpr_TS.of_string input)
        done
      in
      (* Trap any exception and populate outcomes with it. *)
      let open Rresult.R in
      match trap_exn go () with
      | Ok () ->
          outcomes.(i) <- Finished ()
      | Error (`Exn_trap (exn, trace)) ->
          outcomes.(i) <- Excepted (exn, trace)
    in
    let tids =
      let launch (tids, i) tree =
        let tid = Thread.create parse (i, Tree.show Fun.id tree) in
        (tid :: tids, i + 1)
      in
      fst (List.fold_left launch ([], 0) trees)
    in
    List.iter Thread.join tids ;
    match Array.find_opt is_exceptional outcomes with
    | Some (Excepted (_, trace)) ->
        Printexc.print_raw_backtrace Out_channel.stdout trace
    | _ ->
        ()
end

let () = Test.go 10
