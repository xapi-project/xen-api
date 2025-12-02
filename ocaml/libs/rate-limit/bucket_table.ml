(*
 * Copyright (C) 2025 Cloud Software Group
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

type t = (string, Token_bucket.t) Hashtbl.t

let create () = Hashtbl.create 16

let add_bucket table ~user_agent ~burst_size ~fill_rate =
  let bucket_option = Token_bucket.create ~burst_size ~fill_rate in
  match bucket_option with
  | Some bucket ->
      Hashtbl.replace table user_agent bucket ;
      true
  | None ->
      false

let delete_bucket table ~user_agent = Hashtbl.remove table user_agent

let try_consume table ~user_agent amount =
  match Hashtbl.find_opt table user_agent with
  | None ->
      false
  | Some bucket ->
      Token_bucket.consume bucket amount

let peek table ~user_agent =
  Option.map Token_bucket.peek (Hashtbl.find_opt table user_agent)

let consume_and_block table ~user_agent amount =
  match Hashtbl.find_opt table user_agent with
  | None ->
      ()
  | Some bucket ->
      let rec try_consume () =
        if Token_bucket.consume bucket amount then
          ()
        else
          let wait_time = Token_bucket.delay_until_available bucket amount in
          Thread.delay wait_time ; try_consume ()
      in
      try_consume ()
