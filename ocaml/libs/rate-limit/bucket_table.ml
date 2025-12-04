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

type t = {
    table: (string, Token_bucket.t) Hashtbl.t
  ; mutable readers: int
  ; reader_count: Mutex.t (* protects readers count *)
  ; resource: Mutex.t (* held collectively by readers, exclusively by writers *)
}

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let with_read_lock t f =
  with_lock t.reader_count (fun () ->
      t.readers <- t.readers + 1 ;
      if t.readers = 1 then Mutex.lock t.resource
  ) ;
  Fun.protect f ~finally:(fun () ->
      with_lock t.reader_count (fun () ->
          t.readers <- t.readers - 1 ;
          if t.readers = 0 then Mutex.unlock t.resource
      )
  )

let with_write_lock t f = with_lock t.resource f

let create () =
  {
    table= Hashtbl.create 10
  ; readers= 0
  ; reader_count= Mutex.create ()
  ; resource= Mutex.create ()
  }

(* TODO: Indicate failure reason - did we get invalid config or try to add an
   already present user_agent? *)
let add_bucket t ~user_agent ~burst_size ~fill_rate =
  with_write_lock t (fun () ->
      if Hashtbl.mem t.table user_agent then
        false
      else
        match Token_bucket.create ~burst_size ~fill_rate with
        | Some bucket ->
            Hashtbl.add t.table user_agent bucket ;
            true
        | None ->
            false
  )

let delete_bucket t ~user_agent =
  with_write_lock t (fun () -> Hashtbl.remove t.table user_agent)

let try_consume t ~user_agent amount =
  with_read_lock t (fun () ->
      match Hashtbl.find_opt t.table user_agent with
      | None ->
          false
      | Some bucket ->
          Token_bucket.consume bucket amount
  )

let peek t ~user_agent =
  with_read_lock t (fun () ->
      Option.map Token_bucket.peek (Hashtbl.find_opt t.table user_agent)
  )

(* TODO this has fairness issues - fix with queue or similar *)
let consume_and_block t ~user_agent amount =
  let bucket_opt =
    with_read_lock t (fun () -> Hashtbl.find_opt t.table user_agent)
  in
  match bucket_opt with
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
