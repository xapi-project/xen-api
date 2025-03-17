(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Sexplib.Std

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

(* A resource that can be locked *)
type t = {
  t_description_fn: unit -> string;
  m: Lwt_mutex.t;
  c: unit Lwt_condition.t;
  mutable all_locks: lock list;
}
(* A lock held on a resource *)
and lock = {
  t: t;
  client: client;
  mutable reader: bool; (* or writer *)
  mutable released: bool;
}
(* A client owning the lock *)
and client = {
  client_description_fn: unit -> string;
  mutable my_locks: lock list;
}

type ts = t list

let make t_description_fn =
  let m = Lwt_mutex.create () in
  let c = Lwt_condition.create () in
  let all_locks = [] in
  { t_description_fn; m; c; all_locks }

module To_sexp = struct
  (* Project instances of type t into a simpler set of records, organised for
     printing. *)
  module Lock = struct
    type t = {
      description: string;
      mode: [ `Read | `Write ];
      released: bool;
    } [@@deriving sexp_of]
  end
  module Client = struct
    type t = {
      description: string;
      locks: Lock.t list;
    } [@@deriving sexp_of]
  end
  type t = {
    description: string;
    clients: Client.t list;
  } [@@deriving sexp_of]
  let rec setify eq = function
    | [] -> []
    | x :: xs -> if List.filter (fun y -> eq x y) xs <> [] then setify eq xs else x :: (setify eq xs)
  let lock l =
    let description = l.t.t_description_fn () in
    let mode = if l.reader then `Read else `Write in
    let released = l.released in
    { Lock.description; mode; released }
  let client c =
    let description = c.client_description_fn () in
    (* Make the per-client list easier to read by de-duplicating it *)
    let locks = setify ( = ) @@ List.map lock c.my_locks in
    { Client.description; locks }
  let t t =
    let description = t.t_description_fn () in
    let clients = List.map client @@ setify ( == ) @@ List.map (fun l -> l.client) t.all_locks in
    { description; clients }
  type ts = Client.t list [@@deriving sexp_of]
  let ts ts =
    let all_locks = List.concat @@ List.map (fun t -> t.all_locks) ts in
    List.map client @@ setify ( == ) @@ List.map (fun l -> l.client) all_locks
end
let sexp_of_t x = To_sexp.(sexp_of_t @@ t x)
let sexp_of_ts xs = To_sexp.(sexp_of_ts @@ ts xs)
let sexp_of_client x = To_sexp.(Client.sexp_of_t @@ client x)

let anon_client =
  let next_idx = ref 0 in
  fun () ->
    let idx = !next_idx in
    incr next_idx;
    let client_description_fn () = Printf.sprintf "Anonymous client %d" idx in
    let my_locks = [] in
    { client_description_fn; my_locks }

let unlock lock =
  assert(not lock.released);
  lock.released <- true;
  lock.client.my_locks <- List.filter (fun l -> l != lock) lock.client.my_locks;
  lock.t.all_locks <- List.filter (fun l -> l != lock) lock.t.all_locks;
  Lwt_condition.broadcast lock.t.c ()

let any f xs = List.fold_left (fun acc x -> acc || (f x)) false xs

module Read = struct
  let lock ?(client = anon_client ()) t =
    let open Lwt.Infix in
    Lwt_mutex.with_lock t.m
      (fun () ->
        let rec wait () =
          (* If any other client has a write lock then wait *)
          let any_other_writer = any (fun l -> l.client != client && (not l.reader)) t.all_locks in
          if any_other_writer then begin
            Lwt_condition.wait t.c ~mutex:t.m
            >>= fun () ->
            wait ()
          end else begin
            let reader = true and released = false in
            let lock = { t; client; reader; released } in
            t.all_locks <- lock :: t.all_locks;
            client.my_locks <- lock :: client.my_locks;
            Lwt.return lock
          end in
        wait ()
      )

  let with_lock ?(client = anon_client ()) t f =
    let open Lwt.Infix in
    lock ~client t
    >>= fun lock ->
    Lwt.finalize f
      (fun () ->
        unlock lock;
        Lwt.return_unit
      )
end

module Write = struct

  let any_other_client t client =
    any (fun l -> l.client != client) t.all_locks

  let with_lock ?(client = anon_client ()) t f =
    let open Lwt.Infix in
    Lwt_mutex.with_lock t.m
      (fun () ->
        let rec wait () =
          (* If any other client has a lock then wait *)
          if any_other_client t client then begin
            Lwt_condition.wait t.c ~mutex:t.m
            >>= fun () ->
            wait ()
          end else begin
            let reader = false and released = false in
            let lock = { t; client; reader; released } in
            t.all_locks <- lock :: t.all_locks;
            client.my_locks <- lock :: client.my_locks;
            Lwt.return lock
          end in
        wait ()
      )
    >>= fun lock ->
    Lwt.finalize f
      (fun () ->
        unlock lock;
        Lwt.return_unit
      )

  let try_lock ?(client = anon_client ()) t =
    if any_other_client t client
    then None
    else begin
      let reader = false and released = false in
      let lock = { t; client; reader; released } in
      t.all_locks <- lock :: t.all_locks;
      client.my_locks <- lock :: client.my_locks;
      Some lock
    end
end

module Client = struct
  type t = client

  let make client_description_fn =
    let my_locks = [] in
    { client_description_fn; my_locks }
end

module Debug = struct
  let assert_no_locks_held client =
    if client.my_locks <> [] then begin
      Printf.fprintf stderr "Client still holds locks:\n%s\n%!" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_client client);
      assert false
    end
end
