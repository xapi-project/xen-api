(*
 * Copyright (C) 2017 David Scott <dave@recoil.org>
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
open Qcow_types

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  mutable locks: (Qcow_rwlock.t * int) Cluster.Map.t;
  metadata_m: Lwt_mutex.t;
  (** held during metadata changing operations *)
}

module Client = Qcow_rwlock.Client

let make () =
  let locks = Cluster.Map.empty in
  let metadata_m = Lwt_mutex.create () in
  { locks; metadata_m  }

let with_metadata_lock t = Lwt_mutex.with_lock t.metadata_m

let get_lock t cluster =
  let lock, refcount =
    if Cluster.Map.mem cluster t.locks
    then Cluster.Map.find cluster t.locks
    else begin
      Qcow_rwlock.make (fun () -> Printf.sprintf "cluster %s" (Cluster.to_string cluster)), 0
    end in
  t.locks <- Cluster.Map.add cluster (lock, refcount + 1) t.locks;
  lock

let put_lock t cluster =
  (* put_lock is always called after get_lock *)
  assert (Cluster.Map.mem cluster t.locks);
  let lock, refcount = Cluster.Map.find cluster t.locks in
  t.locks <-
    if refcount = 1
    then Cluster.Map.remove cluster t.locks
    else Cluster.Map.add cluster (lock, refcount - 1) t.locks

let with_rwlock t cluster f =
  let lock = get_lock t cluster in
  Lwt.finalize (fun () -> f lock) (fun () -> put_lock t cluster; Lwt.return_unit)

type lock = {
  lock: Qcow_rwlock.lock;
  t: t;
  cluster: Cluster.t;
}

let unlock lock =
  Qcow_rwlock.unlock lock.lock;
  put_lock lock.t lock.cluster

module Read = struct
  let with_lock ?client t cluster f =
    with_rwlock t cluster
      (fun rw ->
        Qcow_rwlock.Read.with_lock ?client rw f
      )

  let with_locks ?client t ~first ~last f =
    let rec loop n =
      if n > last
      then f ()
      else
        with_rwlock t n
          (fun rw ->
            Qcow_rwlock.Read.with_lock ?client rw
              (fun () -> loop (Cluster.succ n))
          ) in
    loop first

  let lock ?client t cluster =
    let lock = get_lock t cluster in
    let open Lwt.Infix in
    Qcow_rwlock.Read.lock ?client lock
    >>= fun lock ->
    Lwt.return { lock; t; cluster }
end

module Write = struct
  let with_lock ?client t cluster f =
    with_rwlock t cluster
      (fun rw ->
        Qcow_rwlock.Write.with_lock ?client rw f
      )

  let with_locks ?client t ~first ~last f =
    let rec loop n =
      if n > last
      then f ()
      else
        with_rwlock t n
          (fun rw ->
            Qcow_rwlock.Write.with_lock ?client rw
              (fun () -> loop (Cluster.succ n))
          ) in
    loop first

  let try_lock ?client t cluster =
    let lock = get_lock t cluster in
    match Qcow_rwlock.Write.try_lock ?client lock with
    | None ->
      put_lock t cluster;
      None
    | Some lock ->
      let lock = { lock; t; cluster } in
      Some lock
end

module Debug = struct
  include Qcow_rwlock.Debug

  let dump_state t =
    let locks = List.map fst @@ List.map snd @@ Cluster.Map.bindings t.locks in
    Log.info (fun f -> f "%s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ Qcow_rwlock.sexp_of_ts locks))
end
