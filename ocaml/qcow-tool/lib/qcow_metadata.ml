(*
 * Copyright (C) 2017 Docker Inc
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

(** An in-memory cache of metadata clusters used to speed up lookups.

    Cache entries may be `read` or `update`d with a lock held to block
    concurrent access.
*)

open Qcow_types

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)


module Lwt_error = Qcow_error.Lwt_error
module Lwt_write_error = Qcow_error.Lwt_write_error
module Cache = Qcow_cache
module Locks = Qcow_locks

type error = [ Mirage_block.error | `Msg of string ]
type write_error = [ Mirage_block.write_error | `Msg of string ]

type t = {
  cache: Cache.t;
  locks: Locks.t;
  mutable cluster_map: Qcow_cluster_map.t option; (* free/ used space map *)
  cluster_bits: int;
  m: Lwt_mutex.t;
  c: unit Lwt_condition.t;
}

type contents = {
  t: t;
  data: Cstruct.t;
  cluster: Cluster.t;
}

module Refcounts = struct
  type t = contents
  let of_contents x = x
  let get t n = Cstruct.BE.get_uint16 t.data (2 * n)
  let set t n v = Cstruct.BE.set_uint16 t.data (2 * n) v
end

module Physical = struct
  type t = contents
  let of_contents x = x
  let get t n = Qcow_physical.read (Cstruct.shift t.data (8 * n))
  let set t n v =
    begin match t.t.cluster_map with
      | Some m ->
        (* Find the block currently being referenced so it can be marked
           as free. *)
        let existing = Qcow_physical.read (Cstruct.shift t.data (8 * n)) in
        let cluster = Qcow_physical.cluster ~cluster_bits:t.t.cluster_bits existing in
        let v' = Qcow_physical.cluster ~cluster_bits:t.t.cluster_bits v in
        Log.debug (fun f -> f "Physical.set %s:%d -> %s%s" (Cluster.to_string t.cluster) n
                      (if v = Qcow_physical.unmapped then "unmapped" else Cluster.to_string v')
                      (if cluster <> Cluster.zero then ", unmapping " ^ (Cluster.to_string cluster) else "")
                  );
        if cluster <> Cluster.zero then begin
          Qcow_cluster_map.remove m cluster;
        end;
        Qcow_cluster_map.add m (t.cluster, n) v'
      | None -> ()
    end;
    Qcow_physical.write v (Cstruct.shift t.data (8 * n))
  let len t = Cstruct.len t.data / 8
end

let erase cluster = Cstruct.memset cluster.data 0

let make ~cache ~cluster_bits ~locks () =
  let m = Lwt_mutex.create () in
  let c = Lwt_condition.create () in
  let cluster_map = None in
  { cache; cluster_map; cluster_bits; locks; m; c }

let set_cluster_map t cluster_map = t.cluster_map <- Some cluster_map

let read_and_lock ?client t cluster =
  let open Lwt.Infix in
  Locks.Read.lock ?client t.locks cluster
  >>= fun lock ->
  let open Lwt_error.Infix in
  Cache.read t.cache cluster
  >>= fun data ->
  Lwt.return (Ok ({ t; data; cluster }, lock))

(** Read the contents of [cluster] and apply the function [f] with the
    lock held. *)
let read ?client t cluster f =
  let open Lwt_error.Infix in
  Locks.Read.with_lock ?client t.locks cluster
    (fun () ->
       Cache.read t.cache cluster
       >>= fun data ->
       f { t; data; cluster }
    )

(** Read the contents of [cluster], transform it via function [f] and write
    back the results, all with the lock held. *)
let update ?client t cluster f =
  let open Lwt_write_error.Infix in
  Locks.Write.with_lock ?client t.locks cluster
    (fun () ->
       (* Cancel any in-progress move since the data will be stale *)
       begin match t.cluster_map with
         | Some cluster_map -> Qcow_cluster_map.cancel_move cluster_map cluster
         | None -> ()
       end;
       Cache.read t.cache cluster
       >>= fun data ->
       f { t; data; cluster }
       >>= fun result ->
       let open Lwt.Infix in
       Cache.write t.cache cluster data
       >>= function
       | Error `Is_read_only -> Lwt.return (Error `Is_read_only)
       | Error `Disconnected -> Lwt.return (Error `Disconnected)
       | Error `Unimplemented -> Lwt.return (Error `Unimplemented)
       | Ok () -> Lwt.return (Ok result)
    )
