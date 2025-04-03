(*
 * Copyright (C) 2015 David Scott <dave@recoil.org>
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
open Result
open Qcow_types
module Error = Qcow_error
module Header = Qcow_header
module Virtual = Qcow_virtual
module Physical = Qcow_physical
module Locks = Qcow_locks
module Cstructs = Qcow_cstructs
module Int = Qcow_int
module Int64 = Qcow_types.Int64

let ( <| ) = Int64.shift_left

let ( |> ) = Int64.shift_right_logical

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info) ;
  src

module Log = (val Logs.src_log src : Logs.LOG)

module DebugSetting = struct let compact_mid_write = ref false end

open Prometheus

module Metrics = struct
  let namespace = "Mirage"

  let subsystem = "qcow"

  let label_name = "id"

  let reads =
    let help = "Number of bytes read" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "reads"

  let writes =
    let help = "Number of bytes written" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "writes"

  let discards =
    let help = "Number of bytes discarded" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "discards"
end

module Make (Base : Qcow_s.RESIZABLE_BLOCK) (Time : Mirage_time.S) = struct
  (* samoht: `Msg should be the list of all possible exceptions *)
  type error = [Mirage_block.error | `Msg of string]

  module Lwt_error = Qcow_error.Lwt_error

  (* samoht: `Msg should be the list of all possible exceptions *)
  type write_error = [Mirage_block.write_error | `Msg of string]

  module Lwt_write_error = Qcow_error.Lwt_write_error

  let pp_error ppf = function
    | #Mirage_block.error as e ->
        Mirage_block.pp_error ppf e
    | `Msg s ->
        Fmt.string ppf s

  let pp_write_error ppf = function
    | #Mirage_block.write_error as e ->
        Mirage_block.pp_write_error ppf e
    | `Msg s ->
        Fmt.string ppf s

  module Config = Qcow_config

  (* Qemu-img will 'allocate' the last cluster by writing only the last sector.
     Cope with this by assuming all later sectors are full of zeroes *)
  module B = Qcow_padded.Make (Base)

  (* Run all threads in parallel, wait for all to complete, then iterate through
     the results and return the first failure we discover. *)
  let iter_p f xs =
    let threads = List.map f xs in
    Lwt_list.fold_left_s
      (fun acc t ->
        match acc with
        | Error x ->
            Lwt.return (Error x) (* first error wins *)
        | Ok () ->
            t
      )
      (Ok ()) threads

  module Cache = Qcow_cache
  module Recycler = Qcow_recycler.Make (B) (Time)
  module Metadata = Qcow_metadata

  module Stats = struct
    type t = {mutable nr_erased: int64; mutable nr_unmapped: int64}

    let zero = {nr_erased= 0L; nr_unmapped= 0L}
  end

  type t = {
      mutable h: Header.t
    ; base: B.t
    ; config: Config.t
    ; info: Mirage_block.info
    ; cache: Cache.t
    ; locks: Locks.t
    ; recycler: Recycler.t
    ; metadata: Metadata.t
    ; (* for convenience *)
      cluster_bits: int
    ; sector_size: int
    ; mutable lazy_refcounts: bool
          (* true if we are omitting refcounts right now *)
    ; mutable stats: Stats.t
    ; mutable cluster_map: Qcow_cluster_map.t
          (* a live map of the allocated storage *)
    ; cluster_map_m: Lwt_mutex.t
  }

  let get_info t = Lwt.return t.info

  let to_config t = t.config

  let get_stats t = t.stats

  let malloc t =
    let cluster_bits = Int32.to_int t.Header.cluster_bits in
    let npages = max 1 (cluster_bits lsl (cluster_bits - 12)) in
    let pages = Io_page.(to_cstruct (get npages)) in
    Cstruct.sub pages 0 (1 lsl cluster_bits)

  (* Mmarshal a disk physical address written at a given offset within the disk. *)
  let marshal_physical_address ?client t offset v =
    let cluster = Physical.cluster ~cluster_bits:t.cluster_bits offset in
    Metadata.update ?client t.metadata cluster (fun c ->
        let addresses = Metadata.Physical.of_contents c in
        let within =
          Physical.within_cluster ~cluster_bits:t.cluster_bits offset
        in
        try
          Metadata.Physical.set addresses within v ;
          Lwt.return (Ok ())
        with e -> Lwt.fail e
    )

  (* Unmarshal a disk physical address written at a given offset within the disk. *)
  let unmarshal_physical_address ?client t offset =
    let cluster = Physical.cluster ~cluster_bits:t.cluster_bits offset in
    let open Lwt_error.Infix in
    Metadata.read_and_lock ?client t.metadata cluster >>= fun (c, lock) ->
    let addresses = Metadata.Physical.of_contents c in
    let within = Physical.within_cluster ~cluster_bits:t.cluster_bits offset in
    Lwt.return (Ok (Metadata.Physical.get addresses within, lock))

  let adapt_error : B.error -> error = function
    | #Mirage_block.error as e ->
        e
    | _ ->
        `Msg "Unknown error"

  let adapt_write_error : B.write_error -> write_error = function
    | #Mirage_block.write_error as e ->
        e
    | _ ->
        `Msg "Unknown error"

  let adapt_write_error_result = function
    | Error e ->
        Lwt.return_error (adapt_write_error e)
    | Ok x ->
        Lwt.return_ok x

  let update_header t h =
    let cluster = malloc t.h in
    match Header.write h cluster with
    | Result.Ok _ -> (
        let open Lwt.Infix in
        B.write t.base 0L [cluster] >>= function
        | Error e ->
            Lwt.return_error (adapt_write_error e)
        | Ok () -> (
            Recycler.flush t.recycler >>= function
            | Error e ->
                Lwt.return_error (adapt_write_error e)
            | Ok () ->
                Log.debug (fun f -> f "Written header") ;
                t.h <- h ;
                Lwt.return (Ok ())
          )
      )
    | Result.Error (`Msg m) ->
        Lwt.return (Error (`Msg m))

  let resize_base base sector_size cluster_map new_size =
    let sector, within = Physical.to_sector ~sector_size new_size in
    if within <> 0 then
      Lwt.return
        (Error
           (`Msg
             (Printf.sprintf
                "Internal error: attempting to resize to a non-sector multiple \
                 %s"
                (Physical.to_string new_size)
             )
             )
        )
    else
      let open Lwt.Infix in
      ( match cluster_map with
      | Some (cluster_map, cluster_bits) ->
          let cluster = Physical.cluster ~cluster_bits new_size in
          Qcow_cluster_map.resize cluster_map cluster
      | None ->
          ()
      ) ;
      B.resize base sector >>= function
      | Error e ->
          Lwt.return_error (adapt_write_error e)
      | Ok () ->
          Log.debug (fun f ->
              f "Resized device to %d bytes" (Qcow_physical.to_bytes new_size)
          ) ;
          Lwt.return (Ok ())

  module ClusterIO = struct
    (** Allocate [n] clusters and registers them as new roots in the cluster map
        where [set] is a a set of possibly non-contiguous physical clusters which
        are guaranteed to contain zeroes.

        This must be called via Locks.with_metadata_lock, to prevent
        a parallel thread allocating another cluster for the same purpose.
        This also prevents the recycling thread from resizing the file
        concurrently.
        *)
    let allocate_clusters t n =
      let sectors_per_cluster = (1 lsl t.cluster_bits) / t.sector_size in

      let open Lwt.Infix in
      B.get_info t.base >>= fun base_info ->
      let open Lwt_write_error.Infix in
      (* If there is junk beyond the last block because someone just discarded
         something then truncate the file to erase it. *)
      let last_block = Qcow_cluster_map.get_last_block t.cluster_map in
      let last_file_block =
        Cluster.of_int
          (Int64.to_int base_info.Mirage_block.size_sectors
           / sectors_per_cluster
          - 1
          )
      in
      assert (last_block <= last_file_block) ;
      let rest_of_file =
        if last_block = last_file_block then
          Cluster.IntervalSet.empty
        else
          Cluster.IntervalSet.(
            add (Interval.make (Cluster.succ last_block) last_file_block) empty
          )
      in
      ( if
          Cluster.IntervalSet.(
            not
            @@ is_empty
            @@ inter rest_of_file
            @@ Qcow_cluster_map.Junk.get t.cluster_map
          )
        then (
          Log.debug (fun f ->
              f
                "Allocator: there is junk after the last block %s, shrinking \
                 file"
                (Cluster.to_string last_block)
          ) ;
          let size_clusters_should_be = Cluster.to_int last_block + 1 in
          let p = Physical.make (size_clusters_should_be lsl t.cluster_bits) in
          let size_sectors = Physical.sector ~sector_size:t.sector_size p in
          resize_base t.base t.sector_size
            (Some (t.cluster_map, t.cluster_bits))
            p
          >>= fun () ->
          Log.debug (fun f ->
              f "Resized file to %d clusters (%Ld sectors)"
                size_clusters_should_be size_sectors
          ) ;
          Lwt.return (Ok size_sectors)
        ) else
          Lwt.return (Ok base_info.Mirage_block.size_sectors)
      )
      >>= fun size_sectors ->
      let limit = 256 in
      (* 16 MiB *)
      let quantum = 512 in

      (* 32 MiB *)
      let max_cluster_needed = Cluster.to_int last_block + n in
      let len_cluster = Int64.to_int size_sectors / sectors_per_cluster in
      let len_cluster_should_be =
        if len_cluster - max_cluster_needed < limit then
          len_cluster + quantum
        else
          len_cluster
      in
      (* keep it the same *)
      ( if len_cluster_should_be <> len_cluster then (
          Log.info (fun f ->
              f "Allocator: %s"
                (Qcow_cluster_map.to_summary_string t.cluster_map)
          ) ;
          Log.info (fun f ->
              f
                "Allocator: file contains cluster 0 .. %d will enlarge file to \
                 0 .. %d"
                (len_cluster - 1)
                (len_cluster_should_be - 1)
          ) ;
          (* Resync the file size only *)
          let p = Physical.make (len_cluster_should_be lsl t.cluster_bits) in
          let size_sectors = Physical.sector ~sector_size:t.sector_size p in
          resize_base t.base t.sector_size
            (Some (t.cluster_map, t.cluster_bits))
            p
          >>= fun () ->
          Log.debug (fun f ->
              f "Resized file to %d clusters (%Ld sectors)"
                len_cluster_should_be size_sectors
          ) ;
          Lwt.return (Ok ())
        ) else
          Lwt.return (Ok ())
      )
      >>= fun () ->
      match Recycler.allocate t.recycler (Cluster.of_int n) with
      | Some set ->
          Log.debug (fun f -> f "Allocated %d clusters from free list" n) ;
          Lwt.return (Ok set)
      | None ->
          assert false (* never happens because of the `resize_base` above *)

    module Refcount = struct
      (* The refcount table contains pointers to clusters which themselves
         contain the 2-byte refcounts *)

      let zero_all ?client t =
        (* Zero all clusters allocated in the refcount table *)
        let cluster =
          Physical.cluster ~cluster_bits:t.cluster_bits
            t.h.Header.refcount_table_offset
        in
        let refcount_table_clusters =
          Int32.to_int t.h.Header.refcount_table_clusters
        in
        let rec loop i =
          if i >= refcount_table_clusters then
            Lwt.return (Ok ())
          else
            (* `read` expects the function to be read-only, however we cheat and
                perform write operations inside the read context *)
            let open Lwt_error.Infix in
            Metadata.read ?client t.metadata
              Cluster.(add cluster (of_int i))
              (fun c ->
                let addresses = Metadata.Physical.of_contents c in
                let rec loop i =
                  if i >= Metadata.Physical.len addresses then
                    Lwt.return (Ok ())
                  else
                    let open Lwt_write_error.Infix in
                    let addr = Metadata.Physical.get addresses i in
                    ( if Physical.to_bytes addr <> 0 then
                        let cluster =
                          Physical.cluster ~cluster_bits:t.cluster_bits addr
                        in
                        Metadata.update ?client t.metadata cluster (fun c ->
                            Metadata.erase c ; Lwt.return (Ok ())
                        )
                        >>= fun () ->
                        let open Lwt.Infix in
                        Recycler.flush t.recycler >>= adapt_write_error_result
                      else
                        Lwt.return (Ok ())
                    )
                    >>= fun () -> loop (i + 1)
                in
                let open Lwt.Infix in
                loop 0 >>= function
                | Error `Disconnected ->
                    Lwt.return (Error `Disconnected)
                | Error `Is_read_only ->
                    Lwt.return (Error (`Msg "Device is read only"))
                | Error (`Msg m) ->
                    Lwt.return (Error (`Msg m))
                | Ok () ->
                    Lwt.return (Ok ())
              )
            >>= fun () -> loop (i + 1)
        in
        loop 0

      let read ?client t cluster =
        let cluster = Cluster.to_int64 cluster in
        let within_table =
          Int64.(div cluster (Header.refcounts_per_cluster t.h))
        in
        let within_cluster =
          Int64.(to_int (rem cluster (Header.refcounts_per_cluster t.h)))
        in

        let offset =
          Physical.add t.h.Header.refcount_table_offset
            (8 * Int64.to_int within_table)
        in
        let open Lwt_error.Infix in
        unmarshal_physical_address ?client t offset >>= fun (offset, lock) ->
        Lwt.finalize
          (fun () ->
            if Physical.to_bytes offset = 0 then
              Lwt.return (Ok 0)
            else
              let cluster =
                Physical.cluster ~cluster_bits:t.cluster_bits offset
              in
              Metadata.read ?client t.metadata cluster (fun c ->
                  let refcounts = Metadata.Refcounts.of_contents c in
                  Lwt.return
                    (Ok (Metadata.Refcounts.get refcounts within_cluster))
              )
          )
          (fun () -> Locks.unlock lock ; Lwt.return_unit)

      (** Decrement the refcount of a given cluster. This will never need to allocate.
          We never bother to deallocate refcount clusters which are empty. *)
      let really_decr ?client t cluster =
        let cluster = Cluster.to_int64 cluster in
        let within_table =
          Int64.(div cluster (Header.refcounts_per_cluster t.h))
        in
        let within_cluster =
          Int64.(to_int (rem cluster (Header.refcounts_per_cluster t.h)))
        in

        let offset =
          Physical.add t.h.Header.refcount_table_offset
            (8 * Int64.to_int within_table)
        in
        let open Lwt_write_error.Infix in
        unmarshal_physical_address ?client t offset >>= fun (offset, lock) ->
        Lwt.finalize
          (fun () ->
            if Physical.to_bytes offset = 0 then (
              Log.err (fun f ->
                  f
                    "Refcount.decr: cluster %Ld has no refcount cluster \
                     allocated"
                    cluster
              ) ;
              Lwt.return
                (Error
                   (`Msg
                     (Printf.sprintf
                        "Refcount.decr: cluster %Ld has no refcount cluster \
                         allocated"
                        cluster
                     )
                     )
                )
            ) else
              let cluster =
                Physical.cluster ~cluster_bits:t.cluster_bits offset
              in
              Metadata.update ?client t.metadata cluster (fun c ->
                  let refcounts = Metadata.Refcounts.of_contents c in
                  let current =
                    Metadata.Refcounts.get refcounts within_cluster
                  in
                  if current = 0 then (
                    Log.err (fun f ->
                        f
                          "Refcount.decr: cluster %s already has a refcount of \
                           0"
                          (Cluster.to_string cluster)
                    ) ;
                    Lwt.return
                      (Error
                         (`Msg
                           (Printf.sprintf
                              "Refcount.decr: cluster %s already has a \
                               refcount of 0"
                              (Cluster.to_string cluster)
                           )
                           )
                      )
                  ) else (
                    Metadata.Refcounts.set refcounts within_cluster (current - 1) ;
                    Lwt.return (Ok ())
                  )
              )
          )
          (fun () -> Locks.unlock lock ; Lwt.return_unit)

      (** Increment the refcount of a given cluster. Note this might need
          to allocate itself, to enlarge the refcount table. When this function
          returns the refcount is guaranteed to have been persisted. *)
      let rec really_incr ?client t cluster =
        let open Lwt_write_error.Infix in
        let cluster = Cluster.to_int64 cluster in
        let within_table =
          Int64.(div cluster (Header.refcounts_per_cluster t.h))
        in
        let within_cluster =
          Int64.(to_int (rem cluster (Header.refcounts_per_cluster t.h)))
        in

        (* If the table (containing pointers to clusters which contain the refcounts)
           is too small, then reallocate it now. *)
        let cluster_containing_pointer =
          let within_table_offset = Int64.mul within_table 8L in
          within_table_offset |> t.cluster_bits
        in
        let current_size_clusters =
          Int64.of_int32 t.h.Header.refcount_table_clusters
        in
        ( if cluster_containing_pointer >= current_size_clusters then
            let needed = Header.max_refcount_table_size t.h in
            (* Make sure this is actually an increase: make the table 2x larger if not *)
            let needed =
              if needed = current_size_clusters then
                Int64.mul 2L current_size_clusters
              else
                needed
            in
            allocate_clusters t (Int64.to_int needed) >>= fun free ->
            Lwt.finalize
              (fun () ->
                (* Erasing new blocks is handled after the copy *)
                (* Copy any existing refcounts into new table *)
                let buf = malloc t.h in
                let rec loop free i =
                  if i >= Int32.to_int t.h.Header.refcount_table_clusters then
                    Lwt.return (Ok ())
                  else
                    let physical =
                      Physical.add t.h.Header.refcount_table_offset
                        (i lsl t.cluster_bits)
                    in
                    let src =
                      Physical.cluster ~cluster_bits:t.cluster_bits physical
                    in
                    let first =
                      Cluster.IntervalSet.(Interval.x (min_elt free))
                    in
                    let physical =
                      Physical.make (Cluster.to_int first lsl t.cluster_bits)
                    in
                    let dst =
                      Physical.cluster ~cluster_bits:t.cluster_bits physical
                    in
                    let open Lwt.Infix in
                    Recycler.copy t.recycler src dst >>= function
                    | Error e ->
                        Lwt.return_error (adapt_write_error e)
                    | Ok () ->
                        let free =
                          Cluster.IntervalSet.(
                            remove (Interval.make first first) free
                          )
                        in
                        loop free (i + 1)
                in
                loop free 0 >>= fun () ->
                Log.debug (fun f -> f "Copied refcounts into new table") ;
                (* Zero new blocks *)
                Cstruct.memset buf 0 ;
                let rec loop free i =
                  if i >= needed then
                    Lwt.return (Ok ())
                  else
                    let first =
                      Cluster.IntervalSet.(Interval.x (min_elt free))
                    in
                    let physical =
                      Physical.make (Cluster.to_int first lsl t.cluster_bits)
                    in
                    let sector, _ =
                      Physical.to_sector ~sector_size:t.sector_size physical
                    in
                    let open Lwt.Infix in
                    B.write t.base sector [buf] >>= function
                    | Error e ->
                        Lwt.return_error (adapt_write_error e)
                    | Ok () ->
                        let free =
                          Cluster.IntervalSet.(
                            remove (Interval.make first first) free
                          )
                        in
                        loop free (Int64.succ i)
                in
                loop free (Int64.of_int32 t.h.Header.refcount_table_clusters)
                >>= fun () ->
                let first = Cluster.IntervalSet.(Interval.x (min_elt free)) in
                let refcount_table_offset =
                  Physical.make (Cluster.to_int first lsl t.cluster_bits)
                in
                let h' =
                  {
                    t.h with
                    Header.refcount_table_offset
                  ; refcount_table_clusters= Int64.to_int32 needed
                  }
                in
                update_header t h' >>= fun () ->
                (* increase the refcount of the clusters we just allocated *)
                let rec loop free i =
                  if i >= needed then
                    Lwt.return (Ok ())
                  else
                    let first =
                      Cluster.IntervalSet.(Interval.x (min_elt free))
                    in
                    really_incr ?client t first >>= fun () ->
                    let free =
                      Cluster.IntervalSet.(
                        remove (Interval.make first first) free
                      )
                    in
                    loop free (Int64.succ i)
                in
                loop free 0L
              )
              (fun () ->
                Qcow_cluster_map.Roots.remove t.cluster_map free ;
                Lwt.return_unit
              )
          else
            Lwt.return (Ok ())
        )
        >>= fun () ->
        let offset =
          Physical.add t.h.Header.refcount_table_offset
            (8 * Int64.to_int within_table)
        in
        unmarshal_physical_address ?client t offset >>= fun (addr, lock) ->
        Lwt.finalize
          (fun () ->
            ( if Physical.to_bytes addr = 0 then
                allocate_clusters t 1 >>= fun free ->
                Lwt.finalize
                  (fun () ->
                    let cluster =
                      Cluster.IntervalSet.(Interval.x (min_elt free))
                    in
                    (* NB: the pointers in the refcount table are different from the pointers
                       in the cluster table: the high order bits are not used to encode extra
                       information and wil confuse qemu/qemu-img. *)
                    let addr =
                      Physical.make (Cluster.to_int cluster lsl t.cluster_bits)
                    in
                    (* zero the cluster *)
                    let buf = malloc t.h in
                    Cstruct.memset buf 0 ;
                    let sector, _ =
                      Physical.to_sector ~sector_size:t.sector_size addr
                    in
                    let open Lwt.Infix in
                    B.write t.base sector [buf] >>= function
                    | Error e ->
                        Lwt.return_error (adapt_write_error e)
                    | Ok () -> (
                        (* Ensure the new zeroed cluster has been persisted before we reference
                           it via `marshal_physical_address` *)
                        Recycler.flush t.recycler
                        >>= function
                        | Error e ->
                            Lwt.return_error (adapt_write_error e)
                        | Ok () -> (
                            Log.debug (fun f ->
                                f "Allocated new refcount cluster %s"
                                  (Cluster.to_string cluster)
                            ) ;
                            let open Lwt_write_error.Infix in
                            marshal_physical_address ?client t offset addr
                            >>= fun () ->
                            let open Lwt.Infix in
                            Recycler.flush t.recycler >>= function
                            | Error e ->
                                Lwt.return_error (adapt_write_error e)
                            | Ok () ->
                                let open Lwt_write_error.Infix in
                                really_incr ?client t cluster >>= fun () ->
                                Lwt.return (Ok addr)
                          )
                      )
                  )
                  (fun () ->
                    Qcow_cluster_map.Roots.remove t.cluster_map free ;
                    Lwt.return_unit
                  )
              else
                Lwt.return (Ok addr)
            )
            >>= fun offset ->
            let refcount_cluster =
              Physical.cluster ~cluster_bits:t.cluster_bits offset
            in
            Metadata.update ?client t.metadata refcount_cluster (fun c ->
                let refcounts = Metadata.Refcounts.of_contents c in
                let current = Metadata.Refcounts.get refcounts within_cluster in
                (* We don't support refcounts of more than 1 *)
                assert (current == 0) ;
                Metadata.Refcounts.set refcounts within_cluster (current + 1) ;
                Lwt.return (Ok ())
            )
          )
          (fun () -> Locks.unlock lock ; Lwt.return_unit)
        >>= fun () ->
        let open Lwt.Infix in
        Recycler.flush t.recycler >>= function
        | Error e ->
            Lwt.return_error (adapt_write_error e)
        | Ok () ->
            Log.debug (fun f -> f "Incremented refcount of cluster %Ld" cluster) ;
            Lwt.return (Ok ())

      (* If the lazy refcounts feature is enabled then don't actually Increment
         the refcounts. *)
      let incr ?client t cluster =
        if t.lazy_refcounts then
          Lwt.return (Ok ())
        else
          really_incr ?client t cluster

      let decr ?client t cluster =
        if t.lazy_refcounts then
          Lwt.return (Ok ())
        else
          really_decr ?client t cluster
    end

    let read_l1_table ?client t l1_index =
      (* Read l1[l1_index] as a 64-bit offset *)
      let l1_index_offset =
        Physical.shift t.h.Header.l1_table_offset (8 * Int64.to_int l1_index)
      in
      unmarshal_physical_address ?client t l1_index_offset

    (* Find the first l1_index whose values satisfies [f] *)
    let find_mapped_l1_table t l1_index =
      let open Lwt_error.Infix in
      (* Read l1[l1_index] as a 64-bit offset *)
      let rec loop l1_index =
        if l1_index >= Int64.of_int32 t.h.Header.l1_size then
          Lwt.return (Ok None)
        else
          let l1_index_offset =
            Physical.shift t.h.Header.l1_table_offset (8 * Int64.to_int l1_index)
          in

          let cluster =
            Physical.cluster ~cluster_bits:t.cluster_bits l1_index_offset
          in

          Metadata.read t.metadata cluster (fun c ->
              let addresses = Metadata.Physical.of_contents c in
              let within =
                Physical.within_cluster ~cluster_bits:t.cluster_bits
                  l1_index_offset
              in
              let rec loop l1_index i : [`Skip of int | `GotOne of int64] =
                if i >= Metadata.Physical.len addresses then
                  `Skip i
                else if Metadata.Physical.get addresses i <> Physical.unmapped
                then
                  `GotOne l1_index
                else
                  loop (Int64.succ l1_index) (i + 1)
              in
              Lwt.return (Ok (loop l1_index within))
          )
          >>= function
          | `GotOne l1_index' ->
              Lwt.return (Ok (Some l1_index'))
          | `Skip n ->
              loop Int64.(add l1_index (of_int n))
      in
      loop l1_index

    let write_l1_table ?client t l1_index l2_table_offset =
      let open Lwt_write_error.Infix in
      (* Always set the mutable flag *)
      let l2_table_offset =
        if l2_table_offset = Physical.unmapped then
          Physical.unmapped (* don't set metadata bits for unmapped clusters *)
        else
          Physical.make ~is_mutable:true (Physical.to_bytes l2_table_offset)
      in
      (* Write l1[l1_index] as a 64-bit offset *)
      let l1_index_offset =
        Physical.shift t.h.Header.l1_table_offset (8 * Int64.to_int l1_index)
      in
      marshal_physical_address ?client t l1_index_offset l2_table_offset
      >>= fun () ->
      Log.debug (fun f ->
          f "Written l1_table[%Ld] <- %s" l1_index
            (Cluster.to_string
            @@ Physical.cluster ~cluster_bits:t.cluster_bits l2_table_offset
            )
      ) ;
      Lwt.return (Ok ())

    let read_l2_table ?client t l2_table_offset l2_index =
      let l2_index_offset =
        Physical.shift l2_table_offset (8 * Int64.to_int l2_index)
      in
      unmarshal_physical_address ?client t l2_index_offset

    let write_l2_table ?client t l2_table_offset l2_index cluster =
      let open Lwt_write_error.Infix in
      (* Always set the mutable flag *)
      let cluster =
        if cluster = Physical.unmapped then
          Physical.unmapped (* don't set metadata bits for unmapped clusters *)
        else
          Physical.make ~is_mutable:true (Physical.to_bytes cluster)
      in
      let l2_index_offset =
        Physical.shift l2_table_offset (8 * Int64.to_int l2_index)
      in
      marshal_physical_address ?client t l2_index_offset cluster >>= fun _ ->
      Log.debug (fun f ->
          f "Written l2_table[%Ld] <- %s" l2_index
            (Cluster.to_string
            @@ Physical.cluster ~cluster_bits:t.cluster_bits cluster
            )
      ) ;
      Lwt.return (Ok ())

    (* Walk the L1 and L2 tables to translate an address. If a table entry
       is unallocated then return [None]. Note if a [walk_and_allocate] is
       racing with us then we may or may not see the mapping. *)
    let walk_readonly ?client t a =
      let open Lwt_error.Infix in
      Locks.with_metadata_lock t.locks (fun () ->
          read_l1_table ?client t a.Virtual.l1_index
          >>= fun (l2_table_offset, l1_lock) ->
          let ( >>|= ) m f =
            let open Lwt in
            m >>= function
            | Error x ->
                Lwt.return (Error x)
            | Ok None ->
                Lwt.return (Ok None)
            | Ok (Some x) ->
                f x
          in

          (* Look up an L2 table *)
          ( if Physical.to_bytes l2_table_offset = 0 then (
              Locks.unlock l1_lock ; Lwt.return (Ok None)
            ) else (
              if Physical.is_compressed l2_table_offset then
                failwith "compressed" ;
              Lwt.return (Ok (Some l2_table_offset))
            )
          )
          >>|= fun l2_table_offset ->
          (* Look up a cluster *)
          read_l2_table ?client t l2_table_offset a.Virtual.l2_index
          >>= fun (cluster_offset, l2_lock) ->
          ( if Physical.to_bytes cluster_offset = 0 then (
              Locks.unlock l1_lock ; Locks.unlock l2_lock ; Lwt.return (Ok None)
            ) else (
              if Physical.is_compressed cluster_offset then
                failwith "compressed" ;
              Lwt.return (Ok (Some cluster_offset))
            )
          )
          >>|= fun cluster_offset ->
          let p =
            Physical.shift cluster_offset (Int64.to_int a.Virtual.cluster)
          in
          Lwt.return (Ok (Some (p, l1_lock, l2_lock)))
      )

    (* Walk the L1 and L2 tables to translate an address, allocating missing
       entries as we go. *)
    let walk_and_allocate ?client t a =
      let open Lwt_write_error.Infix in
      Locks.with_metadata_lock t.locks (fun () ->
          read_l1_table ?client t a.Virtual.l1_index
          >>= fun (l2_offset, l1_lock) ->
          (* If there is no L2 table entry then allocate L2 and data clusters
             at the same time to minimise I/O *)
          ( if Physical.to_bytes l2_offset = 0 then
              allocate_clusters t 2 >>= fun free ->
              Lwt.finalize
                (fun () ->
                  (* FIXME: it's unnecessary to write to the data cluster if we're
                     about to overwrite it with real data straight away *)
                  let open Lwt_write_error.Infix in
                  let l2_cluster =
                    Cluster.IntervalSet.(Interval.x (min_elt free))
                  in
                  let free =
                    Cluster.IntervalSet.(
                      remove (Interval.make l2_cluster l2_cluster) free
                    )
                  in
                  let data_cluster =
                    Cluster.IntervalSet.(Interval.x (min_elt free))
                  in
                  Refcount.incr t l2_cluster >>= fun () ->
                  Refcount.incr t data_cluster >>= fun () ->
                  let l2_offset =
                    Physical.make (Cluster.to_int l2_cluster lsl t.cluster_bits)
                  in
                  let data_offset =
                    Physical.make
                      (Cluster.to_int data_cluster lsl t.cluster_bits)
                  in
                  write_l2_table ?client t l2_offset a.Virtual.l2_index
                    data_offset
                  >>= fun () ->
                  read_l2_table ?client t l2_offset a.Virtual.l2_index
                  >>= fun (data_offset', l2_lock) ->
                  (* NB the new blocks can't be moved within the `allocate_clusters` callback
                     since they are registered as global roots *)
                  assert (
                    Physical.to_bytes data_offset
                    = Physical.to_bytes data_offset'
                  ) ;
                  write_l1_table ?client t a.Virtual.l1_index l2_offset
                  >>= fun () -> Lwt.return (Ok (data_offset, l1_lock, l2_lock))
                )
                (fun () ->
                  Qcow_cluster_map.Roots.remove t.cluster_map free ;
                  Lwt.return_unit
                )
            else
              read_l2_table ?client t l2_offset a.Virtual.l2_index
              >>= fun (data_offset, l2_lock) ->
              if Physical.to_bytes data_offset = 0 then
                allocate_clusters t 1 >>= fun free ->
                Lwt.finalize
                  (fun () ->
                    let open Lwt_write_error.Infix in
                    let data_cluster =
                      Cluster.IntervalSet.(Interval.x (min_elt free))
                    in
                    Refcount.incr t data_cluster >>= fun () ->
                    let data_offset =
                      Physical.make
                        (Cluster.to_int data_cluster lsl t.cluster_bits)
                    in
                    let open Lwt_write_error.Infix in
                    write_l2_table ?client t l2_offset a.Virtual.l2_index
                      data_offset
                    >>= fun () -> Lwt.return (Ok (data_offset, l1_lock, l2_lock))
                  )
                  (fun () ->
                    Qcow_cluster_map.Roots.remove t.cluster_map free ;
                    Lwt.return_unit
                  )
              else (
                if Physical.is_compressed data_offset then failwith "compressed" ;
                Lwt.return (Ok (data_offset, l1_lock, l2_lock))
              )
          )
          >>= fun (data_offset, l1_lock, l2_lock) ->
          let p = Physical.shift data_offset (Int64.to_int a.Virtual.cluster) in
          Lwt.return (Ok (p, l1_lock, l2_lock))
      )

    let walk_and_deallocate ?client t sector n =
      let open Lwt_write_error.Infix in
      let sectors_per_cluster =
        Int64.(div (1L <| t.cluster_bits) (of_int t.sector_size))
      in
      Locks.with_metadata_lock t.locks (fun () ->
          let get_l2 sector =
            let byte =
              Int64.(mul sector (of_int t.info.Mirage_block.sector_size))
            in
            let a = Virtual.make ~cluster_bits:t.cluster_bits byte in
            read_l1_table ?client t a.Virtual.l1_index
            >>= fun (l2_offset, l1_lock) ->
            if Physical.to_bytes l2_offset = 0 then (
              Locks.unlock l1_lock ; Lwt.return (Ok None)
            ) else
              let l2_index_offset =
                Physical.shift l2_offset (8 * Int64.to_int a.Virtual.l2_index)
              in
              let cluster =
                Physical.cluster ~cluster_bits:t.cluster_bits l2_index_offset
              in
              let within =
                Physical.within_cluster ~cluster_bits:t.cluster_bits
                  l2_index_offset
              in
              Lwt.return (Ok (Some ((cluster, within), l1_lock)))
          in
          let rec loop sector n =
            if n = 0L then
              Lwt.return (Ok ())
            else
              (get_l2 sector >>= function
               | None ->
                   (* FIXME: we can almost certainly jump more than this *)
                   Lwt.return (Ok sectors_per_cluster)
               | Some ((cluster, _), l1_lock) ->
                   Lwt.finalize
                     (fun () ->
                       Metadata.update ?client t.metadata cluster (fun c ->
                           let addresses = Metadata.Physical.of_contents c in
                           (* With the cluster write lock held, complete as many
                              writes to it as we need, unlocking and writing it out
                              once at the end. *)
                           let rec inner acc sector n =
                             if n = 0L then
                               Lwt.return (Ok acc)
                             else
                               get_l2 sector >>= function
                               | None ->
                                   Lwt.return (Ok acc)
                               | Some ((cluster', _), l1_lock)
                                 when cluster <> cluster' ->
                                   Locks.unlock l1_lock ; Lwt.return (Ok acc)
                               | Some ((_, within), l1_lock) ->
                                   Locks.unlock l1_lock ;
                                   (* still locked above *)
                                   let data_offset =
                                     Metadata.Physical.get addresses within
                                   in
                                   if Physical.to_bytes data_offset = 0 then
                                     inner
                                       (Int64.add acc sectors_per_cluster)
                                       (Int64.add sector sectors_per_cluster)
                                       (Int64.sub n sectors_per_cluster)
                                   else
                                     (* The data at [data_offset] is about to become an unreferenced
                                        hole in the file *)
                                     let current =
                                       Metadata.Physical.get addresses within
                                     in
                                     ( if current <> Physical.unmapped then
                                         Locks.Write.with_lock t.locks ?client
                                           (Physical.cluster
                                              ~cluster_bits:t.cluster_bits
                                              current
                                           ) (fun () ->
                                             (* It's important to hold the write lock because we might
                                                be about to erase or copy this block *)
                                             Metadata.Physical.set addresses
                                               within Physical.unmapped ;
                                             t.stats.Stats.nr_unmapped <-
                                               Int64.add
                                                 t.stats.Stats.nr_unmapped
                                                 sectors_per_cluster ;
                                             Lwt.return (Ok ())
                                         )
                                       else
                                         Lwt.return (Ok ())
                                     )
                                     >>= fun () ->
                                     Refcount.decr t
                                       (Physical.cluster
                                          ~cluster_bits:t.cluster_bits
                                          data_offset
                                       )
                                     >>= fun () ->
                                     inner
                                       (Int64.add acc sectors_per_cluster)
                                       (Int64.add sector sectors_per_cluster)
                                       (Int64.sub n sectors_per_cluster)
                           in
                           inner 0L sector n
                       )
                     )
                     (fun () -> Locks.unlock l1_lock ; Lwt.return_unit)
              )
              >>= fun to_advance ->
              loop (Int64.add sector to_advance) (Int64.sub n to_advance)
          in
          loop sector n
      )
  end

  (* Starting at byte offset [ofs], map a list of buffers onto a list of
     [byte offset, buffer] pairs, where
       - no [byte offset, buffer] pair crosses an [alignment] boundary;
       - each [buffer] is as large as possible (so for example if we supply
         one large buffer it will only be fragmented to the minimum extent. *)
  let rec chop_into_aligned alignment ofs = function
    | [] ->
        []
    | buf :: bufs ->
        (* If we're not aligned, sync to the next boundary *)
        let into = Int64.(to_int (sub alignment (rem ofs alignment))) in
        if Cstruct.length buf > into then
          let this = (ofs, Cstruct.sub buf 0 into) in
          let rest =
            chop_into_aligned alignment
              Int64.(add ofs (of_int into))
              (Cstruct.shift buf into :: bufs)
          in
          this :: rest
        else
          (ofs, buf)
          :: chop_into_aligned alignment
               Int64.(add ofs (of_int (Cstruct.length buf)))
               bufs

  type work = {
      sector: int64 (* starting sector of the operaiton *)
    ; bufs: Cstruct.t list
    ; metadata_locks: Locks.lock list
          (* read locks on the metadata pointing to the physical clusters: our guarantee
             that the target physical clusters haven't been moved and the references
             rewritten *)
  }

  (* Given a list of offset, buffer pairs for reading or writing, coalesce
     adjacent offsets for readv/writev *)
  let coalesce_into_adjacent sector_size =
    let rec loop sector bufs metadata_locks next_sector acc = function
      | [] ->
          List.rev ({sector; bufs= List.rev bufs; metadata_locks} :: acc)
      | work :: rest ->
          let next_sector' =
            Int64.(
              add work.sector (of_int (Cstructs.len work.bufs / sector_size))
            )
          in
          if next_sector = work.sector then
            loop sector (work.bufs @ bufs)
              (work.metadata_locks @ metadata_locks)
              next_sector' acc rest
          else
            loop work.sector work.bufs work.metadata_locks next_sector'
              ({sector; bufs= List.rev bufs; metadata_locks} :: acc)
              rest
    in
    function
    | [] ->
        []
    | work :: rest ->
        let next_sector' =
          Int64.(add work.sector (of_int (Cstructs.len work.bufs / sector_size)))
        in
        loop work.sector work.bufs work.metadata_locks next_sector' [] rest

  exception Reference_outside_file of int64 * int64

  let make_cluster_map t ?id () =
    let open Qcow_cluster_map in
    let sectors_per_cluster =
      Int64.(div (1L <| t.cluster_bits) (of_int t.sector_size))
    in
    let open Lwt.Infix in
    B.get_info t.base >>= fun base_info ->
    let max_cluster =
      Cluster.of_int64
      @@ Int64.div base_info.Mirage_block.size_sectors sectors_per_cluster
    in
    (* Iterate over the all clusters referenced from all the tables in the file
       and (a) construct a set of free clusters; and (b) construct a map of
       physical cluster back to virtual. The free set will show us the holes,
       and the map will tell us where to get the data from to fill the holes in
       with. *)
    let refs = ref Cluster.Map.empty in

    let refcount_start_cluster =
      Cluster.to_int64
      @@ Physical.cluster ~cluster_bits:t.cluster_bits
           t.h.Header.refcount_table_offset
    in
    let int64s_per_cluster = 1L <| Int32.to_int t.h.Header.cluster_bits - 3 in
    let l1_table_start_cluster =
      Cluster.to_int64
      @@ Physical.cluster ~cluster_bits:t.cluster_bits
           t.h.Header.l1_table_offset
    in
    let l1_table_clusters =
      Int64.(
        div
          (round_up (of_int32 t.h.Header.l1_size) int64s_per_cluster)
          int64s_per_cluster
      )
    in
    (* Assume all clusters are free. Note when the file is sparse we can exceed the max
       possible cluster. This is only a sanity check to catch crazily-wrong inputs. *)
    let cluster_size = 1L <| t.cluster_bits in
    let max_possible_cluster =
      Cluster.of_int64
        (Int64.round_up t.h.Header.size cluster_size |> t.cluster_bits)
    in
    let free =
      Qcow_bitmap.make_full
        ~initial_size:(Cluster.to_int max_cluster)
        ~maximum_size:(Cluster.to_int max_possible_cluster * 50)
    in
    (* The header structures are untracked by the qcow_cluster_map and we assume
       they don't move and we don't try to move them. We assume the structures
       have no holes in them, otherwise we would miscompute the `first_movable_cluster`
       and accidentally truncate the file. *)
    Qcow_bitmap.(
      remove
        (Interval.make 0L
           Int64.(pred @@ add l1_table_start_cluster l1_table_clusters)
        )
        free
    ) ;
    Qcow_bitmap.(
      remove
        (Interval.make 0L
           Int64.(
             pred
             @@ add refcount_start_cluster
                  (Int64.of_int32 t.h.Header.refcount_table_clusters)
           )
        )
        free
    ) ;
    Qcow_bitmap.(remove (Interval.make 0L 0L) free) ;
    let first_movable_cluster =
      try Cluster.of_int64 @@ Qcow_bitmap.min_elt free
      with Not_found -> max_cluster (* header takes up the whole file *)
    in
    let parse x =
      if x = Physical.unmapped then
        Cluster.zero
      else
        Physical.cluster ~cluster_bits:t.cluster_bits x
    in

    let mark rf cluster =
      let c, w = rf in
      if cluster > max_cluster then (
        Log.err (fun f ->
            f
              "Found a reference to cluster %s outside the file (max cluster \
               %s) from cluster %s.%d"
              (Cluster.to_string cluster)
              (Cluster.to_string max_cluster)
              (Cluster.to_string c) w
        ) ;
        let src =
          Int64.add (Int64.of_int w)
            (Cluster.to_int64 c <| Int32.to_int t.h.Header.cluster_bits)
        in
        let dst =
          Cluster.to_int64 cluster <| Int32.to_int t.h.Header.cluster_bits
        in
        raise (Reference_outside_file (src, dst))
      ) ;
      let c, w = rf in
      if cluster = Cluster.zero then
        ()
      else (
        if Cluster.Map.mem cluster !refs then (
          let c', w' = Cluster.Map.find cluster !refs in
          Log.err (fun f ->
              f "Found two references to cluster %s: %s.%d and %s.%d"
                (Cluster.to_string cluster)
                (Cluster.to_string c) w (Cluster.to_string c') w'
          ) ;
          raise
            (Error.Duplicate_reference
               ( (Cluster.to_int64 c, w)
               , (Cluster.to_int64 c', w')
               , Cluster.to_int64 cluster
               )
            )
        ) ;
        Qcow_bitmap.(
          remove
            (Interval.make (Cluster.to_int64 cluster) (Cluster.to_int64 cluster))
            free
        ) ;
        refs := Cluster.Map.add cluster rf !refs
      )
    in

    (* scan the refcount table *)
    let open Lwt_error.Infix in
    let rec loop i =
      if i >= Int64.of_int32 t.h.Header.refcount_table_clusters then
        Lwt.return (Ok ())
      else
        let refcount_cluster =
          Cluster.of_int64 @@ Int64.(add refcount_start_cluster i)
        in
        Metadata.read t.metadata refcount_cluster (fun c ->
            let addresses = Metadata.Physical.of_contents c in
            let rec loop i =
              if i >= Metadata.Physical.len addresses then
                Lwt.return (Ok ())
              else
                let cluster = parse (Metadata.Physical.get addresses i) in
                mark (refcount_cluster, i) cluster ;
                loop (i + 1)
            in
            loop 0
        )
        >>= fun () -> loop (Int64.succ i)
    in
    loop 0L >>= fun () ->
    (* scan the L1 and L2 tables, marking the L2 and data clusters *)
    let rec l1_iter i =
      let l1_table_cluster =
        Cluster.of_int64 @@ Int64.(add l1_table_start_cluster i)
      in
      if i >= l1_table_clusters then
        Lwt.return (Ok ())
      else
        Metadata.read t.metadata l1_table_cluster (fun c ->
            let l1 = Metadata.Physical.of_contents c in
            Lwt.return (Ok l1)
        )
        >>= fun l1 ->
        let rec l2_iter i =
          if i >= Metadata.Physical.len l1 then
            Lwt.return (Ok ())
          else
            let l2_table_cluster = parse (Metadata.Physical.get l1 i) in
            if l2_table_cluster <> Cluster.zero then (
              mark (l1_table_cluster, i) l2_table_cluster ;
              Metadata.read t.metadata l2_table_cluster (fun c ->
                  let l2 = Metadata.Physical.of_contents c in
                  Lwt.return (Ok l2)
              )
              >>= fun l2 ->
              let rec data_iter i =
                if i >= Metadata.Physical.len l2 then
                  Lwt.return (Ok ())
                else
                  let cluster = parse (Metadata.Physical.get l2 i) in
                  mark (l2_table_cluster, i) cluster ;
                  data_iter (i + 1)
              in
              data_iter 0 >>= fun () -> l2_iter (i + 1)
            ) else
              l2_iter (i + 1)
        in
        l2_iter 0 >>= fun () -> l1_iter (Int64.succ i)
    in
    l1_iter 0L >>= fun () ->
    let map =
      make ~free ~refs:!refs ~first_movable_cluster ~cache:t.cache
        ~runtime_asserts:t.config.Config.runtime_asserts ~id
        ~cluster_size:(Int64.to_int cluster_size)
    in

    Lwt.return (Ok map)

  type check_result = {free: int64; used: int64}

  type compact_result = {
      copied: int64
    ; refs_updated: int64
    ; old_size: int64
    ; new_size: int64
  }

  let compact t ?(progress_cb = fun ~percent:_ -> ()) () =
    if t.config.Config.read_only then
      Lwt.return (Error `Is_read_only)
    else
      (* We will return a cancellable task to the caller, and on cancel we will
         set the cancel_requested flag. The main compact loop will detect this
         and complete the moves already in progress before returning. *)
      let cancel_requested = ref false in

      let th, u = Lwt.task () in
      Lwt.on_cancel th (fun () ->
          Log.info (fun f -> f "cancellation of compact requested") ;
          cancel_requested := true
      ) ;
      (* Catch stray exceptions and return as unknown errors *)
      let open Lwt.Infix in
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let open Lwt_write_error.Infix in
              let open Qcow_cluster_map in
              let map = t.cluster_map in
              Log.debug (fun f -> f "Disk clusters: %s" (to_summary_string map)) ;
              let start_last_block = get_last_block map in

              let sector_size = Int64.of_int t.sector_size in
              let cluster_bits = Int32.to_int t.h.Header.cluster_bits in
              let sectors_per_cluster =
                Int64.div (1L <| cluster_bits) sector_size
              in

              let one_pass ?progress_cb () =
                Qcow_cluster_map.Debug.assert_no_leaked_blocks map ;

                let moves = Qcow_cluster_map.start_moves map in
                let open Lwt_write_error.Infix in
                Recycler.move_all ?progress_cb t.recycler moves >>= fun () ->
                (* Flush now so that if we crash after updating some of the references, the
                   destination blocks will contain the correct data. *)
                let open Lwt.Infix in
                Recycler.flush t.recycler >>= function
                | Error e ->
                    Lwt.return_error (adapt_write_error e)
                | Ok () -> (
                    let open Lwt_write_error.Infix in
                    Recycler.update_references t.recycler
                    >>= fun refs_updated ->
                    (* Flush now so that the pointers are persisted before we truncate the file *)
                    let open Lwt.Infix in
                    Recycler.flush t.recycler >>= function
                    | Error e ->
                        Lwt.return_error (adapt_write_error e)
                    | Ok () ->
                        Lwt.return (Ok refs_updated)
                  )
              in
              one_pass
                ~progress_cb:(fun ~percent ->
                  progress_cb ~percent:(percent * 80 / 100)
                )
                ()
              >>= fun refs_updated ->
              if refs_updated <> 0L then
                Log.info (fun f ->
                    f "Pass 1: %Ld references updated" refs_updated
                ) ;
              (* modifying a L2 metadata block will have cancelled the move, so
                 perform an additional pass. *)
              one_pass
                ~progress_cb:(fun ~percent ->
                  progress_cb ~percent:(80 + (percent * 4 / 100))
                )
                ()
              >>= fun refs_updated' ->
              if refs_updated' <> 0L then
                Log.info (fun f ->
                    f "Pass 2: %Ld references updated" refs_updated'
                ) ;
              one_pass () >>= fun refs_updated'' ->
              if refs_updated'' <> 0L then
                Log.err (fun f ->
                    f
                      "Failed to reach a fixed point after %Ld, %Ld and %Ld \
                       block moves"
                      refs_updated refs_updated' refs_updated''
                ) ;

              let last_block = get_last_block map in
              let open Lwt_write_error.Infix in
              ( if last_block <> start_last_block then (
                  Log.info (fun f ->
                      f "Shrink file so that last cluster was %s, now %s"
                        (Cluster.to_string start_last_block)
                        (Cluster.to_string last_block)
                  ) ;

                  let p =
                    Physical.make
                      ((Cluster.to_int last_block + 1) lsl t.cluster_bits)
                  in
                  let size_sectors =
                    Physical.sector ~sector_size:t.sector_size p
                  in
                  resize_base t.base t.sector_size
                    (Some (t.cluster_map, t.cluster_bits))
                    p
                  >>= fun () ->
                  Log.debug (fun f ->
                      f "Resized file to %s clusters (%Ld sectors)"
                        (Cluster.to_string last_block)
                        size_sectors
                  ) ;
                  Lwt.return (Ok ())
                ) else
                  Lwt.return (Ok ())
              )
              >>= fun () ->
              progress_cb ~percent:100 ;

              let total_refs_updated =
                Int64.(add (add refs_updated refs_updated') refs_updated'')
              in
              let copied = Int64.(mul total_refs_updated sectors_per_cluster) in
              (* one ref per block *)
              let old_size =
                Int64.mul
                  (Cluster.to_int64 start_last_block)
                  sectors_per_cluster
              in
              let new_size =
                Int64.mul (Cluster.to_int64 last_block) sectors_per_cluster
              in
              let report = {refs_updated; copied; old_size; new_size} in
              if copied <> 0L || total_refs_updated <> 0L then
                Log.info (fun f ->
                    f
                      "%Ld sectors copied, %Ld references updated, file shrunk \
                       by %Ld sectors"
                      copied total_refs_updated
                      (Int64.sub old_size new_size)
                ) ;
              Lwt.return (Ok report)
            )
            (fun e -> Lwt.return (Error (`Msg (Printexc.to_string e))))
          >>= fun result -> Lwt.wakeup u result ; Lwt.return_unit
      ) ;
      th

  (* If a request from the client takes more than ~30s then the client may
     decide that the storage layer has failed. This could happen if a thread
     was starved or if there's deadlock, so try to detect it and log something
     useful. *)
  let with_deadline t describe_fn nsec f =
    let open Lwt.Infix in
    let timeout =
      Time.sleep_ns nsec >>= fun () -> Lwt.return (Error `Timeout)
    in
    let work = f () in
    Lwt.choose [timeout; (work >>= fun x -> Lwt.return (Ok x))] >>= function
    | Error `Timeout ->
        Log.err (fun f -> f "%s: I/O deadline exceeded" (describe_fn ())) ;
        Locks.Debug.dump_state t.locks ;
        work (* return the answer anyway *)
    | Ok x ->
        Lwt.cancel timeout ; Lwt.return x

  let time_30s = 30_000_000_000L

  let read t sector bufs =
    let describe_fn () =
      Printf.sprintf "read sector = %Ld length = %d" sector (Cstructs.len bufs)
    in
    with_deadline t describe_fn time_30s (fun () ->
        let open Lwt_error.Infix in
        Counter.inc
          (Metrics.reads t.config.Config.id)
          (float_of_int
          @@ List.fold_left ( + ) 0
          @@ List.map Cstruct.length bufs
          ) ;
        let sectors_per_cluster = (1 lsl t.cluster_bits) / t.sector_size in
        let client = Locks.Client.make describe_fn in
        let cluster_size = 1L <| t.cluster_bits in
        let byte =
          Int64.(mul sector (of_int t.info.Mirage_block.sector_size))
        in
        Error.Lwt_error.List.map_p
          (fun (byte, buf) ->
            let vaddr = Virtual.make ~cluster_bits:t.cluster_bits byte in
            ClusterIO.walk_readonly ~client t vaddr >>= function
            | None ->
                Cstruct.memset buf 0 ;
                Lwt.return (Ok None) (* no work to do *)
            | Some (offset', l1_lock, l2_lock) ->
                let sector =
                  Physical.sector ~sector_size:t.sector_size offset'
                in
                Lwt.return
                  (Ok
                     (Some
                        {
                          sector
                        ; bufs= [buf]
                        ; metadata_locks= [l1_lock; l2_lock]
                        }
                     )
                  )
          )
          (chop_into_aligned cluster_size byte bufs)
        >>= fun work ->
        let work' =
          List.rev
          @@ List.fold_left
               (fun acc x -> match x with None -> acc | Some y -> y :: acc)
               [] work
        in
        (* work may contain contiguous items *)
        let work = coalesce_into_adjacent t.sector_size work' in
        let open Lwt.Infix in
        iter_p
          (fun work ->
            let first =
              Cluster.of_int64
                Int64.(div work.sector (of_int sectors_per_cluster))
            in
            let last_sector =
              Int64.(
                add work.sector (of_int (Cstructs.len work.bufs / t.sector_size))
              )
            in
            let last_sector' =
              Int64.(round_up last_sector (of_int sectors_per_cluster))
            in
            let last =
              Cluster.of_int64
                Int64.(div last_sector' (of_int sectors_per_cluster))
            in
            Lwt.finalize
              (fun () ->
                Locks.Read.with_locks t.locks ~first ~last (fun () ->
                    Lwt.catch
                      (fun () -> B.read t.base work.sector work.bufs)
                      (fun e ->
                        Log.err (fun f ->
                            f "%s: low-level I/O exception %s" (describe_fn ())
                              (Printexc.to_string e)
                        ) ;
                        Locks.Debug.dump_state t.locks ;
                        let cluster =
                          Cluster.of_int
                            (Int64.to_int work.sector / sectors_per_cluster)
                        in
                        Qcow_debug.check_references t.metadata t.cluster_map
                          ~cluster_bits:t.cluster_bits cluster
                        >>= fun _ ->
                        Cache.Debug.check_disk t.cache >>= fun _ -> Lwt.fail e
                      )
                )
                >>= function
                | Error e ->
                    Lwt.return_error (adapt_error e)
                | Ok () ->
                    Lwt.return (Ok ())
              )
              (fun () ->
                List.iter Locks.unlock work.metadata_locks ;
                Lwt.return_unit
              )
          )
          work
        >>= fun result ->
        Locks.Debug.assert_no_locks_held client ;
        Lwt.return result
    )

  let write t sector bufs =
    let describe_fn () =
      Printf.sprintf "write sector = %Ld length = %d" sector (Cstructs.len bufs)
    in
    if t.config.Config.read_only then
      Lwt.return (Error `Is_read_only)
    else
      with_deadline t describe_fn time_30s (fun () ->
          let open Lwt_write_error.Infix in
          Counter.inc
            (Metrics.writes t.config.Config.id)
            (float_of_int
            @@ List.fold_left ( + ) 0
            @@ List.map Cstruct.length bufs
            ) ;
          let cluster_size = 1L <| t.cluster_bits in
          let client = Locks.Client.make describe_fn in
          let sectors_per_cluster = (1 lsl t.cluster_bits) / t.sector_size in
          let byte =
            Int64.(mul sector (of_int t.info.Mirage_block.sector_size))
          in
          Error.Lwt_error.List.map_p
            (fun (byte, buf) ->
              let vaddr = Virtual.make ~cluster_bits:t.cluster_bits byte in
              ClusterIO.walk_readonly ~client t vaddr >>= function
              | None ->
                  (* Only the first write to this area needs to allocate, so it's ok
                     to make this a little slower *)
                  Lwt.catch
                    (fun () ->
                      ClusterIO.walk_and_allocate ~client t vaddr
                      >>= fun (offset', l1_lock, l2_lock) ->
                      let sector =
                        Physical.sector ~sector_size:t.sector_size offset'
                      in
                      Lwt.return
                        (Ok
                           {
                             sector
                           ; bufs= [buf]
                           ; metadata_locks= [l1_lock; l2_lock]
                           }
                        )
                    )
                    (function
                      | Error.Duplicate_reference ((c, w), (c', w'), target) as
                        e ->
                          Log.err (fun f ->
                              f "Duplicate_reference during %s" (describe_fn ())
                          ) ;
                          Qcow_debug.on_duplicate_reference t.metadata
                            t.cluster_map ~cluster_bits:t.cluster_bits (c, w)
                            (c', w') target
                          >>= fun () -> Lwt.fail e
                      | e ->
                          Lwt.fail e
                      )
              | Some (offset', l1_lock, l2_lock) ->
                  let sector =
                    Physical.sector ~sector_size:t.sector_size offset'
                  in
                  Lwt.return
                    (Ok
                       {sector; bufs= [buf]; metadata_locks= [l1_lock; l2_lock]}
                    )
            )
            (chop_into_aligned cluster_size byte bufs)
          >>= fun work' ->
          (let open Lwt.Infix in
           if !DebugSetting.compact_mid_write then (
             Log.debug (fun f -> f "DebugSetting.compact_mid_write") ;
             compact t () >>= fun _ -> Lwt.return (Ok ())
           ) else
             Lwt.return (Ok ())
          )
          >>= fun () ->
          (* work may contain contiguous items *)
          let work = coalesce_into_adjacent t.sector_size work' in
          let open Lwt.Infix in
          iter_p
            (fun work ->
              let first =
                Cluster.of_int64
                  Int64.(div work.sector (of_int sectors_per_cluster))
              in
              let last_sector =
                Int64.(
                  add work.sector
                    (of_int (Cstructs.len work.bufs / t.sector_size))
                )
              in
              let last_sector' =
                Int64.(round_up last_sector (of_int sectors_per_cluster))
              in
              let last =
                Cluster.of_int64
                  Int64.(div last_sector' (of_int sectors_per_cluster))
              in
              Locks.Write.with_locks ~client t.locks ~first ~last (fun () ->
                  (* Cancel any in-progress move since the data will be stale *)
                  let rec loop n =
                    if n > last then
                      ()
                    else (
                      Qcow_cluster_map.cancel_move t.cluster_map n ;
                      loop (Cluster.succ n)
                    )
                  in
                  loop first ;
                  Lwt.finalize
                    (fun () ->
                      Lwt.catch
                        (fun () ->
                          B.write t.base work.sector work.bufs >>= function
                          | Error e ->
                              Lwt.return_error (adapt_write_error e)
                          | Ok () ->
                              Lwt.return (Ok ())
                        )
                        (fun e ->
                          Log.err (fun f ->
                              f "%s: low-level I/O exception %s"
                                (describe_fn ()) (Printexc.to_string e)
                          ) ;
                          Locks.Debug.dump_state t.locks ;
                          let cluster =
                            Cluster.of_int
                              (Int64.to_int work.sector / sectors_per_cluster)
                          in
                          Qcow_debug.check_references t.metadata t.cluster_map
                            ~cluster_bits:t.cluster_bits cluster
                          >>= fun _ ->
                          Cache.Debug.check_disk t.cache >>= fun _ -> Lwt.fail e
                        )
                    )
                    (fun () ->
                      List.iter Locks.unlock work.metadata_locks ;
                      Lwt.return_unit
                    )
              )
            )
            work
          >>= fun result ->
          Locks.Debug.assert_no_locks_held client ;
          Lwt.return result
      )

  let seek_mapped t from =
    let open Lwt_error.Infix in
    let bytes = Int64.(mul from (of_int t.sector_size)) in
    let int64s_per_cluster = 1L <| Int32.to_int t.h.Header.cluster_bits - 3 in
    let rec scan_l1 a =
      if a.Virtual.l1_index >= Int64.of_int32 t.h.Header.l1_size then
        Lwt.return
          (Ok Int64.(mul t.info.Mirage_block.size_sectors (of_int t.sector_size))
          )
      else
        ClusterIO.find_mapped_l1_table t a.Virtual.l1_index >>= function
        | None ->
            Lwt.return
              (Ok
                 Int64.(
                   mul t.info.Mirage_block.size_sectors (of_int t.sector_size)
                 )
              )
        | Some l1_index ->
            let a = {a with Virtual.l1_index} in
            ClusterIO.read_l1_table t a.Virtual.l1_index >>= fun (x, l1_lock) ->
            Locks.unlock l1_lock ;
            if Physical.to_bytes x = 0 then
              scan_l1
                {
                  a with
                  Virtual.l1_index= Int64.succ a.Virtual.l1_index
                ; l2_index= 0L
                }
            else
              let rec scan_l2 a =
                if a.Virtual.l2_index >= int64s_per_cluster then
                  scan_l1
                    {
                      a with
                      Virtual.l1_index= Int64.succ a.Virtual.l1_index
                    ; l2_index= 0L
                    }
                else
                  ClusterIO.read_l2_table t x a.Virtual.l2_index
                  >>= fun (x, l2_lock) ->
                  Locks.unlock l2_lock ;
                  if Physical.to_bytes x = 0 then
                    scan_l2
                      {a with Virtual.l2_index= Int64.succ a.Virtual.l2_index}
                  else
                    Lwt.return
                      (Ok (Qcow_virtual.to_offset ~cluster_bits:t.cluster_bits a)
                      )
              in
              scan_l2 a
    in
    scan_l1 (Virtual.make ~cluster_bits:t.cluster_bits bytes) >>= fun offset ->
    let x = Int64.(div offset (of_int t.sector_size)) in
    assert (x >= from) ;
    Lwt.return (Ok x)

  let seek_unmapped t from =
    let open Lwt_error.Infix in
    let bytes = Int64.(mul from (of_int t.sector_size)) in
    let int64s_per_cluster = 1L <| Int32.to_int t.h.Header.cluster_bits - 3 in
    let rec scan_l1 a =
      if a.Virtual.l1_index >= Int64.of_int32 t.h.Header.l1_size then
        Lwt.return
          (Ok Int64.(mul t.info.Mirage_block.size_sectors (of_int t.sector_size))
          )
      else
        ClusterIO.read_l1_table t a.Virtual.l1_index >>= fun (x, l1_lock) ->
        Locks.unlock l1_lock ;
        if Physical.to_bytes x = 0 then
          Lwt.return (Ok (Qcow_virtual.to_offset ~cluster_bits:t.cluster_bits a))
        else
          let rec scan_l2 a =
            if a.Virtual.l2_index >= int64s_per_cluster then
              scan_l1
                {
                  a with
                  Virtual.l1_index= Int64.succ a.Virtual.l1_index
                ; l2_index= 0L
                }
            else
              ClusterIO.read_l2_table t x a.Virtual.l2_index
              >>= fun (y, l2_lock) ->
              Locks.unlock l2_lock ;
              if Physical.to_bytes y = 0 then
                Lwt.return
                  (Ok (Qcow_virtual.to_offset ~cluster_bits:t.cluster_bits a))
              else
                scan_l2 {a with Virtual.l2_index= Int64.succ a.Virtual.l2_index}
          in
          scan_l2 a
    in
    scan_l1 (Virtual.make ~cluster_bits:t.cluster_bits bytes) >>= fun offset ->
    let x = Int64.(div offset (of_int t.sector_size)) in
    assert (x >= from) ;
    Lwt.return (Ok x)

  let disconnect t = B.disconnect t.base

  let make config base h =
    let open Lwt in
    B.get_info base >>= fun base_info ->
    (* The virtual disk has 512 byte sectors *)
    let info' =
      {
        Mirage_block.read_write= false
      ; sector_size= 512
      ; size_sectors= Int64.(div h.Header.size 512L)
      }
    in
    (* We assume the backing device is resized dynamically so the
       size is the address of the next cluster *)
    let sector_size = base_info.Mirage_block.sector_size in
    let cluster_bits = Int32.to_int h.Header.cluster_bits in
    (* The first cluster is allocated after the L1 table *)
    let cluster_size = 1L <| cluster_bits in
    (* qemu-img will allocate a cluster by writing only a single sector to the end
       of the file. We insist that the file is a whole number of clusters in size. *)
    let sectors_per_cluster =
      Int64.(div (1L <| cluster_bits) (of_int sector_size))
    in
    let new_size_sectors =
      Int64.round_up base_info.Mirage_block.size_sectors sectors_per_cluster
    in
    ( if new_size_sectors > base_info.Mirage_block.size_sectors then (
        Log.info (fun f ->
            f "rounding up file to a whole number of clusters (= %Ld sectors)"
              new_size_sectors
        ) ;
        B.resize base new_size_sectors >>= function
        | Error _ ->
            Lwt.fail_with "resizing file"
        | Ok () ->
            Lwt.return_unit
      ) else
        Lwt.return_unit
    )
    >>= fun () ->
    let locks = Locks.make () in
    let read_cluster i =
      let buf = malloc h in
      let cluster = Cluster.to_int64 i in
      let offset = cluster <| cluster_bits in
      let sector = Int64.(div offset (of_int sector_size)) in
      let open Lwt.Infix in
      Lwt.catch
        (fun () ->
          B.read base sector [buf] >>= function
          | Error _ ->
              Lwt.fail_with "unknown error"
          | Ok () ->
              Lwt.return (Ok buf)
        )
        (fun e ->
          Log.err (fun f ->
              f "read_cluster %Ld: low-level I/O exception %s" cluster
                (Printexc.to_string e)
          ) ;
          Locks.Debug.dump_state locks ;
          Lwt.fail e
        )
    in
    let write_cluster i buf =
      if config.Config.read_only then
        Lwt.return (Error `Is_read_only)
      else
        let cluster = Cluster.to_int64 i in
        let offset = cluster <| cluster_bits in
        let sector = Int64.(div offset (of_int sector_size)) in
        Lwt.catch
          (fun () ->
            B.write base sector [buf] >>= function
            | Error `Disconnected ->
                Lwt.return (Error `Disconnected)
            | Error `Is_read_only ->
                Lwt.return (Error `Is_read_only)
            | Error _ ->
                Lwt.fail_with "unknown error"
            | Ok () ->
                Lwt.return (Ok ())
          )
          (fun e ->
            Log.err (fun f ->
                f "write_cluster %Ld: low-level I/O exception %s" cluster
                  (Printexc.to_string e)
            ) ;
            Locks.Debug.dump_state locks ;
            Lwt.fail e
          )
    in
    let cache = Cache.create ~read_cluster ~write_cluster () in
    let metadata = Metadata.make ~cache ~cluster_bits ~locks () in
    let recycler =
      Recycler.create ~base ~sector_size ~cluster_bits ~cache ~locks ~metadata
        ~runtime_asserts:config.Config.runtime_asserts
    in
    let lazy_refcounts =
      match h.Header.additional with
      | Some {Header.lazy_refcounts= true; _} ->
          true
      | _ ->
          false
    in
    let stats = Stats.zero in
    let cluster_map = Qcow_cluster_map.zero in
    let cluster_map_m = Lwt_mutex.create () in
    let t' =
      {
        h
      ; base
      ; info= info'
      ; config
      ; locks
      ; recycler
      ; metadata
      ; cache
      ; sector_size
      ; cluster_bits
      ; lazy_refcounts
      ; stats
      ; cluster_map
      ; cluster_map_m
      }
    in
    Lwt_error.or_fail_with @@ make_cluster_map t' ~id:config.Config.id ()
    >>= fun cluster_map ->
    if config.Config.runtime_asserts then
      Qcow_cluster_map.Debug.assert_equal cluster_map cluster_map ;
    (* An opened file may have junk at the end, which means that we would simultaneously
       allocate from it (get_last_block + n) as well as erase and recycle it.
       We should trim the file now so that it is safe to allocate from it as normal.
       Normally when the file is expanded the blocks at the end are not considered to be
       junk. *)
    let last_block = Qcow_cluster_map.get_last_block cluster_map in
    let size_clusters = Cluster.succ last_block in
    let p = Physical.make (Cluster.to_int size_clusters lsl cluster_bits) in
    let size_sectors = Physical.sector ~sector_size p in
    ( if config.Config.read_only then
        Lwt.return_unit
      else
        Lwt_write_error.or_fail_with @@ resize_base base sector_size None p
        >>= fun () ->
        Log.info (fun f ->
            f "Resized file to %s clusters (%Ld sectors)"
              (Cluster.to_string size_clusters)
              size_sectors
        ) ;
        Qcow_cluster_map.resize cluster_map size_clusters ;
        Lwt.return_unit
    )
    >>= fun () ->
    t'.cluster_map <- cluster_map ;
    Metadata.set_cluster_map t'.metadata cluster_map ;
    Recycler.set_cluster_map t'.recycler cluster_map ;
    if config.Config.read_only then
      Lwt.return t'
    else (
      ( match config.Config.keep_erased with
      | None ->
          ()
      | Some sectors ->
          let keep_erased =
            Int64.(div (mul sectors (of_int sector_size)) cluster_size)
          in
          let compact_after_unmaps =
            match config.Config.compact_after_unmaps with
            | None ->
                None
            | Some sectors ->
                Some Int64.(div (mul sectors (of_int sector_size)) cluster_size)
          in
          Recycler.start_background_thread t'.recycler ~keep_erased
            ?compact_after_unmaps ()
      ) ;
      ( if config.Config.discard && not lazy_refcounts then (
          Log.info (fun f ->
              f
                "discard requested and lazy_refcounts is disabled: erasing \
                 refcount table and enabling lazy_refcounts"
          ) ;
          Lwt_error.or_fail_with @@ ClusterIO.Refcount.zero_all t' >>= fun () ->
          let additional =
            match h.Header.additional with
            | Some h ->
                {h with Header.lazy_refcounts= true}
            | None ->
                {
                  Header.dirty= true
                ; corrupt= false
                ; lazy_refcounts= true
                ; autoclear_features= 0L
                ; refcount_order= 4l
                }
          in
          let extensions = [`Feature_name_table Header.Feature.understood] in
          let h = {h with Header.additional= Some additional; extensions} in
          Lwt_write_error.or_fail_with @@ update_header t' h >>= fun () ->
          t'.lazy_refcounts <- true ;
          Lwt.return_unit
        ) else
          Lwt.return_unit
      )
      >>= fun () ->
      Recycler.flush t'.recycler >>= function
      | Error _ ->
          Log.err (fun f -> f "initial flush failed") ;
          Lwt.fail (Failure "initial flush failed")
      | Ok () ->
          Lwt.return t'
    )

  let connect ?(config = Config.default ()) base =
    let open Lwt.Infix in
    B.get_info base >>= fun base_info ->
    let sector =
      Cstruct.sub
        Io_page.(to_cstruct (get 1))
        0 base_info.Mirage_block.sector_size
    in
    B.read base 0L [sector] >>= function
    | Error e ->
        Format.kasprintf Lwt.fail_with "%a" B.pp_error e
    | Ok () -> (
      match Header.read sector with
      | Error (`Msg m) ->
          Lwt.fail_with m
      | Ok (h, _) ->
          make config base h >>= fun t ->
          let open Qcow_cluster_map in
          let free = total_free t.cluster_map in
          let used = total_used t.cluster_map in
          Log.info (fun f ->
              f "image has %Ld free sectors and %Ld used sectors" free used
          ) ;
          Lwt.return t
    )

  let check base =
    let open Lwt.Infix in
    let open Qcow_cluster_map in
    Lwt.catch
      (fun () ->
        let config = Config.create ~read_only:true () in
        connect ~config base >>= fun t ->
        let free = total_free t.cluster_map in
        let used = total_used t.cluster_map in
        Lwt.return (Ok {free; used})
      )
      (function
        | Reference_outside_file (src, dst) ->
            Lwt.return (Error (`Reference_outside_file (src, dst)))
        | Error.Duplicate_reference ((c, w), (c', w'), dst) ->
            Lwt.return (Error (`Duplicate_reference ((c, w), (c', w'), dst)))
        | e ->
            Lwt.fail e
        )

  let resize t ~new_size:requested_size_bytes ?(ignore_data_loss = false) () =
    if t.config.Config.read_only then
      Lwt.return (Error `Is_read_only)
    else
      let existing_size = t.h.Header.size in
      if existing_size > requested_size_bytes && not ignore_data_loss then
        Lwt.return
          (Error
             (`Msg
               (Printf.sprintf
                  "Requested resize would result in data loss: requested size \
                   = %Ld but current size = %Ld"
                  requested_size_bytes existing_size
               )
               )
          )
      else
        let size = Int64.round_up requested_size_bytes 512L in
        let l2_tables_required =
          Header.l2_tables_required ~cluster_bits:t.cluster_bits size
        in
        (* Keep it simple for now by refusing resizes which would require us to
           reallocate the L1 table. *)
        let l2_entries_per_cluster =
          1L <| Int32.to_int t.h.Header.cluster_bits - 3
        in
        let old_max_entries =
          Int64.round_up
            (Int64.of_int32 t.h.Header.l1_size)
            l2_entries_per_cluster
        in
        let new_max_entries =
          Int64.round_up l2_tables_required l2_entries_per_cluster
        in
        if new_max_entries > old_max_entries then
          Lwt.return
            (Error
               (`Msg
                 "I don't know how to resize in the case where the L1 table \
                  needs new clusters:"
                 )
            )
        else
          update_header t
            {t.h with Header.l1_size= Int64.to_int32 l2_tables_required; size}

  let zero =
    let page = Io_page.(to_cstruct (get 1)) in
    Cstruct.memset page 0 ; page

  let rec erase t ~sector ~n () =
    let open Lwt_write_error.Infix in
    if n <= 0L then
      Lwt.return (Ok ())
    else
      (* This could walk one cluster at a time instead of one sector at a time *)
      let byte = Int64.(mul sector (of_int t.info.Mirage_block.sector_size)) in
      let vaddr = Virtual.make ~cluster_bits:t.cluster_bits byte in
      (ClusterIO.walk_readonly t vaddr >>= function
       | None ->
           (* Already zero, nothing to do *)
           Lwt.return (Ok ())
       | Some (offset', l1_lock, l2_lock) ->
           Lwt.finalize
             (fun () ->
               let base_sector, _ =
                 Physical.to_sector ~sector_size:t.sector_size offset'
               in
               t.stats.Stats.nr_erased <- Int64.succ t.stats.Stats.nr_erased ;
               let open Lwt.Infix in
               B.write t.base base_sector
                 [Cstruct.sub zero 0 t.info.Mirage_block.sector_size]
               >>= adapt_write_error_result
             )
             (fun () ->
               Locks.unlock l1_lock ; Locks.unlock l2_lock ; Lwt.return_unit
             )
      )
      >>= fun () -> erase t ~sector:(Int64.succ sector) ~n:(Int64.pred n) ()

  let discard t ~sector ~n () =
    let describe_fn () = Printf.sprintf "discard sector %Ld n %Ld" sector n in
    with_deadline t describe_fn time_30s (fun () ->
        let open Lwt_write_error.Infix in
        ( if not t.config.Config.discard then (
            Log.err (fun f ->
                f "discard called but feature not implemented in configuration"
            ) ;
            Lwt.fail (Failure "Unimplemented")
          ) else
            Lwt.return (Ok ())
        )
        >>= fun () ->
        Counter.inc
          (Metrics.discards t.config.Config.id)
          Int64.(to_float @@ mul n @@ of_int t.sector_size) ;
        let client = Locks.Client.make describe_fn in

        (* we can only discard whole clusters. We will explicitly zero non-cluster
           aligned discards in order to satisfy RZAT *)

        (* round sector, n up to a cluster boundary *)
        let sectors_per_cluster =
          Int64.(div (1L <| t.cluster_bits) (of_int t.sector_size))
        in
        let sector' = Int64.round_up sector sectors_per_cluster in

        (* we can only discard whole clusters. We will explicitly zero non-cluster
           aligned discards in order to satisfy RZAT *)
        let to_erase = min n (Int64.sub sector' sector) in
        erase t ~sector ~n:to_erase () >>= fun () ->
        let n' = Int64.sub n to_erase in

        let to_discard = Int64.round_down n' sectors_per_cluster in
        ClusterIO.walk_and_deallocate ~client t sector' to_discard >>= fun () ->
        erase t
          ~sector:(Int64.add sector' to_discard)
          ~n:(Int64.sub n' to_discard) ()
    )

  let create base ~size ?(lazy_refcounts = true) ?(cluster_bits = 16)
      ?(config = Config.default ()) () =
    let version = `Three in
    let backing_file_offset = 0L in
    let backing_file_size = 0l in
    let cluster_size = 1 lsl cluster_bits in
    let crypt_method = `None in
    (* qemu-img places the refcount table next in the file and only
       qemu-img creates a tiny refcount table and grows it on demand *)
    let refcount_table_offset = Physical.make cluster_size in
    let refcount_table_clusters = 1 in

    (* qemu-img places the L1 table after the refcount table *)
    let l1_table_offset =
      Physical.make ((refcount_table_clusters + 1) lsl cluster_bits)
    in
    let l2_tables_required = Header.l2_tables_required ~cluster_bits size in
    let nb_snapshots = 0l in
    let snapshots_offset = 0L in
    let additional =
      Some
        {
          Header.dirty= lazy_refcounts
        ; corrupt= false
        ; lazy_refcounts
        ; autoclear_features= 0L
        ; refcount_order= 4l
        }
    in
    let extensions = [`Feature_name_table Header.Feature.understood] in
    let h =
      {
        Header.version
      ; backing_file_offset
      ; backing_file_size
      ; cluster_bits= Int32.of_int cluster_bits
      ; size
      ; crypt_method
      ; l1_size= Int64.to_int32 l2_tables_required
      ; l1_table_offset
      ; refcount_table_offset
      ; refcount_table_clusters= Int32.of_int refcount_table_clusters
      ; nb_snapshots
      ; snapshots_offset
      ; additional
      ; extensions
      }
    in
    (* Resize the underlying device to contain the header + refcount table
       + l1 table. Future allocations will enlarge the file. *)
    let l1_size_bytes = 8 * Int64.to_int l2_tables_required in
    let next_free_byte =
      Int.round_up
        (Physical.to_bytes l1_table_offset + l1_size_bytes)
        cluster_size
    in
    let open Lwt in
    B.get_info base >>= fun base_info ->
    (* Erase existing contents *)
    let open Lwt_write_error.Infix in
    resize_base base base_info.Mirage_block.sector_size None (Physical.make 0)
    >>= fun () ->
    let p = Physical.make next_free_byte in
    resize_base base base_info.Mirage_block.sector_size None p >>= fun () ->
    let open Lwt.Infix in
    make config base h >>= fun t ->
    let open Lwt_write_error.Infix in
    update_header t h >>= fun () ->
    (* Write an initial empty refcount table *)
    let cluster = malloc t.h in
    Cstruct.memset cluster 0 ;
    let open Lwt.Infix in
    B.write base
      (Physical.sector ~sector_size:t.sector_size refcount_table_offset)
      [cluster]
    >>= function
    | Error e ->
        Lwt.return_error (adapt_write_error e)
    | Ok () -> (
        let open Lwt_write_error.Infix in
        let next_cluster = next_free_byte / cluster_size in
        let rec loop limit i =
          if i = limit then
            Lwt.return (Ok ())
          else
            ClusterIO.Refcount.incr t (Cluster.of_int i) >>= fun () ->
            loop limit (i + 1)
        in
        (* Increase the refcount of all header clusters i.e. those < next_free_cluster *)
        loop next_cluster 0 >>= fun () ->
        (* Write an initial empty L1 table *)
        let open Lwt.Infix in
        B.write base
          (Physical.sector ~sector_size:t.sector_size l1_table_offset)
          [cluster]
        >>= function
        | Error e ->
            Lwt.return_error (adapt_write_error e)
        | Ok () -> (
            Recycler.flush t.recycler >>= function
            | Error e ->
                Lwt.return_error (adapt_write_error e)
            | Ok () ->
                Lwt.return (Ok t)
          )
      )

  let rebuild_refcount_table t =
    let open Lwt_write_error.Infix in
    let client = Locks.Client.make (fun () -> "rebuild_refcount_table") in
    (* Disable lazy refcounts so we actually update the real refcounts *)
    let lazy_refcounts = t.lazy_refcounts in
    t.lazy_refcounts <- false ;
    Log.info (fun f -> f "Zeroing existing refcount table") ;
    ClusterIO.Refcount.zero_all ~client t >>= fun () ->
    let cluster =
      Physical.cluster ~cluster_bits:t.cluster_bits
        t.h.Header.refcount_table_offset
    in
    let refcount_table_clusters =
      Int32.to_int t.h.Header.refcount_table_clusters
    in
    let rec loop i =
      if i >= refcount_table_clusters then
        Lwt.return (Ok ())
      else
        ClusterIO.Refcount.incr ~client t Cluster.(add cluster (of_int i))
        >>= fun () ->
        (* If any of the table entries point to a block, increase its refcount too *)
        Metadata.read ~client t.metadata
          Cluster.(add cluster (of_int i))
          (fun c ->
            let addresses = Metadata.Physical.of_contents c in
            Lwt.return (Ok addresses)
          )
        >>= fun addresses ->
        let rec inner i =
          if i >= Metadata.Physical.len addresses then
            Lwt.return (Ok ())
          else
            let addr = Metadata.Physical.get addresses i in
            ( if addr <> Physical.unmapped then (
                let cluster' =
                  Physical.cluster ~cluster_bits:t.cluster_bits addr
                in
                Log.debug (fun f ->
                    f "Refcount cluster %s has reference to cluster %s"
                      (Cluster.to_string cluster)
                      (Cluster.to_string cluster')
                ) ;
                (* It might have been incremented already by a previous `incr` *)
                ClusterIO.Refcount.read ~client t cluster' >>= function
                | 0 ->
                    ClusterIO.Refcount.incr ~client t cluster'
                | _ ->
                    Lwt.return (Ok ())
              ) else
                Lwt.return (Ok ())
            )
            >>= fun () -> inner (i + 1)
        in
        inner 0 >>= fun () -> loop (i + 1)
    in
    Log.info (fun f -> f "Incrementing refcount of the refcount table clusters") ;
    loop 0 >>= fun () ->
    (* Increment the refcount of the header and L1 table *)
    Log.info (fun f -> f "Incrementing refcount of the header") ;
    ClusterIO.Refcount.incr ~client t Cluster.zero >>= fun () ->
    let l1_table_clusters =
      let refs_per_cluster = 1L <| t.cluster_bits - 3 in
      Int64.(
        to_int
        @@ div
             (round_up (of_int32 t.h.Header.l1_size) refs_per_cluster)
             refs_per_cluster
      )
    in
    let l1_table_cluster =
      Physical.cluster ~cluster_bits:t.cluster_bits t.h.Header.l1_table_offset
    in
    let rec loop i =
      if i >= l1_table_clusters then
        Lwt.return (Ok ())
      else
        ClusterIO.Refcount.incr ~client t
          Cluster.(add l1_table_cluster (of_int i))
        >>= fun () ->
        (* Increment clusters of L1 tables *)
        Metadata.read ~client t.metadata
          Cluster.(add l1_table_cluster (of_int i))
          (fun c ->
            let addresses = Metadata.Physical.of_contents c in
            Lwt.return (Ok addresses)
          )
        >>= fun addresses ->
        let rec inner i =
          if i >= Metadata.Physical.len addresses then
            Lwt.return (Ok ())
          else
            let addr = Metadata.Physical.get addresses i in
            ( if addr <> Physical.unmapped then (
                let cluster' =
                  Physical.cluster ~cluster_bits:t.cluster_bits addr
                in
                Log.debug (fun f ->
                    f "L1 cluster %s has reference to L2 cluster %s"
                      (Cluster.to_string cluster)
                      (Cluster.to_string cluster')
                ) ;
                ClusterIO.Refcount.incr ~client t cluster'
              ) else
                Lwt.return (Ok ())
            )
            >>= fun () -> inner (i + 1)
        in
        inner 0 >>= fun () -> loop (i + 1)
    in
    Log.info (fun f ->
        f "Incrementing refcount of the %Ls L1 table clusters starting at %s"
          l1_table_clusters
          (Cluster.to_string l1_table_cluster)
    ) ;
    loop 0 >>= fun () ->
    (* Fold over the mapped data, incrementing refcounts along the way *)
    let sectors_per_cluster =
      Int64.(div (1L <| t.cluster_bits) (of_int t.sector_size))
    in
    let rec loop sector =
      if sector >= t.info.Mirage_block.size_sectors then
        Lwt.return (Ok ())
      else
        seek_mapped t sector >>= fun mapped_sector ->
        if mapped_sector <> sector then
          loop mapped_sector
        else
          ClusterIO.walk_readonly ~client t
            (Virtual.make ~cluster_bits:t.cluster_bits
               Int64.(mul (of_int t.info.Mirage_block.sector_size) mapped_sector)
            )
          >>= function
          | None ->
              assert false
          | Some (offset', l1_lock, l2_lock) ->
              Locks.unlock l1_lock ;
              Locks.unlock l2_lock ;
              let cluster =
                Physical.cluster ~cluster_bits:t.cluster_bits offset'
              in
              ClusterIO.Refcount.incr ~client t cluster >>= fun () ->
              loop (Int64.add mapped_sector sectors_per_cluster)
    in
    Log.info (fun f -> f "Incrementing refcount of the data clusters") ;
    loop 0L >>= fun () ->
    (* Restore the original lazy_refcount setting *)
    t.lazy_refcounts <- lazy_refcounts ;
    Lwt.return (Ok ())

  let flush t =
    let open Lwt.Infix in
    Recycler.flush t.recycler >>= adapt_write_error_result

  let header t = t.h

  module Debug = struct
    let check_no_overlaps t =
      let within =
        Physical.within_cluster ~cluster_bits:t.cluster_bits
          t.h.Header.l1_table_offset
      in
      assert (within = 0) ;
      let within =
        Physical.within_cluster ~cluster_bits:t.cluster_bits
          t.h.Header.refcount_table_offset
      in
      assert (within = 0) ;
      Lwt.return (Ok ())

    let assert_no_leaked_blocks t =
      Qcow_cluster_map.Debug.assert_no_leaked_blocks t.cluster_map

    let assert_cluster_map_in_sync t =
      let open Lwt.Infix in
      Lwt_error.or_fail_with @@ make_cluster_map t () >>= fun cluster_map ->
      Qcow_cluster_map.Debug.assert_equal cluster_map t.cluster_map ;
      Lwt.return_unit

    module Setting = DebugSetting

    let metadata_blocks t =
      let clusters = Qcow_cluster_map.Debug.metadata_blocks t.cluster_map in
      Qcow_types.Cluster.(
        IntervalSet.(
          fold (fun i acc ->
              let x, y =
                Interval.
                  ( to_int64 (x i) <| t.cluster_bits
                  , (* the last inclusive byte = next cluster start - 1 *)
                    Int64.pred (to_int64 (succ @@ y i) <| t.cluster_bits)
                  )
              in
              Qcow_types.Int64.IntervalSet.(add (Interval.make x y) acc)
          )
        )
      )
        clusters Qcow_types.Int64.IntervalSet.empty
  end
end
