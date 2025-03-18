(* Securely erase and then recycle clusters *)

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

open Qcow_types

let ( <| ) = Int64.shift_left
let ( |> ) = Int64.shift_right

module Cache = Qcow_cache
module Error = Qcow_error
module Locks = Qcow_locks
module Metadata = Qcow_metadata
module Physical = Qcow_physical

module Make(B: Qcow_s.RESIZABLE_BLOCK)(Time: Mirage_time.S) = struct

  type t = {
    base: B.t;
    sector_size: int;
    cluster_bits: int;
    mutable cluster_map: Qcow_cluster_map.t option; (* free/ used space map *)
    cache: Cache.t;
    locks: Locks.t;
    metadata: Metadata.t;
    zero_buffer: Cstruct.t;
    mutable background_thread: unit Lwt.t;
    mutable need_to_flush: bool;
    need_to_flush_c: unit Lwt_condition.t;
    flush_m: Lwt_mutex.t;
    runtime_asserts: bool;
  }

  let create ~base ~sector_size ~cluster_bits ~cache ~locks ~metadata ~runtime_asserts =
    let zero_buffer = Io_page.(to_cstruct @@ get 256) in (* 1 MiB *)
    Cstruct.memset zero_buffer 0;
    let background_thread = Lwt.return_unit in
    let flush_m = Lwt_mutex.create () in
    let cluster_map = None in
    let need_to_flush = false in
    let need_to_flush_c = Lwt_condition.create () in
    { base; sector_size; cluster_bits; cluster_map; cache; locks; metadata;
      zero_buffer; background_thread; need_to_flush; need_to_flush_c;
      flush_m; runtime_asserts; }

  let set_cluster_map t cluster_map = t.cluster_map <- Some cluster_map

  let allocate t n =
    let cluster_map = match t.cluster_map with
      | Some x -> x
      | None -> assert false in
    match Cluster.IntervalSet.take (Qcow_cluster_map.Available.get cluster_map) n with
    | Some (set, _free) ->
      Log.debug (fun f -> f "Allocated %s clusters from free list: %s"
        (Cluster.to_string n) (Sexplib.Sexp.to_string_hum ~indent:2 @@ Cluster.IntervalSet.sexp_of_t set)
      );
      Qcow_cluster_map.(set_cluster_state cluster_map set Available Roots);
      Some set
    | None ->
      None

  let copy_already_locked t src dst =
    let src = Cluster.to_int64 src and dst = Cluster.to_int64 dst in
    let cluster_map = match t.cluster_map with
      | Some x -> x
      | None -> assert false in
    Log.debug (fun f -> f "Copy cluster %Ld to %Ld" src dst);
    let npages = 1 lsl (t.cluster_bits - 12) in
    let pages = Io_page.(to_cstruct @@ get npages) in
    let cluster = Cstruct.sub pages 0 (1 lsl t.cluster_bits) in

    let sectors_per_cluster = Int64.(div (1L <| t.cluster_bits) (of_int t.sector_size)) in

    let src_sector = Int64.mul src sectors_per_cluster in
    let dst_sector = Int64.mul dst sectors_per_cluster in
    let open Lwt.Infix in
    B.read t.base src_sector [ cluster ]
    >>= function
    | Error `Disconnected -> Lwt.return (Error `Disconnected)
    | Error e -> Format.kasprintf Lwt.fail_with "Unknown error: %a" B.pp_error e
    | Ok () ->
      B.write t.base dst_sector [ cluster ]
      >>= function
      | Error `Disconnected -> Lwt.return (Error `Disconnected)
      | Error `Is_read_only -> Lwt.return (Error `Is_read_only)
      | Error e -> Format.kasprintf Lwt.fail_with "Unknown error: %a" B.pp_write_error e
      | Ok () ->
        let dst' = Cluster.of_int64 dst in
        Cache.Debug.assert_not_cached t.cache dst';
        if not @@ Qcow_cluster_map.Copies.mem cluster_map dst' then begin
          Log.err (fun f -> f "Copy cluster %Ld to %Ld: but %Ld is not Junk" src dst dst);
          Qcow_cluster_map.Debug.assert_no_leaked_blocks cluster_map;
          assert false
        end;
        if Qcow_cluster_map.is_moving cluster_map dst' then begin
          Log.err (fun f -> f "Copy cluster from %Ld to %Ld: but %Ld is also moving" src dst dst);
          Qcow_cluster_map.Debug.assert_no_leaked_blocks cluster_map;
          assert false
        end;
        Lwt.return (Ok ())

  let copy t src dst =
    Locks.Read.with_lock t.locks src
      (fun () ->
         Locks.Write.with_lock t.locks dst
           (fun () ->
             copy_already_locked t src dst
           )
      )

  let move t move =
    let cluster_map = match t.cluster_map with
      | Some x -> x
      | None -> assert false in
    let src, dst = Qcow_cluster_map.Move.(move.src, move.dst) in
    Log.debug (fun f -> f "move %s -> %s" (Cluster.to_string src) (Cluster.to_string dst));
    let open Lwt.Infix in
    Locks.Read.with_lock t.locks src
      (fun () ->
         Locks.Write.with_lock t.locks dst
           (fun () ->
             (* Consider that a discard might have arrived and removed the src
                cluster. *)
             if not(Qcow_cluster_map.is_moving cluster_map src) then begin
               Log.info (fun f -> f "Copy of cluster %s prevented: move operation cancelled" (Cluster.to_string src));
               Lwt.return (Ok ())
             end else begin
               copy_already_locked t src dst
               >>= function
               | Error `Disconnected -> Lwt.return (Error `Disconnected)
               | Error `Is_read_only -> Lwt.return (Error `Is_read_only)
               | Error _ -> Format.kasprintf Lwt.fail_with "Unknown error in qcow_recylcer.ml"
               | Ok () ->
                 Qcow_cluster_map.(set_move_state cluster_map move Copied);
                 Lwt.return (Ok ())
             end
            )
      )

  let move_all ?(progress_cb = fun ~percent:_ -> ()) t moves =
    let total = List.length moves in
    let rec loop i = function
      | [] -> Lwt.return (Ok ())
      | m :: ms ->
        let open Lwt.Infix in
        move t m
        >>= function
        | Error e -> Lwt.return_error e
        | Ok () ->
          progress_cb ~percent:((100 * i) / total);
          loop (i + 1) ms in
    loop 0 moves

  let erase t remaining =
    let open Lwt.Infix in
    let intervals = Cluster.IntervalSet.fold (fun i acc -> i :: acc) remaining [] in
    let buffer_size_clusters = Int64.of_int (Cstruct.length t.zero_buffer) |> t.cluster_bits in

    Lwt_list.fold_left_s
      (fun acc i -> match acc with
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          let x, y = Cluster.IntervalSet.Interval.(x i, y i) in
          let x = Cluster.to_int64 x and y = Cluster.to_int64 y in
          let n = Int64.(succ @@ sub y x) in
          Log.debug (fun f -> f "erasing %Ld clusters (%Ld -> %Ld)" n x y);
          let erase cluster n =
            (* Erase [n] clusters starting from [cluster] *)
            assert (n <= buffer_size_clusters);
            let buf = Cstruct.sub t.zero_buffer 0 (Int64.to_int (n <| t.cluster_bits)) in
            let sector = Int64.(div (cluster <| t.cluster_bits) (of_int t.sector_size)) in
            (* No-one else is writing to this cluster so no locking is needed *)
            B.write t.base sector [ buf ] in
          let rec loop from n m =
            if n = 0L then Lwt.return (Ok ())
            else if n > m then begin
              erase from m
              >>= function
              | Error e -> Lwt.return (Error e)
              | Ok () -> loop (Int64.add from m) (Int64.sub n m) m
            end else begin
              erase from n
            end in
          loop x n buffer_size_clusters
      ) (Ok ()) intervals

  let update_references t =
    let cluster_map = match t.cluster_map with
      | None -> assert false (* by construction, see `make` *)
      | Some x -> x in
    let open Qcow_cluster_map in

    (* Build a list of moves per referring cluster, so we can take the referring
       cluster lock once, make all the updates and release it. *)
    let flushed' =
      Cluster.Map.fold (fun src move acc ->
        assert (src = move.Qcow_cluster_map.move.Qcow_cluster_map.Move.src);
        match move.state with
        | Flushed ->
          begin match Qcow_cluster_map.find cluster_map src with
          | exception Not_found -> acc
          | ref_cluster, _ ->
            let existing =
              if Cluster.Map.mem ref_cluster acc
              then Cluster.Map.find ref_cluster acc
              else [] in
            Cluster.Map.add ref_cluster (move :: existing) acc
          end
        | _ -> acc
      ) (moves cluster_map) Cluster.Map.empty in
    let flushed = Cluster.Map.bindings flushed' in

    let nr_updated = ref 0L in
    let open Lwt.Infix in
    (* If I can't acquire a write lock on the metadata cluster then skip
       this update and do it later. *)
    let client = Locks.Client.make
      (fun () -> "Rewriting references after a block copy") in
    Lwt_list.fold_left_s
      (fun acc (ref_cluster', moves) -> match acc with
        | Error e -> Lwt.return (Error e)
        | Ok subst ->
          let ref_cluster = try Cluster.Map.find ref_cluster' subst with Not_found -> ref_cluster' in
          begin match Locks.Write.try_lock ~client t.locks ref_cluster with
          | None ->
            List.iter (fun ({ move = { Move.src; dst }; _ }) ->
              Log.debug (fun f -> f "Not rewriting reference in %s from %s to %s: metadata cluster is locked"
                (Cluster.to_string ref_cluster)
                (Cluster.to_string src) (Cluster.to_string dst)
              );
              cancel_move cluster_map src
            ) moves;
            Lwt.return (Ok subst)
          | Some lock ->
            Lwt.finalize
              (fun () ->
                (* The flush function will call complete move for all moves with state Referenced.
                   However these won't actually have hit the disk until Metadata.update returns
                   and the disk write has been performed. *)
                Lwt_mutex.with_lock t.flush_m
                (fun () ->
                Metadata.update ~client t.metadata ref_cluster
                  (fun c ->
                    Log.info (fun f -> f "Updating %d references in cluster %s" (List.length moves) (Cluster.to_string ref_cluster));
                    let addresses = Metadata.Physical.of_contents c in
                    try
                      let result = List.fold_left
                        (fun acc ({ move = { Move.src; dst }; _ } as move) -> match acc with
                          | Error e -> Error e
                          | Ok subst ->
                            begin match Qcow_cluster_map.find cluster_map src with
                            | exception Not_found ->
                              (* Block was probably discarded after we started running. *)
                              Log.warn (fun f -> f "Not copying cluster %s to %s: %s has been discarded"
                                (Cluster.to_string src) (Cluster.to_string dst) (Cluster.to_string src)
                              );
                              Ok subst
                            | ref_cluster', ref_cluster_within ->
                              if ref_cluster' <> ref_cluster then begin
                                Log.info (fun f -> f "Reference to %s moved from %s:%d to %s:%d"
                                  (Cluster.to_string src) (Cluster.to_string ref_cluster) ref_cluster_within
                                  (Cluster.to_string ref_cluster') ref_cluster_within
                                );
                                Ok subst
                              end else if not(Cluster.Map.mem src (Qcow_cluster_map.moves cluster_map)) then begin
                                Log.debug (fun f -> f "Not rewriting reference in %s :%d from %s to %s: move as been cancelled"
                                  (Cluster.to_string ref_cluster) ref_cluster_within
                                  (Cluster.to_string src) (Cluster.to_string dst)
                                );
                                Ok subst
                              end else begin
                                (* Read the current value in the referencing cluster as a sanity check *)
                                let old_reference = Metadata.Physical.get addresses ref_cluster_within in
                                let old_cluster = Qcow_physical.cluster ~cluster_bits:t.cluster_bits old_reference in
                                if old_cluster <> src then begin
                                  Log.err (fun f -> f "Rewriting reference in %s :%d from %s to %s, old reference actually pointing to %s"
                                    (Cluster.to_string ref_cluster) ref_cluster_within
                                    (Cluster.to_string src) (Cluster.to_string dst)
                                    (Cluster.to_string old_cluster)
                                  );
                                  assert false
                                end;
                                Log.debug (fun f -> f "Rewriting reference in %s :%d from %s to %s"
                                  (Cluster.to_string ref_cluster) ref_cluster_within
                                  (Cluster.to_string src) (Cluster.to_string dst)
                                );
                                (* Preserve any flags but update the pointer *)
                                let dst' = Cluster.to_int dst lsl t.cluster_bits in
                                let new_reference = Qcow_physical.make ~is_mutable:(Qcow_physical.is_mutable old_reference) ~is_compressed:(Qcow_physical.is_compressed old_reference) dst' in
                                set_move_state cluster_map move.move Referenced;
                                Metadata.Physical.set addresses ref_cluster_within new_reference;
                                nr_updated := Int64.succ !nr_updated;
                                (* The move cannot be cancelled now that the metadata has
                                   been updated. *)
                                Ok (Cluster.Map.add src dst subst)
                            end
                          end
                      ) (Ok subst) moves in
                      match result with
                      | Error e -> Lwt.return (Error e)
                      | Ok subst ->
                        (* If `ref_cluster` is an L1 table entry then `src` must be an
                           L2 block, and the values in `cluster_map.refs` will point to it.
                           These need to be redirected to `dst` otherwise the `cluster_map`
                           will be out-of-sync. This only happens because we bypass the
                           `Metadata.Physical.set` function in the block copier. *)
                        if Qcow_cluster_map.is_immovable cluster_map ref_cluster then begin
                          Log.info (fun f -> f "Cluster %s is L1: we must remap L2 references" (Cluster.to_string ref_cluster));
                          Qcow_cluster_map.update_references cluster_map subst
                        end;
                        Lwt.return (Ok subst)
                    with Error.Duplicate_reference((c, w), (c', w'), (target: int64)) as e ->
                      Log.err (fun f -> f "Duplicate_reference during update_references of %s"
                        (String.concat ", " @@ List.map Qcow_cluster_map.string_of_move @@ List.concat @@ List.map snd flushed)
                      );
                      let open Error.Lwt_write_error.Infix in
                      Qcow_debug.on_duplicate_reference t.metadata cluster_map ~cluster_bits:t.cluster_bits (c, w) (c', w') target
                      >>= fun () ->
                      Qcow_cluster_map.Debug.assert_no_leaked_blocks cluster_map;
                      Lwt.fail e
                    | e ->
                      Qcow_cluster_map.Debug.assert_no_leaked_blocks cluster_map;
                      raise e
                  )
                  )
              ) (fun () ->
                Locks.unlock lock;
                Lwt.return_unit
              )
          end
        ) (Ok Cluster.Map.empty) flushed
    >>= function
    | Ok _subst ->
      t.need_to_flush <- true;
      Lwt_condition.signal t.need_to_flush_c ();
      Lwt.return (Ok !nr_updated)
    | Error e -> Lwt.return (Error e)

  let flush t =
    let open Qcow_cluster_map in
    let cluster_map = match t.cluster_map with
      | None -> assert false (* by construction, see `make` *)
      | Some x -> x in
    let open Lwt.Infix in
    (* This can be called concurrently by both the user and by the background
       flusher thread. *)
    Lwt_mutex.with_lock t.flush_m
      (fun () ->
        (* Anything erased right now will become available *)
        let erased = Qcow_cluster_map.Erased.get cluster_map in
        let moves = Qcow_cluster_map.moves cluster_map in
        B.flush t.base
        >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          (* Walk over the snapshot of moves before the flush and update. This
             ensures we don't accidentally advance the state of moves which appeared
             after the flush. *)
          let nr_flushed, nr_completed = Cluster.Map.fold (fun _ (move: move) (nr_flushed, nr_completed) ->
            match move.state with
            | Copying | Flushed -> (* no change *)
              nr_flushed, nr_completed
            | Copied ->
              Qcow_cluster_map.(set_move_state cluster_map move.move Flushed);
              nr_flushed + 1, nr_completed
            | Referenced ->
              Qcow_cluster_map.complete_move cluster_map move.move;
              nr_flushed, nr_completed + 1
            ) moves (0, 0) in
          let nr_erased = Cluster.to_int @@ Cluster.IntervalSet.cardinal erased in
          Qcow_cluster_map.(set_cluster_state cluster_map erased Erased Available);
          if nr_flushed <> 0 || nr_completed <> 0 || nr_erased <> 0 then begin
            Log.info (fun f -> f "block recycler: %d cluster copies flushed; %d cluster copies complete; %d clusters erased"
              nr_flushed nr_completed nr_erased);
            Log.info (fun f -> f "block recycler: flush: %s" (Qcow_cluster_map.to_summary_string cluster_map));
          end;
          Lwt.return (Ok ())
      )

  let start_background_thread t ~keep_erased ?compact_after_unmaps () =
    let th, _ = Lwt.task () in
    Lwt.on_cancel th
      (fun () ->
        Log.info (fun f -> f "cancellation of block recycler not implemented");
      );
    let cluster_map = match t.cluster_map with
      | Some x -> x
      | None -> assert false in
    Log.info (fun f -> f "block recycler starting with keep_erased = %Ld" keep_erased);
    let open Lwt.Infix in

    let rec background_flusher () =
      let rec wait () = match t.need_to_flush with
        | true -> Lwt.return_unit
        | false ->
          Lwt_condition.wait t.need_to_flush_c
          >>= fun () ->
          wait () in
      wait ()
      >>= fun () ->
      t.need_to_flush <- false;
      Time.sleep_ns 5_000_000_000L
      >>= fun () ->
      Log.info (fun f -> f "block recycler: triggering background flush: %s" (Qcow_cluster_map.to_summary_string cluster_map));
      flush t
      >>= function
      | Error _ ->
        Log.err (fun f -> f "block recycler: flush failed");
        Lwt.return_unit
      | Ok () ->
        background_flusher () in
    Lwt.async background_flusher;

    let last_block = ref (Qcow_cluster_map.get_last_block cluster_map) in
    let rec wait_for_work () =
      let junk = Qcow_cluster_map.Junk.get cluster_map in
      let nr_junk = Cluster.to_int64 @@ Cluster.IntervalSet.cardinal junk in
      let erased = Qcow_cluster_map.Erased.get cluster_map in
      let nr_erased = Cluster.to_int64 @@ Cluster.IntervalSet.cardinal erased in
      let available = Qcow_cluster_map.Available.get cluster_map in
      let nr_available = Cluster.to_int64 @@ Cluster.IntervalSet.cardinal available in
      (* Apply the threshold to the total clusters erased, which includes those
         marked as available *)
      let total_erased = Int64.add nr_erased nr_available in
      (* Prioritise cluster reuse because it's more efficient not to have to
         move a cluster at all U*)
      let highest_priority =
        if total_erased < keep_erased && nr_junk > 0L then begin
          (* Take some of the junk and erase it *)
          let n = Cluster.of_int64 @@ min nr_junk (Int64.sub keep_erased total_erased) in
          if Cluster.IntervalSet.cardinal junk < n
          then None
          else Some (`Erase n)
        end else None in
      (* If we need to update references, do that next *)
      let moves = Qcow_cluster_map.moves cluster_map in
      let middle_priority =
        let flushed =
          Cluster.Map.fold (fun _src move acc ->
            match move.Qcow_cluster_map.state with
            | Qcow_cluster_map.Flushed -> true
            | _ -> acc
          ) moves false in
        if flushed then Some `Update_references else None in
      begin match highest_priority, middle_priority, compact_after_unmaps with
        | Some x, _, _ -> Lwt.return (Some x)
        | _, Some x, _ -> Lwt.return (Some x)
        | None, _, Some x when x < nr_junk ->
          if not(Cluster.Map.is_empty moves) then begin
            Lwt.return None
          end else begin
            (* Wait for the junk data to stabilise before starting to copy *)
            Log.info (fun f -> f "Discards (%Ld) over threshold (%Ld): waiting for discards to finish before beginning compaction" nr_junk x);
            let rec wait nr_junk n =
              Time.sleep_ns 5_000_000_000L
              >>= fun () ->
              let nr_junk' = Cluster.to_int64 @@ Cluster.IntervalSet.cardinal @@ Qcow_cluster_map.Junk.get cluster_map in
              if nr_junk = nr_junk' then begin
                Log.info (fun f -> f "Discards have finished, %Ld clusters have been discarded" nr_junk);
                Lwt.return ()
              end else begin
                if (n mod 60 = 0) then Log.info (fun f -> f "Total discards %Ld, still waiting" nr_junk');
                wait nr_junk' (n + 1)
              end in
            wait nr_junk 0
            >>= fun () ->
            Lwt.return (Some `Junk)
          end
        | _ ->
          let last_block' = Qcow_cluster_map.get_last_block cluster_map in
          let result =
            if last_block' < !last_block then Some `Resize else None in
          last_block := last_block';
          Lwt.return result
      end >>= function
      | None ->
        Qcow_cluster_map.wait cluster_map
        >>= fun () ->
        wait_for_work ()
      | Some work ->
        Lwt.return work in

    let resize () =
      Locks.with_metadata_lock t.locks
        (fun () ->
          let new_last_block = 1 + (Cluster.to_int @@ Qcow_cluster_map.get_last_block cluster_map) in
          Log.info (fun f -> f "block recycler: resize to %d clusters" new_last_block);
          let new_size = Physical.make (new_last_block lsl t.cluster_bits) in
          let sector = Physical.sector ~sector_size:t.sector_size new_size in
          let cluster = Physical.cluster ~cluster_bits:t.cluster_bits new_size in
          Qcow_cluster_map.resize cluster_map cluster;
          B.resize t.base sector
          >>= function
          | Error _ -> Lwt.fail_with "resize"
          | Ok () ->
          Log.debug (fun f -> f "Resized device to %d sectors of size %d" (Qcow_physical.to_bytes new_size) t.sector_size);
          Lwt.return_unit
        ) in
    let rec loop () =
      t.need_to_flush <- true;
      Lwt_condition.signal t.need_to_flush_c (); (* trigger a flush later *)
      wait_for_work ()
      >>= function
      | `Erase n ->
        begin match Cluster.IntervalSet.take (Qcow_cluster_map.Junk.get cluster_map) n with
        | None -> loop ()
        | Some (to_erase, _) ->
          Log.debug (fun f -> f "block recycler: should erase %s clusters" (Cluster.to_string @@ Cluster.IntervalSet.cardinal to_erase));
          Qcow_cluster_map.(set_cluster_state cluster_map to_erase Junk Roots);
          Lwt.catch
            (fun () ->
              erase t to_erase
              >>= function
              | Error e -> Format.kasprintf Lwt.fail_with "%a" B.pp_write_error e
              | Ok () ->
                Qcow_cluster_map.(set_cluster_state cluster_map to_erase Roots Erased);
                Lwt.return_unit
            ) (fun e ->
              Qcow_cluster_map.(set_cluster_state cluster_map to_erase Roots Junk);
              Lwt.fail e
            )
          >>= fun () ->
          loop ()
        end
      | `Junk ->
        if t.runtime_asserts then Qcow_cluster_map.Debug.assert_no_leaked_blocks cluster_map;
        (* There must be no moves already in progress when starting new moves, otherwise
           we might move the same block twice maybe even to a different location. *)
        assert(Cluster.Map.is_empty @@ Qcow_cluster_map.moves cluster_map);
        let junk = Qcow_cluster_map.Junk.get cluster_map in
        let nr_junk = Cluster.to_int64 @@ Cluster.IntervalSet.cardinal junk in
        let moves = Qcow_cluster_map.start_moves cluster_map in
        Log.info (fun f -> f "block recycler: %Ld clusters are junk, %d moves are possible" nr_junk (List.length moves));
        Qcow_error.Lwt_write_error.or_fail_with @@ move_all t moves
        >>= fun () ->
        resize ()
        >>= fun () ->
        loop ()
      | `Update_references ->
        Log.info (fun f -> f "block recycler: need to update references to blocks");
        begin update_references t
          >>= function
          | Error (`Msg x) -> Lwt.fail_with x
          | Error `Disconnected -> Lwt.fail_with "Disconnected"
          | Error `Is_read_only -> Lwt.fail_with "Is_read_only"
          | Ok nr_updated ->
            Log.info (fun f -> f "block recycler: %Ld block references updated" nr_updated);
            loop ()
        end
      | `Resize ->
        resize ()
        >>= fun () ->
        loop ()
      in

    Lwt.async loop;
    t.background_thread <- th
end
