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

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

open Qcow_types

open Prometheus

module Metrics = struct

  let namespace = "Mirage"
  let subsystem = "qcow"
  let label_name = "id"

  let used =
    let help = "Number of clusters containing user data" in
    Gauge.v_label ~label_name ~help ~namespace ~subsystem "used"

  let junk =
    let help = "Number of junk clusters created" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "junk"

  let erased =
    let help = "Number of clusters erased" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "erased"

  let available =
    let help = "Number of clusters made available for reallocation" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "available"

  let roots =
    let help = "Number of GC root clusters registered" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "roots"

  let copied =
    let help = "Number of cluster copies completed" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "copied"

  let flushed =
    let help = "Number of cluster copies flushed" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "flushed"

  let referenced =
    let help = "Number of references updated" in
    Counter.v_label ~label_name ~help ~namespace ~subsystem "referenced"

  let size =
    let help = "File size in clusters" in
    Gauge.v_label ~label_name ~help ~namespace ~subsystem "size"
end

module Cache = Qcow_cache
module Error = Qcow_error

type reference = Cluster.t * int

let string_of_reference (c, w) = Cluster.to_string c ^ ":" ^ (string_of_int w)

type move_state =
  | Copying
  | Copied
  | Flushed
  | Referenced

let string_of_move_state = function
  | Copying    -> "Copying"
  | Copied     -> "Copied"
  | Flushed    -> "Flushed"
  | Referenced -> "Referenced"

module Move = struct
  type t = { src: Cluster.t; dst: Cluster.t }

  let to_string t =
    Printf.sprintf "%s -> %s" (Cluster.to_string t.src) (Cluster.to_string t.dst)
end

type move = {
  move: Move.t;
  state: move_state;
}

let string_of_move m =
  let state = string_of_move_state m.state in
  Printf.sprintf "%s %s"
    (Move.to_string m.move)
    state

type t = {
  mutable all: Cluster.IntervalSet.t;
  (** Represents the whole file *)
  mutable junk: Cluster.IntervalSet.t;
  (** These are unused clusters containing arbitrary data. They must be erased
      or fully overwritten and then flushed in order to be safely reused. *)
  mutable erased: Cluster.IntervalSet.t;
  (* These are clusters which have been erased, but not flushed. They will become
     available for reallocation on the next flush. *)
  mutable available: Cluster.IntervalSet.t;
  (** These clusters are available for immediate reuse; after a crash they are
      guaranteed to be full of zeroes. *)
  mutable roots: Cluster.IntervalSet.t;
  (* map from physical cluster to the physical cluster + offset of the reference.
     When a block is moved, this reference must be updated. *)
  mutable copies: Cluster.IntervalSet.t;
  (** Clusters which contain clusters as part of a move *)
  mutable moves: move Cluster.Map.t;
  (** The state of in-progress block moves, indexed by the source cluster *)
  mutable refs: reference Cluster.Map.t;
  first_movable_cluster: Cluster.t;
  cache: Cache.t;
  c: unit Lwt_condition.t;
  (** Signalled when any of the junk/erased sets change or when references need
      to be rewritten to kick the background recycling thread. *)
  runtime_asserts: bool;
  (** Check leak and sharing invariants on every update *)
  id: string option;
  (** value of the `id` label used in Metrics above *)
  cluster_size: int;
  (** the number of bytes in a cluster, for computing Metrics in bytes *)
}

let get_last_block t =
  let max_ref =
    try
      fst @@ Cluster.Map.max_binding t.refs
    with Not_found ->
      Cluster.zero in
  let max_root =
    try
      Cluster.IntervalSet.Interval.y @@ Cluster.IntervalSet.max_elt t.roots
    with Not_found ->
      Cluster.zero in
  let max_copies =
    try
      Cluster.IntervalSet.Interval.y @@ Cluster.IntervalSet.max_elt t.copies
    with Not_found ->
      Cluster.zero in
  let max_move =
    try
      fst @@ Cluster.Map.max_binding t.moves
    with Not_found ->
      Cluster.zero in
  max (Cluster.pred t.first_movable_cluster) @@ max max_ref @@ max max_root @@ max max_move max_copies

let total_used t =
  Int64.of_int @@ Cluster.Map.cardinal t.refs

let total_free t =
  Cluster.to_int64 @@ Cluster.IntervalSet.cardinal t.junk

let total_erased t =
  Cluster.to_int64 @@ Cluster.IntervalSet.cardinal t.erased

let total_available t =
  Cluster.to_int64 @@ Cluster.IntervalSet.cardinal t.available

let total_moves t =
  Cluster.Map.fold (fun _ m (copying, copied, flushed, referenced) -> match m.state with
    | Copying -> (copying + 1), copied, flushed, referenced
    | Copied -> copying, copied + 1, flushed, referenced
    | Flushed -> copying, copied, flushed + 1, referenced
    | Referenced -> copying, copied, flushed, referenced + 1
  ) t.moves (0, 0, 0, 0)

let total_copies t =
  Cluster.to_int64 @@ Cluster.IntervalSet.cardinal t.copies

let total_roots t =
  Cluster.to_int64 @@ Cluster.IntervalSet.cardinal t.roots

let to_summary_string t =
  let copying, copied, flushed, referenced = total_moves t in
  Printf.sprintf "%Ld used; %Ld junk; %Ld erased; %Ld available; %Ld copies; %Ld roots; %d Copying; %d Copied; %d Flushed; %d Referenced; max_cluster = %s"
    (total_used t) (total_free t) (total_erased t) (total_available t) (total_copies t) (total_roots t)
    copying copied flushed referenced (Cluster.to_string @@ get_last_block t)

module Debug = struct
  let check ?(leaks=true) ?(sharing=true) t =
    let open Cluster.IntervalSet in
    let last = get_last_block t in
    if last >= t.first_movable_cluster then begin
      let whole_file = add (Interval.make t.first_movable_cluster last) empty in
      let refs = Cluster.Map.fold (fun cluster _ set ->
        add (Interval.make cluster cluster) set
      ) t.refs empty in
      let moves = Cluster.Map.fold (fun _ m set ->
        let dst = m.move.Move.dst in
        add (Interval.make dst dst) set
      ) t.moves empty in
      let junk = "junk", t.junk in
      let erased = "erased", t.erased in
      let available = "available", t.available in
      let refs = "refs", refs in
      let moves = "moves", moves in
      let copies = "copies", t.copies in
      let roots = "roots", t.roots in
      let cached = "cached", Cache.Debug.all_cached_clusters t.cache in
      let all = [ junk; erased; available; refs; moves; copies; roots ] in
      let leaked = List.fold_left diff whole_file (List.map snd all) in
      if leaks && (cardinal leaked <> Cluster.zero) then begin
        Log.err (fun f -> f "%s" (to_summary_string t));
        Log.err (fun f -> f "%s clusters leaked: %s" (Cluster.to_string @@ cardinal leaked)
          (Sexplib.Sexp.to_string_hum (sexp_of_t leaked)));
        assert false
      end;
      let rec cross xs = function
        | [] -> []
        | y :: ys -> List.map (fun x -> x, y) xs @ cross xs ys in
      let check zs =
        List.iter (fun ((x_name, x), (y_name, y)) ->
          if x_name <> y_name then begin
            let i = inter x y in
            if cardinal i <> Cluster.zero then begin
              Log.err (fun f -> f "%s" (to_summary_string t));
              Log.err (fun f -> f "%s and %s are not disjoint" x_name y_name);
              Log.err (fun f -> f "%s = %s" x_name (Sexplib.Sexp.to_string_hum (sexp_of_t x)));
              Log.err (fun f -> f "%s = %s" y_name (Sexplib.Sexp.to_string_hum (sexp_of_t y)));
              Log.err (fun f -> f "intersection = %s" (Sexplib.Sexp.to_string_hum (sexp_of_t i)));
              assert false
            end
          end
        ) zs in
      (* These must be disjoint *)
      if sharing then begin
        check @@ cross
          [ junk; erased; available; refs ]
          [ junk; erased; available; refs ];
        check @@ cross
          [ junk; copies; erased; available ]
          [ junk; copies; erased; available ];
        check @@ cross
          [ cached ]
          [ junk; erased; available ]
      end;
      (* moves and copies should be the same *)
      let d = union (diff (snd copies) (snd moves)) (diff (snd moves) (snd copies)) in
      if not(is_empty d) then begin
        Log.err (fun f -> f "%s" (to_summary_string t));
        Log.err (fun f -> f "moves and refs are not the same");
        Log.err (fun f -> f "moves = %s" (Sexplib.Sexp.to_string_hum (sexp_of_t (snd moves))));
        Log.err (fun f -> f "refs  = %s" (Sexplib.Sexp.to_string_hum (sexp_of_t (snd refs))));
        Log.err (fun f -> f "diff  = %s" (Sexplib.Sexp.to_string_hum (sexp_of_t d)))
      end
    end
    let assert_no_leaked_blocks t = check t

  let assert_equal a b =
    let map_equals name pp a b =
      Cluster.Map.fold (fun k v acc ->
        let v' = try Some (Cluster.Map.find k b) with Not_found -> None in
        if Some v <> v' then begin
          Log.err (fun f -> f "%s: a has cluster %s -> %s but b has cluster %s -> %s"
            name (Cluster.to_string k) (pp v) (Cluster.to_string k) (match v' with None -> "None" | Some v -> pp v)
          );
          false
        end else acc
      ) a true in
    let moves = map_equals "moves" string_of_move a.moves b.moves in
    let refs = map_equals "refs" string_of_reference a.refs b.refs in
    let first_movable_cluster =
      if a.first_movable_cluster <> b.first_movable_cluster then begin
        Log.err (fun f -> f "a has first_movable_cluster = %s but b has first_movable_cluster = %s"
          (Cluster.to_string a.first_movable_cluster) (Cluster.to_string b.first_movable_cluster)
        );
        false
      end else true in
    if not(moves && refs && first_movable_cluster) then begin
      failwith "cluster maps are different"
    end

  let metadata_blocks t =
    let open Cluster.IntervalSet in
    let header = add (Interval.make Cluster.zero (Cluster.pred t.first_movable_cluster)) empty in
    (* All clusters which reference other clusters must be metadata *)
    Cluster.Map.fold (fun _ (cluster, _) set ->
      add (Interval.make cluster cluster) set
    ) t.refs header
end

module type MutableSet = sig
  val get: t -> Cluster.IntervalSet.t
  val remove: t -> Cluster.IntervalSet.t -> unit
  val mem: t -> Cluster.t -> bool
end

let make ~free ~refs ~cache ~first_movable_cluster ~runtime_asserts ~id ~cluster_size =
  let junk = Qcow_bitmap.fold
    (fun i acc ->
      let x, y = Qcow_bitmap.Interval.(x i, y i) in
      let x = Cluster.of_int64 x and y = Cluster.of_int64 y in
      Cluster.IntervalSet.(add (Interval.make x y) acc)
    ) free Cluster.IntervalSet.empty in
  let copies = Cluster.IntervalSet.empty in
  let roots = Cluster.IntervalSet.empty in
  let available = Cluster.IntervalSet.empty in
  let erased = Cluster.IntervalSet.empty in
  let moves = Cluster.Map.empty in
  let c = Lwt_condition.create () in
  let last =
    let last_header = Cluster.pred first_movable_cluster in
    let last_ref =
      try
        fst @@ Cluster.Map.max_binding refs
      with Not_found ->
        Cluster.zero in
    let last_free =
      try
        Cluster.IntervalSet.Interval.y @@ Cluster.IntervalSet.max_elt junk
      with Not_found ->
        Cluster.zero in
    max last_header (max last_ref last_free) in
  let all = Cluster.IntervalSet.(add (Interval.make Cluster.zero last) empty) in
  ( match id with
    | Some id ->
      Counter.inc (Metrics.junk id) (float_of_int cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal junk));
      Gauge.set (Metrics.used id) (float_of_int cluster_size *. (float_of_int @@ Cluster.Map.cardinal refs));
    | None -> () );
  { all; junk; available; erased; copies; roots; moves; refs; first_movable_cluster; cache; c; runtime_asserts; id;
    cluster_size }

let zero =
  let free = Qcow_bitmap.make_empty ~initial_size:0 ~maximum_size:0 in
  let refs = Cluster.Map.empty in
  let cache = Cache.create
    ~read_cluster:(fun _ -> Lwt.return (Error `Unimplemented))
    ~write_cluster:(fun _ _ -> Lwt.return (Error `Unimplemented))
    () in
  make ~free ~refs ~first_movable_cluster:Cluster.zero ~cache ~runtime_asserts:false ~id:None ~cluster_size:0

let resize t new_size_clusters =
  let open Cluster.IntervalSet in
  let file = add (Interval.make Cluster.zero (Cluster.pred new_size_clusters)) empty in
  Cache.resize t.cache new_size_clusters;
  t.junk <- inter t.junk file;
  t.erased <- inter t.erased file;
  t.available <- inter t.available file;
  (* New blocks on the end of the file are assumed to be zeroed and therefore available *)
  let zeroed = diff file t.all in
  t.available <- union t.available zeroed;
  ( match t.id with None -> () | Some id -> Counter.inc (Metrics.available id) (float_of_int t.cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal zeroed)) );
  if cardinal zeroed > Cluster.zero
  then Log.info (fun f -> f "resize: adding available clusters %s"
    (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t zeroed)
  );
  t.all <- file;
  match t.id with None -> () | Some id -> Gauge.set (Metrics.size id) (Cluster.to_float new_size_clusters *. (float_of_int t.cluster_size))

module Junk = struct
  let get t = t.junk
  let add t more =
    Log.debug (fun f -> f "Junk.add %s" (Sexplib.Sexp.to_string (Cluster.IntervalSet.sexp_of_t more)));
    t.junk <- Cluster.IntervalSet.union t.junk more;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.junk id) (float_of_int t.cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal more)) );
    (* Ensure all cached copies of junk blocks are dropped *)
    Cluster.IntervalSet.(fold (fun i () ->
      let x, y = Interval.(x i, y i) in
      let rec loop n =
        if n <= y then begin
          Cache.remove t.cache n;
          loop (Cluster.succ n)
        end in
      loop x
    ) more ());
    if t.runtime_asserts then Debug.check ~leaks:false t;
    Lwt_condition.signal t.c ()
  let remove t less =
    let open Cluster.IntervalSet in
    let old_junk = t.junk in
    t.junk <- Cluster.IntervalSet.diff t.junk less;
    if (Cluster.sub (cardinal old_junk) (cardinal less)) <> (cardinal t.junk) then begin
      Log.err (fun f -> f "Junk.remove: clusters were not in junk");
      Log.err (fun f -> f "Junk = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t old_junk));
      Log.err (fun f -> f "To remove = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t less));
      failwith "Junk.remove: clusters were not in junk"
    end;
    Lwt_condition.signal t.c ()
  let mem t elt = Cluster.IntervalSet.mem elt t.junk
end

module Available = struct
  let get t = t.available
  let add t more =
    let open Cluster.IntervalSet in
    Log.debug (fun f -> f "Available.add %s"
      (Sexplib.Sexp.to_string (sexp_of_t more))
    );
    t.available <- union t.available more;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.available id) (float_of_int t.cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal more)) );
    if t.runtime_asserts then Debug.check ~leaks:false t;
    Lwt_condition.signal t.c ()
  let remove t less =
    let open Cluster.IntervalSet in
    Log.debug (fun f -> f "Available.remove %s"
      (Sexplib.Sexp.to_string @@ sexp_of_t less)
    );
    let old_available = t.available in
    t.available <- Cluster.IntervalSet.diff t.available less;
    if (Cluster.sub (cardinal old_available) (cardinal less)) <> (cardinal t.available) then begin
      Log.err (fun f -> f "Available.remove: clusters were not in junk");
      Log.err (fun f -> f "Available = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t old_available));
      Log.err (fun f -> f "To remove = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t less));
      failwith "Available.remove: clusters were not in available"
    end;
    Lwt_condition.signal t.c ()
  let mem t elt = Cluster.IntervalSet.mem elt t.available
end

module Erased = struct
  let get t = t.erased
  let add t more =
    Log.debug (fun f -> f "Erased.add %s" (Sexplib.Sexp.to_string (Cluster.IntervalSet.sexp_of_t more)));
    t.erased <- Cluster.IntervalSet.union t.erased more;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.erased id) (float_of_int t.cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal more)) );
    if t.runtime_asserts then Debug.check ~leaks:false t;
    Lwt_condition.signal t.c ()
  let remove t less =
    let open Cluster.IntervalSet in
    let old_erased = t.erased in
    t.erased <- diff t.erased less;
    if (Cluster.sub (cardinal old_erased) (cardinal less)) <> (cardinal t.erased) then begin
      Log.err (fun f -> f "Erased.remove: clusters were not in erased");
      Log.err (fun f -> f "Erased = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t old_erased));
      Log.err (fun f -> f "To remove = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t less));
      failwith "Erased.remove: clusters were not in erased"
    end;
    Lwt_condition.signal t.c ()
  let mem t elt = Cluster.IntervalSet.mem elt t.erased
end

module Copies = struct
  let get t = t.erased
  let add t more =
    Log.debug (fun f -> f "Copies.add %s" (Sexplib.Sexp.to_string (Cluster.IntervalSet.sexp_of_t more)));
    t.copies <- Cluster.IntervalSet.union t.copies more;
    if t.runtime_asserts then Debug.check ~leaks:false t;
    Lwt_condition.signal t.c ()
  let remove t less =
    let open Cluster.IntervalSet in
    let old_copies = t.copies in
    t.copies <- diff t.copies less;
    if (Cluster.sub (cardinal old_copies) (cardinal less)) <> (cardinal t.copies) then begin
      Log.err (fun f -> f "Copies.remove: clusters were not in copies");
      Log.err (fun f -> f "Copies = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t old_copies));
      Log.err (fun f -> f "To remove = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t less));
      failwith "Copies.remove: clusters were not in copies"
    end;
    Lwt_condition.signal t.c ()
  let mem t elt = Cluster.IntervalSet.mem elt t.copies
end

module Roots = struct
  let get t = t.roots
  let add t more =
    let open Cluster.IntervalSet in
    Log.debug (fun f -> f "Roots.add %s" (Sexplib.Sexp.to_string (sexp_of_t more)));
    let intersection = inter more t.roots in
    if not @@ is_empty @@ intersection then begin
      Log.err (fun f -> f "Clusters are already registered as roots: %s"
        (Sexplib.Sexp.to_string @@ sexp_of_t more)
      );
      Log.err (fun f -> f "Intersection: %s"
        (Sexplib.Sexp.to_string @@ sexp_of_t intersection)
      );
      assert false;
    end;
    t.roots <- union t.roots more;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.roots id) (float_of_int t.cluster_size *. (Cluster.to_float @@ Cluster.IntervalSet.cardinal more)) );
    if t.runtime_asserts then Debug.check ~leaks:false t;
    Lwt_condition.signal t.c ()
  let remove t less =
    let open Cluster.IntervalSet in
    let old_roots = t.roots in
    t.roots <- diff t.roots less;
    if (Cluster.sub (cardinal old_roots) (cardinal less)) <> (cardinal t.roots) then begin
      Log.err (fun f -> f "Roots.remove: clusters were not in roots");
      Log.err (fun f -> f "Roots = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t old_roots));
      Log.err (fun f -> f "To remove = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t less));
      failwith "Roots.remove: clusters were not in roots"
    end;
    Lwt_condition.signal t.c ()
  let mem t elt = Cluster.IntervalSet.mem elt t.roots
end

type cluster_state =
  | Junk
  | Erased
  | Available
  | Copies
  | Roots

let set_cluster_state t set src dst =
  begin match src with
  | Junk      -> Junk.remove      t set
  | Erased    -> Erased.remove    t set
  | Available -> Available.remove t set
  | Copies    -> Copies.remove    t set
  | Roots     -> Roots.remove     t set
  end;
  begin match dst with
  | Junk      -> Junk.add         t set
  | Erased    -> Erased.add       t set
  | Available -> Available.add    t set
  | Copies    -> Copies.add       t set
  | Roots     -> Roots.add        t set
  end

let wait t = Lwt_condition.wait t.c

let find t cluster = Cluster.Map.find cluster t.refs

let moves t = t.moves

let set_move_state t move state =
  let m = { move; state } in
  let old_state =
    if Cluster.Map.mem move.Move.src t.moves
    then Some ((Cluster.Map.find move.Move.src t.moves).state)
    else None in
  let src = move.Move.src in
  let dst = move.Move.dst in
  if Cluster.Map.mem dst t.moves then begin
    let { move = dst_move; state = dst_state } = Cluster.Map.find dst t.moves in
    Log.err (fun f -> f "Illegal cluster move: %s -> %s but destination is already moving from %s -> %s and in state %s"
      (Cluster.to_string move.Move.src) (Cluster.to_string move.Move.dst)
      (Cluster.to_string dst_move.Move.src) (Cluster.to_string dst_move.Move.dst)
      (string_of_move_state dst_state)
    );
    assert false
  end;
  match old_state, state with
  | None, Copying ->
    let open Cluster.IntervalSet in
    let dst' = add (Interval.make dst dst) empty in
    (* We always move into junk blocks *)
    if not @@ mem dst t.junk then begin
      Log.err (fun f -> f "Copying cluster from %s -> %s: destination is not in the Junk set"
        (Cluster.to_string move.Move.src) (Cluster.to_string move.Move.dst)
      );
      Log.err (fun f -> f "Junk = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t t.junk));
      assert false;
    end;
    if mem dst t.copies then begin
      Log.err (fun f -> f "Copying cluster from %s -> %s: destination is already in the Copies set"
        (Cluster.to_string move.Move.src) (Cluster.to_string move.Move.dst)
      );
      Log.err (fun f -> f "Copies = %s" (Sexplib.Sexp.to_string_hum ~indent:2 @@ sexp_of_t t.copies));
      assert false;
    end;
    if Cluster.Map.mem dst t.moves then begin
      Log.err (fun f -> f "Copying cluster from %s -> %s: destination is already in the moves list"
        (Cluster.to_string move.Move.src) (Cluster.to_string move.Move.dst)
      );
      assert false;
    end;
    if Cluster.Map.mem src t.moves then begin
      Log.err (fun f -> f "Copying cluster from %s -> %s: source is already in the moves list"
        (Cluster.to_string move.Move.src) (Cluster.to_string move.Move.dst)
      );
      assert false;
    end;
    Log.debug (fun f -> f "Cluster %s None -> Copying" (Cluster.to_string move.Move.src));
    Junk.remove t dst';
    t.moves <- Cluster.Map.add move.Move.src m t.moves;
    Copies.add t dst'
  | Some Copying, Copied ->
    Log.debug (fun f -> f "Cluster %s Copying -> Copied" (Cluster.to_string move.Move.src));
    t.moves <- Cluster.Map.add move.Move.src m t.moves;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.copied id) (float_of_int t.cluster_size) )
  | Some Copied, Flushed ->
    Log.debug (fun f -> f "Cluster %s Copied -> Flushed" (Cluster.to_string move.Move.src));
    t.moves <- Cluster.Map.add move.Move.src m t.moves;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.flushed id) (float_of_int t.cluster_size) );
    (* References now need to be rewritten *)
    Lwt_condition.signal t.c ();
  | Some Flushed, Referenced ->
    Log.debug (fun f -> f "Cluster %s Flushed -> Referenced" (Cluster.to_string move.Move.src));
    t.moves <- Cluster.Map.add move.Move.src m t.moves;
    ( match t.id with None -> () | Some id -> Counter.inc (Metrics.referenced id) (float_of_int t.cluster_size) );
  | Some old, _ ->
    Log.err (fun f -> f "Illegal cluster move state transition: %s %s -> %s" (Cluster.to_string move.Move.src)
      (string_of_move_state old) (string_of_move_state state));
    assert false
  | None, _ ->
    Log.warn (fun f -> f "Not updating move state of cluster %s: operation cancelled" (Cluster.to_string move.Move.src))

let cancel_move t cluster =
  match Cluster.Map.find cluster t.moves with
    | { state = Referenced; _ } ->
      (* The write will have followed the reference to the destination block.
         There are 2 interesting possibilities if we crash without flushing:
         - neither the write nor the reference are committed: this behaves as if
           the write wasn't committed which is valid
         - the write is committed but the reference isn't: this also behaves
           as if the write wasn't committed which is valid
         The only reason we still track this move is because when the next flush
         happens it is safe to add the src cluster to the set of junk blocks. *)
      Log.debug (fun f -> f "Not cancelling in-progress move of cluster %s: already Referenced" (Cluster.to_string cluster))
    | { move = { Move.dst; _ }; _ } ->
      Log.debug (fun f -> f "Cancelling in-progress move of cluster %s to %s" (Cluster.to_string cluster) (Cluster.to_string dst));
      t.moves <- Cluster.Map.remove cluster t.moves;
      let dst' = Cluster.IntervalSet.(add (Interval.make dst dst) empty) in
      (* The destination block can now be recycled *)
      Copies.remove t dst';
      Junk.add t dst'
    | exception Not_found ->
      ()

let complete_move t move =
  let old_state =
    if Cluster.Map.mem move.Move.src t.moves
    then Some ((Cluster.Map.find move.Move.src t.moves).state)
    else None in
  match old_state with
  | None ->
    Log.warn (fun f -> f "Not completing move state of cluster %s: operation cancelled" (Cluster.to_string move.Move.src))
  | Some Referenced ->
    t.moves <- Cluster.Map.remove move.Move.src t.moves;
    let dst = Cluster.IntervalSet.(add (Interval.make move.Move.dst move.Move.dst) empty) in
    Copies.remove t dst;
    (* The source block will have already been added to Junk by the Metadata.Physical.set *)
  | Some old ->
    Log.err (fun f -> f "Illegal cluster move state transition: %s %s -> Completed" (Cluster.to_string move.Move.src)
      (string_of_move_state old)
    );
    failwith "Attempt to complete an incomplete cluster move"

let is_moving t src = Cluster.Map.mem src t.moves

let add t rf cluster =
  let c, w = rf in
  if cluster = Cluster.zero then () else begin
    if Cluster.Map.mem cluster t.refs then begin
      let c', w' = Cluster.Map.find cluster t.refs in
      Log.err (fun f -> f "Found two references to cluster %s: %s.%d and %s.%d" (Cluster.to_string cluster) (Cluster.to_string c) w (Cluster.to_string c') w');
      raise (Error.Duplicate_reference((Cluster.to_int64 c, w), (Cluster.to_int64 c', w'), Cluster.to_int64 cluster))
    end;
    let junk      = Junk.mem      t cluster in
    let erased    = Erased.mem    t cluster in
    let available = Available.mem t cluster in
    let roots     = Roots.mem     t cluster in
    let copies    = Copies.mem    t cluster in
    if not(roots || copies) || junk || erased || available then begin
      Log.err (fun f -> f "Adding a reference to cluster %s in %s.%d, cluster in state: %s %s %s %s %s"
        (Cluster.to_string cluster) (Cluster.to_string c) w
        (if junk      then "Junk "      else "")
        (if erased    then "Erased "    else "")
        (if available then "Available " else "")
        (if roots     then "Roots "     else "")
        (if copies    then "Copies "    else "")
      );
      failwith (Printf.sprintf "Adding a reference to unsuitable cluster %s in %s.%d" (Cluster.to_string cluster) (Cluster.to_string c) w);
    end;
    t.refs <- Cluster.Map.add cluster rf t.refs;
    ( match t.id with None -> () | Some id -> Gauge.inc (Metrics.used id) (float_of_int t.cluster_size) );
    ()
  end

let remove t cluster =
  t.refs <- Cluster.Map.remove cluster t.refs;
  ( match t.id with None -> () | Some id -> Gauge.dec (Metrics.used id) (float_of_int t.cluster_size) );
  Junk.add t (Cluster.IntervalSet.(add (Interval.make cluster cluster) empty));
  cancel_move t cluster;
  Lwt_condition.signal t.c ()

let start_moves t =
  (* The last allocated block. Note if there are no data blocks this will
     point to the last header block even though it is immovable. *)
  let max_cluster = get_last_block t in
  let refs = ref t.refs in
  let moves = fst @@ Cluster.IntervalSet.fold_individual
    (fun cluster (moves, max_cluster) ->
      (* A free block after the last allocated block will not be filled.
         It will be erased from existence when the file is truncated at the
         end. *)
      if cluster >= max_cluster then (moves, max_cluster) else begin
        (* find the last physical block *)
        let last_block, rf = Cluster.Map.max_binding (!refs) in

        if cluster >= last_block then moves, last_block else begin
          let src = last_block and dst = cluster in
          if Cluster.Map.mem src t.moves
          then moves, last_block (* move already in progress, don't move it again *)
          else begin
            (* copy last_block into cluster and update rf *)
            let move = { Move.src; dst } in
            refs := Cluster.Map.remove last_block @@ Cluster.Map.add cluster rf (!refs);
            move :: moves, last_block
          end
        end
      end
    ) t.junk ([], max_cluster) in
  List.iter (fun move -> set_move_state t move Copying) moves;
  moves

let is_immovable t cluster = cluster < t.first_movable_cluster

let update_references t substitutions =
  let refs = Cluster.Map.fold (fun to_c (from_c, from_w) acc ->
    (* Has the cluster [from_c] been moved? *)
    let from_c' = try Cluster.Map.find from_c substitutions with Not_found -> from_c in
    if from_c <> from_c' then begin
      Log.debug (fun f -> f "Updating reference %s:%d -> %s to %s:%d -> %s"
        (Cluster.to_string from_c) from_w (Cluster.to_string to_c)
        (Cluster.to_string from_c') from_w (Cluster.to_string to_c)
      );
    end;
    Cluster.Map.add to_c (from_c', from_w) acc
  ) t.refs Cluster.Map.empty in
  t.refs <- refs
