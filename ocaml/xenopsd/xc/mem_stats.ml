(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "mem_stats" end)

module Compat = struct
  let mtime_clock_counter = Mtime_clock.counter

  let mtime_clock_count = Mtime_clock.count

  let mtime_span_to_s f = Mtime.Span.to_float_ns f *. 1e-9

  let file_lines_fold = Xapi_stdext_unix.Unixext.file_lines_fold

  let reporter_async ~shared_page_count ~dss_f =
    let open Rrdd_plugin in
    Reporter.start_async
      (module D : Debug.DEBUG)
      ~uid:"mem-stats" ~neg_shift:0.5 ~target:(Reporter.Local shared_page_count)
      ~protocol:Rrd_interface.V2 ~dss_f
end

open Compat

module SlowReporter = struct
  (** [report ~interval_s ~generate_dss] calls [generate_dss] every [interval_s] only,
      and substitutes the previous value when called more often.
      Using VT_Unknown or NaN would leave gaps in the graph.
      Report_local only supports reporting at 5s intervals, but some metrics are too costly to
      gather that often, e.g. Gc.stat needs to walk the entire heap.
   *)
  let report ~interval_s ~generate_dss state =
    match state with
    | Some (t0, dss) when mtime_clock_count t0 |> mtime_span_to_s < interval_s
      ->
        (state, (false, dss))
    | None | Some _ ->
        let dss = generate_dss () in
        let next_state = Some (mtime_clock_counter (), dss) in
        (next_state, (true, dss))

  let iter_of_fold f =
    let last = ref None in
    fun () ->
      let next, r = f !last in
      (* to make reasoning about metrics easier, this is the only place
         that contains explicit mutation in this file
         (other than hashtbl construction on startup)
      *)
      last := next ;
      r
end

module SimpleMetrics = struct
  (* metric definitions *)

  (* Caveats:
     * do not use ~transform flag to ds_make: it is lost during rrd-transport with no warning!
     * writing a VT_Unknown causes an exception and prevents other values from being written too
     * the way to write an unknown value is to write something out of min/max range, which then gets
     converted to nan upon reading (or write nan if the type is float, don't do this if type is int
     since it causes a metadata change)
     * use Sys.word_size, and not hard-coded word-to-kib conversion, the old code still had the
     values for a 32-bit word
     * the value type (int64 vs float) is cached by the transport, changing it causes a metadata
     change
     * it is best to always write out the same Rrd list, even if some values are missing, otherwise
     the metadata would have to get reparsed (crc changed). But that doesn't actually work that way
     because the metadata also includes the values (they are changed to uninitialized_ds only upon
     read), thus any change in value triggers a metadata crc change, and a metadata reparse! Still
     lets assume this will be fixed and avoid needless metadata changes!
     * everything gets converted to a float in the end: int64 type just has some more precision prior
     to delta calculation. However for our purposes here the 'float' type (which is double precision)
     has 53-bits of precision, which is more than enough for KiB - it results in 63-bits of precision
     in term of bytes (including sign)
  *)

  let ds_int64 i = Rrd.VT_Int64 i

  let ds_float f = Rrd.VT_Float f

  let ds_update ds v = {ds with Ds.ds_value= v}

  let executable = Filename.basename Sys.executable_name

  let ds_name name =
    Printf.sprintf "%s_%s" executable (Astring.String.Ascii.lowercase name)

  let ds_description desc = Printf.sprintf desc executable

  let to_float ds =
    match ds.Ds.ds_value with
    | Rrd.VT_Int64 i ->
        if i >= 0L then Int64.to_float i else nan
    | Rrd.VT_Float f ->
        f
    | Rrd.VT_Unknown ->
        nan

  let define_unit ~ty ~units ~min ?(default = true) name description =
    let name = ds_name name in
    let description = ds_description description in
    fun value ->
      Ds.ds_make ~name ~description ~value ~ty ~default ~units ~min ()

  let kib ?default ?(min = 0.0) v =
    define_unit ~ty:Rrd.Gauge ~units:"KiB" ~min ?default v

  let kib_per_s = define_unit ~ty:Rrd.Derive ~units:"KiB/s" ~min:0.0

  let count = define_unit ~ty:Rrd.Gauge ~units:"" ~min:0.0

  let words_to_kib w = w *. float (Sys.word_size / 8) /. 1024. |> ds_float
end

module Proc = struct
  module KV = Astring.String.Map

  (** [parse_value_count] parses '    N'. *)
  let parse_value_count s = Scanf.sscanf s " %Lu" Fun.id

  (** [parse_value_kib s] parses values of the form '  N kB'. *)
  let parse_value_kib s = Scanf.sscanf s " %Lu kB" Fun.id

  let file_lines_filter_map f ~path =
    let fold acc line =
      match f line with None -> acc | Some (k, v) -> KV.add k v acc
    in
    file_lines_fold fold KV.empty path

  open SimpleMetrics

  let parse_keyvalue fields ~path () =
    let parse_pairs = function
      | None ->
          None
      | Some (key, value) -> (
        match KV.find key fields with
        | None ->
            None
        | Some (parse_value, ds) ->
            let v = parse_value value in
            Some (key, ds_update ds @@ ds_int64 v)
      )
    in
    file_lines_filter_map ~path @@ fun line ->
    line |> Astring.String.cut ~sep:":" |> parse_pairs

  let define_fields ~path l =
    let kv =
      ListLabels.fold_left l ~init:KV.empty ~f:(fun acc (key, ds) ->
          KV.add key ds acc
      )
    in
    parse_keyvalue kv ~path:(Filename.concat "/proc/self" path)

  let unknown = ds_int64 (-1L)

  let kib ?default key desc =
    (key, (parse_value_kib, kib ?default key desc unknown))

  let count ?default key desc =
    (key, (parse_value_count, count ?default key desc unknown))

  module Fields = struct
    let vmdata = "VmData"

    let vmpte = "VmPTE"

    let threads = "Threads"

    let fdsize = "FDSize"

    let vmsize = "VmSize"

    let vmlck = "VmLck"

    let vmpin = "VmPin"

    let vmstk = "VmStk"

    let rss = "Rss"

    (* there is also /proc/self/stat and /proc/self/statm, but we'd need to open and parse both *)
    let status =
      define_fields ~path:"status"
        [
          count threads "Total number of threads used by %s"
        ; count fdsize "Total number of file descriptors used by %s"
        ; kib vmsize "Total amount of memory mapped by %s"
        ; kib vmlck "Total amount of memory locked by %s"
        ; kib vmpin "Total amount of memory pinned by %s"
          (* VmRSS is inaccurate accoring to latest proc(5) *)
        ; kib vmdata
            "Total amount of writable, non-shared and non-stack memory used by \
             %s"
        ; kib vmstk "Total amount of main stack memory used by %s"
        ; kib vmpte "Total amount of page table entry memory used by %s"
        ]

    (* According to latest proc(5) these are slower, but provide more accurate information.
       The RSS reported by other stat counters could be off depending on the number of threads. *)
    let smaps_rollup =
      define_fields ~path:"smaps_rollup"
        [kib rss "Total amount of resident memory used by %s"]
  end

  let find key kv = to_float (KV.get key kv)

  let to_list kv = KV.bindings kv |> List.rev_map snd
end

module GcStat = struct
  open SimpleMetrics
  open Gc

  let ocaml_total =
    let field = kib "ocaml_total" "Total OCaml memory used by %s" in
    fun gc control ->
      gc.heap_words + control.minor_heap_size |> float |> words_to_kib |> field

  let maybe_words name description v =
    (* quick_stat would return a value of 0, which is not valid *)
    v |> float |> words_to_kib |> kib ~min:0.001 name description

  let memory_allocation_precise (gc, control) =
    [
      ocaml_total gc control
    ; gc.minor_words +. gc.major_words -. gc.promoted_words
      |> words_to_kib
      |> kib_per_s "ocaml_allocation_rate"
           "Amount of allocations done by OCaml in the given period by %s"
    ]

  let memory_allocation_approx_expensive (gc, _control) =
    [
      (* see https://github.com/ocaml/ocaml/blob/trunk/stdlib/gc.mli#L50-L59, without running a major
          cycle the live_words may overestimate the actual live words, "live" just means "not currently
         known to be collectible"*)
      gc.live_words
      |> maybe_words "ocaml_maybe_live"
           "OCaml memory not currently known to be collectable by %s"
    ; gc.free_words |> maybe_words "ocaml_free" "OCaml memory available to %s"
    ]
end

module Derived = struct
  open SimpleMetrics

  let memextra_kib =
    kib "mem_extra" "Total amount of non-OCaml and non-stack memory used by %s"

  let ulimit_stack =
    let ic = Unix.open_process_in "ulimit -s" in
    let r = ic |> input_line |> Int64.of_string in
    close_in_noerr ic ; Int64.to_float r

  let memextra_kib stats (gc_stat, control) =
    let ocaml_total_kib = GcStat.ocaml_total gc_stat control |> to_float in
    let vmdata = Proc.find Proc.Fields.vmdata stats in
    let vmpte = Proc.find Proc.Fields.vmpte stats in
    let threads = Proc.find Proc.Fields.threads stats in
    (* Each thread, except the main one will allocate 'ulimit -s' worth of VmData.
       This won't immediately show up in VmRss until actually used (e.g. by a deep calltree due to recursion) *)
    vmdata -. vmpte -. (ulimit_stack *. (threads -. 1.)) -. ocaml_total_kib
    |> ds_float
    |> memextra_kib
end

let observe_stats l =
  let names = ListLabels.rev_map l ~f:(fun ds -> ds.Ds.ds_name) in
  let values =
    ListLabels.rev_map l ~f:(fun ds ->
        let f =
          match ds.Ds.ds_value with
          | Rrd.VT_Int64 i ->
              Int64.to_float i
          | Rrd.VT_Float f ->
              f
          | Rrd.VT_Unknown ->
              nan
        in
        Rrd.apply_transform_function ds.Ds.ds_pdp_transform_function f
        |> Printf.sprintf "%.0f"
    )
  in
  D.debug "stats header: %s" (String.concat "," names) ;
  D.debug "stats values: %s" (String.concat "," values)

let generate_expensive_stats =
  let generate_dss () =
    let stat = Gc.stat () in
    let gc_control = Gc.get () in
    let rss = Proc.Fields.smaps_rollup () |> Proc.to_list in
    let gcstat = GcStat.memory_allocation_approx_expensive (stat, gc_control) in
    List.rev_append rss gcstat
  in
  SlowReporter.iter_of_fold (SlowReporter.report ~interval_s:150. ~generate_dss)

let generate_stats_exn () =
  let status = Proc.Fields.status () in
  let gc_stat = Gc.quick_stat () in
  let gc_control = Gc.get () in
  let derived = Derived.memextra_kib status (gc_stat, gc_control) in
  let gcstat = GcStat.memory_allocation_precise (gc_stat, gc_control) in
  let is_slow, slow_stats = generate_expensive_stats () in
  let stats =
    derived :: List.concat [gcstat; Proc.to_list status; slow_stats]
  in
  if is_slow then
    observe_stats stats ;
  stats |> List.rev_map (fun x -> (Rrd.Host, x))

let generate_stats () =
  try generate_stats_exn ()
  with e ->
    D.log_backtrace () ;
    D.debug "Failed to generate stats: %s" (Printexc.to_string e) ;
    []

(* xapi currently exports 5 datasources if a slave or 7 if a master; this
 * comfortably fits into a single page. *)
let shared_page_count = 1

let start () = reporter_async ~shared_page_count ~dss_f:generate_stats

let stop reporter = Rrdd_plugin.Reporter.cancel ~reporter
