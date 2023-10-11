(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Bechamel

let raw_results = Hashtbl.create 47

open Alcotest.V1

type results = (string, (string, Bechamel.Analyze.OLS.t) Hashtbl.t) Hashtbl.t

type raw_results = (string, Bechamel.Benchmark.t) Hashtbl.t

let registered = ref false

module StringSet = Set.Make(String)

let process_derived measures derived_measures raw_results =
  let derived_labels = derived_measures |> List.fold_left (fun acc (m1, m2, _, _, _) ->
     acc |>
    StringSet.add (Measure.label m1)
    |>
    StringSet.add (Measure.label m2)
  ) StringSet.empty in
  let new_measures = derived_measures |> List.fold_left (fun acc (_, _, label, u, _) ->
      let module M = struct
        type witness = unit
        let label () = label
        let unit () = u
        let make () = ()
        let load () = ()
        let unload () = ()
        let get () = assert false
      end in
      let derived = Measure.register (module M) in
      Measure.instance (module M) derived :: acc) []
  in
  let () = raw_results |> Hashtbl.filter_map_inplace @@ fun _ bench ->
  let update_measurement raw =
    let labeled_measurements =
      measures |> List.to_seq
      |> Seq.map (fun m -> let label = Measure.label m in label, Measurement_raw.get ~label raw)
      |> Hashtbl.of_seq
    in
    let () =
      derived_measures |> List.iter @@ fun (m1, m2, derived, _, compute) ->
      let l1 = Measure.label m1
      and l2 = Measure.label m2 in
      let v1 = Hashtbl.find labeled_measurements l1
      and v2 = Hashtbl.find labeled_measurements l2 in
      let v = compute v1 v2 in
      Hashtbl.remove labeled_measurements l1;
      Hashtbl.remove labeled_measurements l2;
      Hashtbl.add labeled_measurements derived v
    in
    let labels = Hashtbl.to_seq_keys labeled_measurements |> Array.of_seq in
    let measures = Array.map (Hashtbl.find labeled_measurements ) labels in
    Measurement_raw.make ~measures ~labels (Measurement_raw.run raw)
  in
  let update_measurements ma =
    Array.map update_measurement ma
  in
  Some Benchmark.{ bench with lr = update_measurements bench.lr; kde = Option.map update_measurements bench.kde}
  in
  List.rev_append new_measures (List.filter (fun m -> not @@ StringSet.mem (Measure.label m) derived_labels) measures),
  raw_results

let of_bechamel ?(derived_measures=[]) ~process_results tests =
  let open Ezbechamel_cli in
  let name = Bechamel.Test.name tests in
  (* Due to how filtering works in Alcotest we can only access the results using [at_exit].
     Even if we'd set [~and_exit:false] in {!Alcotest.run_with_args} it'd still exit when test filtering is used.
  *)
  let results_at_exit t () =
    let measures, raw_results = process_derived t.measures derived_measures raw_results in
    let results =
      List.map
        (fun measure -> Analyze.all t.analysis measure raw_results)
        measures
    in
    process_results t (Analyze.merge t.analysis measures results) raw_results
  in
  let register_if_needed t =
    if not !registered then (
      at_exit (results_at_exit t) ;
      registered := true
    )
  in
  let tests =
    tests
    |> Test.elements
    |> List.map @@ fun test ->
       let name = Test.Elt.name test in
       test_case name `Slow @@ fun t ->
       register_if_needed t ;
       Hashtbl.add raw_results name @@ Benchmark.run t.cfg t.measures test
  in
  (name, tests)
