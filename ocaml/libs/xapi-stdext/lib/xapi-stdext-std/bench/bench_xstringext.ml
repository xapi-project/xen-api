open Bechamel
open Toolkit
module XString = Xapi_stdext_std.Xstringext.String

(* Test data generators *)
let make_string len = String.init len (fun i -> Char.chr (33 + (i mod 94)))

let escape_rules =
  [('a', "[A]"); ('e', "[E]"); ('i', "[I]"); ('o', "[O]"); ('u', "[U]")]

(* Reference implementation from xstringext_test.ml *)
let escaped_spec ?rules string =
  match rules with
  | None ->
      String.escaped string
  | Some rules ->
      let apply_rules char =
        match List.assoc_opt char rules with
        | None ->
            Seq.return char
        | Some replacement ->
            String.to_seq replacement
      in
      string |> String.to_seq |> Seq.concat_map apply_rules |> String.of_seq

let escaped_benchmark n =
  let s = make_string n in
  Staged.stage @@ fun () -> ignore (XString.escaped ~rules:escape_rules s)

let escaped_spec_benchmark n =
  let s = make_string n in
  Staged.stage @@ fun () -> ignore (escaped_spec ~rules:escape_rules s)

let test_escaped =
  Test.make_indexed ~name:"escaped" ~fmt:"%s %d" ~args:[100; 500; 1000]
    escaped_benchmark

let test_escaped_spec =
  Test.make_indexed ~name:"escaped-spec" ~fmt:"%s %d" ~args:[100; 500; 1000]
    escaped_spec_benchmark

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|]
  in
  let instances =
    Instance.[minor_allocated; major_allocated; monotonic_clock]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let test =
    Test.make_grouped ~name:"escaped-comparison"
      [test_escaped; test_escaped_spec]
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  let all_results = benchmark () in
  let results, _ = all_results in

  (* Extract timing data from the actual benchmark results *)
  let result_groups =
    Hashtbl.fold
      (fun _ v a -> Hashtbl.fold (fun k v a -> (k, v) :: a) v [] :: a)
      results []
  in

  (* Find the monotonic-clock result group (timing data) *)
  let timing_group =
    match result_groups with _ :: _ :: timing :: _ -> Some timing | _ -> None
  in

  let get_timing test_name =
    match timing_group with
    | None ->
        None
    | Some group -> (
      match List.assoc_opt test_name group with
      | Some estimator -> (
          let estimates = Analyze.OLS.estimates estimator in
          match estimates with Some (x :: _) -> Some x | _ -> None
        )
      | None ->
          None
    )
  in

  Printf.printf "\n=== Performance Comparison: Optimized vs Reference ===\n\n" ;

  let sizes = ["100"; "500"; "1000"] in
  List.iter
    (fun size ->
      Printf.printf "String size %s:\n" size ;
      let opt_test = Printf.sprintf "escaped-comparison/escaped %s" size in
      let ref_test = Printf.sprintf "escaped-comparison/escaped-spec %s" size in
      match (get_timing opt_test, get_timing ref_test) with
      | Some opt_time, Some ref_time ->
          let improvement = (ref_time -. opt_time) /. ref_time *. 100.0 in
          Printf.printf "  Optimized: %.3f μs\n" opt_time ;
          Printf.printf "  Reference: %.3f μs\n" ref_time ;
          Printf.printf "  Improvement: %.1f%% %s\n\n" improvement
            (if improvement > 0.0 then "faster" else "slower")
      | None, _ ->
          Printf.printf "  Optimized implementation data missing\n\n"
      | _, None ->
          Printf.printf "  Reference implementation data missing\n\n"
    )
    sizes ;

  Printf.printf "\n=== Detailed Results ===\n" ;
  match result_groups with
  | [results] ->
      let print (k, ols) = Fmt.pr "%s: %a\n%!" k Analyze.OLS.pp ols in
      List.iter print results
  | results_list ->
      Printf.printf "Results structure: %d result groups\n"
        (List.length results_list) ;
      List.iteri
        (fun i results ->
          Printf.printf "Result group %d:\n" i ;
          let print (k, ols) = Fmt.pr "  %s: %a\n%!" k Analyze.OLS.pp ols in
          List.iter print results
        )
        results_list
