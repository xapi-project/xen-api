(* based on bechamel example code *)
open Bechamel
open Toolkit

let instances = Instance.[monotonic_clock; minor_allocated; major_allocated]

let benchmark tests =
  let cfg = Benchmark.cfg () in
  Benchmark.all cfg instances tests

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:[|Measure.run|]
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  (Analyze.merge ols instances results, raw_results)

let () =
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let cli tests =
  Format.printf "@,Running benchmarks@." ;
  let results, _ = tests |> benchmark |> analyze in
  (* compute speed from duration *)
  let () =
    Hashtbl.find results (Measure.label Instance.monotonic_clock)
    |> Hashtbl.iter @@ fun name result ->
       try
         (* this relies on extracting input size from test name,
            which works if Test.make_indexed* was used *)
         Scanf.sscanf name "%_s@:%d" @@ fun length ->
         match Analyze.OLS.estimates result with
         | Some [duration] ->
             (* unit is ns *)
             let speed = 1e9 *. float length /. duration /. 1048576.0 in
             Fmt.pf Fmt.stdout "@[%s = %.1f MiB/s@]@." name speed
         | _ ->
             ()
       with Failure _ | Scanf.Scan_failure _ -> ()
  in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image
