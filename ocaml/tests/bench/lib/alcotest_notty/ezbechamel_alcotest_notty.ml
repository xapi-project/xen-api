(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Bechamel

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let dump_hashtbl name pp_elt =
  Fmt.Dump.iter_bindings Hashtbl.iter Fmt.(any name) Fmt.string pp_elt

let dump_measure ppf m =
  Format.fprintf ppf "%s: %s" (Measure.label m) (Measure.unit m)

open Ezbechamel_cli

(* this cannot be a Bechamel.Measure, because speed can't be averaged arithmetically,
   would need to use a geometric mean
 *)
let output_notty t results =
  let open Notty_unix in
  let () =
    List.iter (fun i ->
      Bechamel_notty.Unit.add i (Measure.unit i)
    ) t.measures
  in

  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image

let output_text t results =
  let time_measure, count_measure =
    List.fold_left (fun (time_measure, count_measure) i ->
      let u = Measure.unit i in
      match u with
      | "ns" -> (Some i, count_measure)
      | "count" -> (time_measure, Some i)
      | _ -> (time_measure, count_measure)
    ) (None, None) t.measures
  in

  Format.printf "Measures: %a@." Fmt.Dump.(list dump_measure) t.measures ;
  Format.printf "%a@."
    (dump_hashtbl "results" (dump_hashtbl "result" Analyze.OLS.pp))
    results;
  match time_measure, count_measure with
  | Some t, Some c ->
    let times = Hashtbl.find results (Measure.label t)
    and counts = Hashtbl.find results (Measure.label c) in
    (* TODO: should be able to query Ci95 from OLS.t *)
    Hashtbl.iter (fun k ns ->
      let ns = Analyze.OLS.estimates ns |> Option.get |> List.hd in
      let count = Hashtbl.find counts k |> Analyze.OLS.estimates |> Option.get |> List.hd in
      Format.printf "%s = %.3f operations/s@." k (count /. (ns *. 1e-9))
    ) times;
  | _ -> ()

let save_to_file t results raw_results =
  let open Ezbechamel_cli in
  let file = open_out t.json_file in
  let finally () = close_out file in
  Fun.protect ~finally @@ fun () ->
  let open Bechamel_js in
  match
    emit ~dst:(Channel file)
      (fun _ -> Ok ())
      ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Bechamel.Toolkit.Instance.monotonic_clock)
      (results, raw_results)
  with
  | Ok () ->
      Format.printf "Saved JSON results to %s@." t.json_file
  | Error (`Msg err) ->
      invalid_arg err

let process_results t results raw_results =
  let output =
    match t.output with Tty -> output_notty | Text -> output_text
  in
  Format.pp_print_flush Format.std_formatter () ;
  output t results ;
  save_to_file t results raw_results

let run ?(measures=Ezbechamel_cli.default_measures) tests =
  let self = Sys.executable_name |> Filename.basename in
  let alcotest =
    List.map (Ezbechamel_alcotest.of_bechamel ~process_results) tests
  in
  Alcotest.V1.run_with_args ~show_errors:true self (Ezbechamel_cli.cli measures) alcotest
