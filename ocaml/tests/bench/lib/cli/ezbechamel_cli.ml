(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Bechamel
open Toolkit
open Cmdliner

let span =
  let parse s =
    try Ok (s |> Float.of_string |> Bechamel.Time.second)
    with e -> Error (Printexc.to_string e)
  in
  let print ppf t =
    Format.fprintf ppf "%fs"
      ((t |> Bechamel.Time.span_to_uint64_ns |> Int64.to_float) *. 1e-9)
  in
  Arg.conv' ~docv:"SECONDS" (parse, print)

let all_measures =
  Instance.
    [
      minor_allocated
    ; major_allocated
    ; promoted
    ; compaction
    ; minor_collection
    ; major_collection
    ; monotonic_clock
    ]

let default_measures =
  Instance.[monotonic_clock; minor_allocated; major_allocated]

let measures =
  let witness =
    (* cannot add the perf measures because they instantly fail when not root even when we're not using them,
       just referencing their name causes the module to be initialized
    *)
    all_measures |> List.map (fun w -> (Measure.label w, w)) |> Arg.enum
  in
  Arg.(
    value
    & opt (list witness) default_measures
    & info ["measures"] ~doc:"What measurements to record" ~docv:"MEASURE"
  )

let cfg =
  let limit =
    Arg.(
      value
      & opt (some int) None
      & info ["limit"] ~doc:"maximum number of samples allowed" ~docv:"SAMPLES"
    )
  and quota =
    Arg.(
      value
      & opt span (Time.second 5.0)
      & info ["quota"] ~doc:"maximum time allowed" ~docv:"SECONDS"
    )
  and kde =
    Arg.(
      value
      & opt (some int) None
      & info ["kde"]
          ~doc:
            "additional measurements for KDE and histogram display. Actual \
             time limit will be 2*quota"
    )
  and stabilize =
    Arg.(
      value
      & opt bool false
      & info ["stabilize"]
          ~doc:
            "stabilize the GC before each run. Beware that although \
             measurements will be more stable they may sometimes slow down the \
             measured value 10x."
    )
  and start =
    Arg.(
      value
      & opt (some int) None
      & info ["start"] ~doc:"the first value of the run metric"
    )
  and compaction =
    Arg.(
      value
      & opt bool false
      & info ["compaction"]
          ~doc:"whether to enable GC compaction during the benchmark"
    )
  in
  let cfg limit quota kde stabilize start compaction =
    Benchmark.cfg ?limit ~quota ~kde ~stabilize ?start ~compaction ()
  in
  Term.(const cfg $ limit $ quota $ kde $ stabilize $ start $ compaction)

let analysis =
  let bootstrap =
    Arg.(
      value
      & opt int 0
      & info ["bootstrap"]
          ~doc:
            "how many times to resample the measurements (needed for \
             confidence interval calculations)"
    )
  in
  let analyze bootstrap =
    Analyze.ols ~r_square:true ~bootstrap ~predictors:[|Measure.run|]
  in
  Term.(const analyze $ bootstrap)

type output = Tty | Text

let output =
  Arg.(value & opt (enum [("tty", Tty); ("text", Text)]) Tty & info ["output"])

let json_file =
  let self = Sys.executable_name |> Filename.basename in
  Arg.(
    value
    & opt string (self ^ ".json")
    & info ["output-json"] ~doc:"JSON file to write results to"
  )

type t = {
    cfg: Benchmark.configuration
  ; measures: Measure.witness list
  ; analysis: Analyze.OLS.t Analyze.t
  ; output: output
  ; json_file: string
}

let cli =
  let f cfg measures analysis output json_file =
    {cfg; measures; analysis; output; json_file}
  in
  Term.(const f $ cfg $ measures $ analysis $ output $ json_file)
