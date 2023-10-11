(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(**
  {!mod:Alcotest} support for {!mod:Bechamel}.  
*)

type results = (string, (string, Bechamel.Analyze.OLS.t) Hashtbl.t) Hashtbl.t

type raw_results = (string, Bechamel.Benchmark.t) Hashtbl.t

val of_bechamel :
  ?derived_measures:(Bechamel.Measure.witness * Bechamel.Measure.witness * string * string * (float -> float -> float)) list
  ->  process_results:(Ezbechamel_cli.t -> results -> raw_results -> unit)
  -> Bechamel.Test.t
  -> Ezbechamel_cli.t Alcotest.V1.test
(** [of_bechamel ~process_results benchmarks] is an {!mod:Alcotest} definition to run [benchmarks].

  @param process_results is called with the results when all the benchmarks have finished
  @see {Ezbechamel_alcotest_notty.run}
 *)
