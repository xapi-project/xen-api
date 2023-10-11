(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(**
  {!mod:Alcotest} support for {!mod:Bechamel} using {!mod:Notty} and {!mod:Cmdliner}.  
*)

open Bechamel

val run :
 ?measures:Bechamel.Measure.witness list
 -> ?derived_measures:(Measure.witness * Measure.witness * string * string * (float -> float -> float)) list
 -> Bechamel.Test.t list -> unit
(** [run ?derived_measures ?measures benchmarks] runs the [benchmarks].
  The benchmarks are configured using {!mod:Ezbechamel_cli}, converted to {!mod:Alcotest} with {!mod:Ezbechamel_alcotest},
  and the results printed using {!mod:Bechamel_notty} and {!mod:Bechamel_js}.

  The JSON results are always saved for further analysis (e.g. using `bechamel-html`).
*)
