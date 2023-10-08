(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(**
  {!mod:Cmdliner} support for {!mod:Bechamel}.  
*)

open Bechamel
open Cmdliner

(** benchmark output to the terminal or plain text *)
type output = Tty | Text

type t = {
    cfg: Benchmark.configuration
  ; measures: Measure.witness list
  ; analysis: Analyze.OLS.t Analyze.t
  ; output: output
  ; json_file: string
}

val cli : t Term.t
(** [cli] is a term for configuring how to run and analyze {!mod:Bechamel} benchmarks. *)
