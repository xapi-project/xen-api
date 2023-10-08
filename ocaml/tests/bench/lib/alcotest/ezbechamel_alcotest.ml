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

let of_bechamel ~process_results tests =
  let open Ezbechamel_cli in
  let name = Bechamel.Test.name tests in
  (* Due to how filtering works in Alcotest we can only access the results using [at_exit].
     Even if we'd set [~and_exit:false] in {!Alcotest.run_with_args} it'd still exit when test filtering is used.
  *)
  let results_at_exit t () =
    let results =
      List.map
        (fun measure -> Analyze.all t.analysis measure raw_results)
        t.measures
    in
    process_results t (Analyze.merge t.analysis t.measures results) raw_results
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
