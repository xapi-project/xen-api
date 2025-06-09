(*
 * Copyright (C) Cloud Software Group
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

open Bechamel
open Toolkit

(* Bechamel doesn't provide before/after hooks, just allocate/free, but those are done outside the place where
   Bechamel checks for GC live words stabilization.
*)
let before_after ~before ~after ~get ~label ~unit =
  let shared_state = Atomic.make None and called = Atomic.make 0 in
  let module BeforeAfter = struct
    type witness = int Atomic.t

    let make () = Atomic.make 0

    let load t = Atomic.set t 0

    let unload _ = ()

    let label _ = label

    let unit _ = unit

    let get _ =
      (*
        We get added to the instances both at the beginning and the end, so we get called 4 times:

        get () - 0: None -> state := before ()
        time ()
        get () - 1
        
        benchmark_loop ()

        get () - 2
        time ()
        get () - 3, after state, state := None

        We want the time measurement to be as close to the benchmark loop as possible,
        so we perform operations only on call 1 and 4
      *)
      let phase = Atomic.fetch_and_add called 1 mod 4 in
      let old = Atomic.get shared_state in
      match (old, phase) with
      | None, 0 ->
          before () |> Option.some |> Atomic.set shared_state ;
          0.
      | Some state, (1 | 2) ->
          get state
      | Some state, 3 ->
          let r = get state in
          Atomic.set shared_state None ;
          after state ;
          r
      | None, _ ->
          assert false
      | Some _, _ ->
          assert false
  end in
  let measure = Measure.register (module BeforeAfter) in
  Measure.instance (module BeforeAfter) measure

let skip_label = "workload"

let thread_workload ~before ~run ~after =
  let before () =
    let state = before ()
    and stop = Atomic.make false
    and loops = Atomic.make 0 in
    let thread_worker () =
      while not (Atomic.get stop) do
        Sys.opaque_identity (run state : unit) ;
        Atomic.incr loops
      done
    in
    let t = Thread.create thread_worker () in
    (state, stop, loops, t)
  and after (state, stop, _loops, worker) =
    Atomic.set stop true ; Thread.join worker ; after state
  and get (_, _, loops, _) = Atomic.fetch_and_add loops 1 |> float_of_int in
  before_after ~before ~after ~get ~label:skip_label ~unit:"loops"

(* based on bechamel example code *)

(* For very short benchmarks ensure that they get to run long enough to switch threads
   a few times.
    Bechamel has both an iteration count and time limit, so this won't be a problem for slower benchmarks.
*)
let default_limit = 10_000_000

let benchmark ~instances cfg tests =
  let n = List.length tests in
  tests
  |> List.to_seq
  |> Seq.mapi (fun i test ->
         let name = Test.Elt.name test in
         Format.eprintf "Running benchmark %u/%u %s ...@?" (i + 1) n name ;
         let results = Benchmark.run cfg instances test in
         Format.eprintf "@." ; (name, results)
     )
  |> Hashtbl.of_seq

let analyze ~instances raw_results =
  let ols ~bootstrap =
    Analyze.ols ~bootstrap ~r_square:true ~predictors:[|Measure.run|]
  in
  let results =
    List.map
      (fun instance ->
        let f bootstrap = Analyze.all (ols ~bootstrap) instance raw_results in
        try f 3000 with _ -> f 0
      )
      instances
  in
  (Analyze.merge (ols ~bootstrap:3000) instances results, raw_results)

open Notty_unix

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let not_workload measure = not (Measure.label measure = skip_label)

let run_and_print cfg instances tests =
  let results, raw_results =
    tests
    |> benchmark ~instances cfg
    |> analyze ~instances:(List.filter not_workload instances)
  in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) ->
        {Bechamel_notty.w; h}
    | None ->
        {Bechamel_notty.w= 80; h= 1}
  in
  img (window, results) |> eol |> output_image ;
  results
  |> Hashtbl.iter (fun label results ->
         if label = Measure.label Instance.monotonic_clock then
           let units = Bechamel_notty.Unit.unit_of_label label in
           results
           |> Hashtbl.iter @@ fun name ols ->
              Format.printf "%s (%s):@, %a@." name units Analyze.OLS.pp ols
     ) ;
  (results, raw_results)

let cli ~always ~workloads cfg tests store =
  let instances =
    always
    @ Instance.[monotonic_clock; minor_allocated; major_allocated]
    @ always
  in
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances ;
  Format.eprintf "@,Running benchmarks (no workloads)@." ;
  let _, raw_results = run_and_print cfg instances tests in
  if workloads <> [] then (
    Format.eprintf "@,Running benchmarks (workloads)@." ;
    List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) workloads ;
    (* workloads come first, so that we unpause them in time *)
    let instances = workloads @ instances @ workloads in
    let _, _ = run_and_print cfg instances tests in
    ()
  ) ;
  store
  |> Option.iter @@ fun dir ->
     let epoch = Unix.gettimeofday () in
     raw_results
     |> Hashtbl.iter @@ fun label results ->
        let label = String.map (function '/' -> '_' | c -> c) label in
        let dir = Filename.concat dir (Float.to_string epoch) in
        let () =
          try Unix.mkdir dir 0o700
          with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        in

        let file = Filename.concat dir (label ^ ".dat") in
        Out_channel.with_open_text file @@ fun out ->
        let label = Measure.label Instance.monotonic_clock in
        results.Benchmark.lr
        |> Array.iter @@ fun measurement ->
           let repeat = Measurement_raw.run measurement in
           let avg = Measurement_raw.get ~label measurement /. repeat in
           (* ministat wants to compare individual measurements, but all we have is a sum. *)
           Printf.fprintf out "%.16g\n" avg

open Cmdliner

let cli ?(always = []) ?(workloads = []) tests =
  let tests = List.concat_map Test.elements tests in
  let cmd =
    let test_names = tests |> List.map (fun t -> (Test.Elt.name t, t)) in
    let filtered =
      let doc =
        Printf.sprintf "Choose the benchmarks to run. $(docv) must be %s"
          Arg.(doc_alts_enum test_names)
      in
      Arg.(
        value
        & pos_all (enum test_names) tests
        & info [] ~absent:"all" ~doc ~docv:"BENCHMARK"
      )
    and cfg =
      let open Term.Syntax in
      let+ limit =
        Arg.(
          value
          & opt int default_limit
          & info ["limit"] ~doc:"Maximum number of samples" ~docv:"SAMPLES"
        )
      and+ quota =
        Arg.(
          value
          & opt float 10.0 (* 1s is too short to reach high batch sizes *)
          & info ["quota"] ~doc:"Maximum time per benchmark" ~docv:"SECONDS"
        )
      and+ kde =
        Arg.(
          value
          & opt (some int) None
          & info ["kde"] ~doc:"Additional samples for Kernel Density Estimation"
              ~docv:"SAMPLES"
        )
      and+ stabilize =
        Arg.(
          value
          & opt bool false
          & info ["stabilize"] ~doc:"Stabilize the GC between measurements"
          (* this actually makes measurements more noisy, not less
             (although there'll be the ocasional outlier).
             When stabilization is disabled we can instead get more measurements within the same amount of time,
             which ultimately increases accuracy.
             core_bench also has this disabled by default
          *)
        )
      and+ compaction =
        Arg.(
          value
          & opt bool false
          (* avoid large differences between runs (since we no longer stabilize the GC) *)
          & info ["compaction"] ~doc:"Enable GC compaction"
        )
      and+ start =
        Arg.(
          value
          & opt int 5 (* small batches can have higher overhead: skip them *)
          & info ["start"] ~doc:"Starting iteration count" ~docv:"COUNT"
        )
      in
      Benchmark.cfg ~limit
        ~quota:Time.(second quota)
        ~kde ~stabilize ~compaction ~start ()
    and store =
      Arg.(
        value
        & opt (some dir) None
        & info ["output-dir"; "d"]
            ~doc:
              "directory to save the raw results to. The output can be used by \
               ministat"
            ~docv:"DIRECTORY"
      )
    in
    let info = Cmd.info "benchmark" ~doc:"Run benchmarks" in
    Cmd.v info Term.(const (cli ~always ~workloads) $ cfg $ filtered $ store)
  in
  exit (Cmd.eval cmd)
