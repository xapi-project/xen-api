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
let limit = 10_000_000

let benchmark ~instances tests =
  let cfg = Benchmark.cfg ~limit ~quota:(Time.second 10.0) () in
  Benchmark.all cfg instances tests

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
  |> eol

let not_workload measure = not (Measure.label measure = skip_label)

let run_and_print instances tests =
  let results, _ =
    tests
    |> benchmark ~instances
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
  |> Hashtbl.iter @@ fun label results ->
     if label = Measure.label Instance.monotonic_clock then
       let units = Bechamel_notty.Unit.unit_of_label label in
       results
       |> Hashtbl.iter @@ fun name ols ->
          Format.printf "%s (%s):@, %a@." name units Analyze.OLS.pp ols

let cli ?(always = []) ?(workloads = []) tests =
  let instances =
    always
    @ Instance.[monotonic_clock; minor_allocated; major_allocated]
    @ always
  in
  List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances ;
  Format.printf "@,Running benchmarks (no workloads)@." ;
  run_and_print instances tests ;

  if workloads <> [] then (
    Format.printf "@,Running benchmarks (workloads)@." ;
    List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) workloads ;
    (* workloads come first, so that we unpause them in time *)
    let instances = workloads @ instances @ workloads in
    run_and_print instances tests
  )
