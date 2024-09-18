open Bechamel

let ( let@ ) f x = f x

(* TODO: before *)

let trace_test_inner span =
  let@ span =
    Tracing.with_child_trace
      ~attributes:[("foo", "testing")]
      span ~name:__FUNCTION__
  in
  let@ _ =
    Tracing.with_child_trace ~attributes:[("bar", "val")] span ~name:"test"
  in
  Sys.opaque_identity ignore ()

let trace_test_span _ = Tracing.with_tracing ~name:__FUNCTION__ trace_test_inner

let trace_test_off _ = trace_test_inner None

let uuid = "TEST"

let export_thread =
  (* need to ensure this isn't running outside the benchmarked section,
     or bechamel might fail with 'Failed to stabilize GC'
  *)
  let after _ = Tracing_export.flush_and_exit () in
  Bechamel_simple_cli.thread_workload ~before:Tracing_export.main ~after
    ~run:ignore

let workload1 =
  Bechamel_simple_cli.thread_workload ~before:ignore ~after:ignore
    ~run:trace_test_span

let create_gc_work =
  let a = Array.make 1_000 "" in
  fun () ->
    (* create work for the GC by continously creating a lot of short lived strings *)
    Sys.opaque_identity (Array.iteri (fun i _ -> a.(i) <- String.make 2 'x') a)

let workload2 =
  Bechamel_simple_cli.thread_workload ~before:ignore ~after:ignore
    ~run:create_gc_work

let workloads = [workload1; workload2]

let allocate () =
  Tracing.TracerProvider.create ~enabled:true ~attributes:[] ~endpoints:[]
    ~name_label:__MODULE__ ~uuid ;
  Tracing_export.main ()

let free t =
  Tracing.TracerProvider.destroy ~uuid ;
  Tracing_export.flush_and_exit () ;
  Thread.join t

let test_tracing_on ?(overflow = false) ~name f =
  let allocate () =
    if overflow then (
      Tracing.Spans.set_max_spans 10 ;
      Tracing.Spans.set_max_traces 10
    ) ;
    allocate ()
  and free t =
    if overflow then (
      Tracing.Spans.set_max_spans Bechamel_simple_cli.limit ;
      Tracing.Spans.set_max_traces Bechamel_simple_cli.limit
    ) ;
    free t
  in
  Test.make_with_resource ~name ~allocate ~free Test.uniq f

let benchmarks =
  Tracing.Spans.set_max_spans Bechamel_simple_cli.limit ;
  Tracing.Spans.set_max_traces Bechamel_simple_cli.limit ;
  Test.make_grouped ~name:"tracing"
    [
      Test.make ~name:"overhead(off)" (Staged.stage trace_test_off)
    ; test_tracing_on ~name:"overhead(on, no span)" (Staged.stage trace_test_off)
    ; test_tracing_on ~name:"overhead(on, create span)"
        (Staged.stage trace_test_span)
    ; test_tracing_on ~overflow:true ~name:"max span overflow"
        (Staged.stage trace_test_span)
    ]

let () = Bechamel_simple_cli.cli ~always:[export_thread] ~workloads benchmarks
