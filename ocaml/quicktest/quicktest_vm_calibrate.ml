(*
+ * Copyright (c) Cloud Software Group, Inc
+ *
+ * This program is free software; you can redistribute it and/or modify
+ * it under the terms of the GNU Lesser General Public License as published
+ * by the Free Software Foundation; version 2.1 only. with the special
+ * exception on linking described in file LICENSE.
+ *
+ * This program is distributed in the hope that it will be useful,
+ * but WITHOUT ANY WARRANTY; without even the implied warranty of
+ * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
+ * GNU Lesser General Public License for more details.
+ *)

open Quicktest_api_helpers
open Client.Client
open Quicktest_trace
open Quicktest_trace_api
open Quicktest_trace_rpc

let div_round_up a b = Int64.(div (add a @@ pred b) b)

module type Variable = sig
  (** type of the variable *)
  type t

  val name : string
  (** name of the variable *)

  val set : client -> vm:API.ref_VM -> t -> unit
  (** [set client ~vm value] API call that sets the variable on [vm] to [value] *)

  val values : client -> host:API.ref_host -> vm:API.ref_VM -> t Seq.t
  (** [values client ~vm] is a sequence of valid values that
      can be used to calibrate the memory overhead for this variable.
   *)

  val to_int64 : t -> int64
  (** [to_int64 t] is the value of [t] as an integer that is used in memory
      overhead calculation.
      This is not necessarily the same as the raw value used by [set].
      *)

  val pp : t Fmt.t
  (** [pp ppf t] pretty prints the value of the variable [t] *)
end

let mib n = Int64.shift_left n 20

let bytes_to_pages bytes = Int64.div bytes @@ pagesize ()

let max_vms_per_host = 1000

let try_to_trigger_failure (type a) t ~host ~vm
    (module V : Variable with type t = a) (x : a) vms =
  if not !Quicktest_args.skip_stress then begin
    V.set t ~vm x ;
    let overhead = call t @@ VM.compute_memory_overhead ~vm
    and vm_mem = Api.VM.call_get t VM.get_memory_dynamic_min ~self:vm in
    let vm_total_mem = Int64.add overhead vm_mem in
    let free_mem = call t @@ Host.compute_free_memory ~host in
    let max_vms = Int64.div free_mem vm_total_mem |> Int64.to_int in
    (* not too many, we cannot have more VMs than would fit on the host,
       or more VMs than supported/host
     *)
    let max_vms = min (min vms max_vms) max_vms_per_host in
    if max_vms >= vms then begin
      Trace.with_ "Creating VMs" ~attrs:[("count", `Int max_vms)] @@ fun _ ->
      let vms = ensure_vm_clones t ~vm max_vms (V.name ^ "-trigger") in
      vms |> List.map (fun vm -> (host, vm)) |> start_vms t ;
      fill_mem_pow2 t ~host ~vm ;
      shutdown_vms t vms
    end
    (* can't create enough VMs to trigger the failure *)
  end

module Iterations = struct
  type t = float

  let to_float = Fun.id
end

module P = Cli_progress_bar.Make (Iterations)

let fill_mem_test rpc session_id template () =
  Trace.with_ __FUNCTION__ @@ fun _ ->
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm -> fill_mem_pow2 t ~host ~vm

let required_stable_iterations = 10

let iterations_limit = 100

let accepted_leak = mib 1L

let host_mem_leak rpc session_id template () =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm ->
  Api.VM.call_set t VM.set_memory ~self:vm ~value:(Int64.shift_left 1L 30) ;
  let p = P.create 80 0. 1. in
  let pages00 = stable_localhost_free_pages scope t ~host in

  let rec loop stable i =
    Api.VM.with_call t "assert_can_boot_here" vm
    @@ VM.assert_can_boot_here ~self:vm ~host ;
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make "assert_can_boot_here succeeded"
    ) ;

    let pages0 = stable_localhost_free_pages scope t ~host in

    Api.VM.with_call t "start_on" vm
    @@ VM.start_on ~vm ~host ~force:false ~start_paused:true ;
    Api.VM.with_call t "hard_shutdown" vm @@ VM.hard_shutdown ~vm ;

    let pages1 = stable_localhost_free_pages scope t ~host in
    let leak = Int64.sub pages0 pages1 in
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make
          ~attrs:
            [
              ("pages before", `Int (Int64.to_int pages0))
            ; ("pages after", `Int (Int64.to_int pages1))
            ; ("pages leak", `Int (Int64.to_int leak))
            ]
          "start/shutdown delta"
    ) ;
    let iterations =
      if leak > 0L then
        Int64.div pages1 leak |> Int64.to_int
      else
        0
    in
    let iterations_done = i in
    let iterations_total =
      iterations_done + iterations + (required_stable_iterations - stable)
    in
    if P.update p (float_of_int iterations_done /. float_of_int iterations_total)
    then
      Printf.eprintf "\r%s%!" P.(string_of_bar p) ;
    let total_leak = Int64.sub pages00 pages1 in
    if i > iterations_limit then begin
      Opentelemetry.(
        Logs.emit
          [
            Logs.make_strf ~severity:Severity_number_info
              "Total memory leak after %d iterations is %Ld pages" i total_leak
          ]
      ) ;
      if total_leak < accepted_leak then
        ()
      else
        (*  it may take a long time to
            actually OOM the host, but we've already proven the leak, so stop,
            otherwise the test will timeout! *)
        failwith "Host memory leak found!"
    end else if iterations > 0 then begin
      let () =
        Scope.add_log scope @@ fun () ->
        Opentelemetry.Logs.make_strf "Iterations to OOM: %d" iterations
      in
      (* we'll either OOM or stabilize (if it isn't a real leak, just a bounded
       growth) *)
      loop 0 (i + 1)
    end else if stable < required_stable_iterations then
      loop (stable + 1) (i + 1)
    else begin
      Printf.eprintf "\r%s\r%s%!" (String.make p.width ' ') P.(summarise p) ;
      if i > stable then
        Opentelemetry.(
          Logs.emit
            [
              Logs.make_strf ~severity:Severity_number_info
                "Apparent memory leak stopped after %d iterations, total \
                 apparent leak = %Ld pages"
                i total_leak
            ]
        )
    end
  in
  loop 0 0

let boot1 rpc session_id template (module V : Variable) () =
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm ->
  let value = V.values t ~host ~vm |> List.of_seq |> List.rev |> List.hd in
  (* set to largest: highest chance to find a bug *)
  V.set t ~vm value ;
  let total = call t @@ Host.compute_free_memory ~host in
  let value = call t @@ VM.maximise_memory ~self:vm ~approximate:false ~total in
  Trace.with_ __FUNCTION__
    ~attrs:
      [
        ("vm_memory", `Int (Int64.to_int value))
      ; ("fill_memory", `Int (Int64.to_int total))
      ]
  @@ fun scope ->
  Api.VM.call_set t VM.set_memory ~self:vm ~value ;

  Api.VM.with_call t "assert_can_boot_here" vm
  @@ VM.assert_can_boot_here ~self:vm ~host ;
  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make "assert_can_boot_here succeeded"
  ) ;

  start_vm t ~host ~vm ;

  Api.VM.with_call t "hard_shutdown" vm @@ VM.hard_shutdown ~vm

let calibrate rpc session_id template (module V : Variable) () =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template @@ fun vm ->
  (* start with a small VM, [module V] can override it *)
  let memory_min = Api.VM.call_get t VM.get_memory_static_min ~self:vm in
  Api.VM.call_set t VM.set_memory ~self:vm ~value:memory_min ;

  let actual_free_pages = stable_localhost_free_pages scope t ~host in
  let computed_free_pages =
    call t @@ Host.compute_free_memory ~host |> bytes_to_pages
  in
  let headroom_pages = Int64.sub actual_free_pages computed_free_pages in
  (* If XAPI overestimates there is this much extra space that can be used to
     cover for any underestimates.
     If this is negative then we can already run the system out of memory.
     *)
  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make
        ~attrs:
          [
            ("actual_free_pages_xen", `Int (Int64.to_int actual_free_pages))
          ; ("computed_free_pages_xapi", `Int (Int64.to_int computed_free_pages))
          ; ("free_headroom_pages", `Int (Int64.to_int headroom_pages))
          ]
        "host_free_mem"
  ) ;
  if headroom_pages < 0L then begin
    Opentelemetry.(
      Logs.emit
        [
          Logs.make_strf ~severity:Logs.Severity_number_warn
            "XAPI has already underestimated free memory: %Ld" headroom_pages
        ]
    ) ;
    Scope.set_status scope
      Span_status.(
        make ~message:"free memory underestimated" ~code:Status_code_error
      )
  end ;

  let measure_overhead_pages value =
    V.set t ~vm value ;
    let pages =
      with_measure_memory_pages scope t ~localhost:host @@ fun () ->
      let (_ : _ list) =
        Api.batched_run_or_cancel t "Start VM(s)/measure"
          [Api.VM.Async.start_on ~vm ~host ~force:false ~start_paused:true]
        |> check_tasks
      in
      ()
    in
    let memory_target_pages =
      call t @@ VM.get_memory_target ~self:vm |> bytes_to_pages
    and memory_overhead_pages_xapi =
      call t @@ VM.get_memory_overhead ~self:vm |> bytes_to_pages
    in
    Api.VM.with_call t "hard_shutdown" vm @@ VM.hard_shutdown ~vm ;
    let overhead_pages = Int64.sub pages memory_target_pages in
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make
          ~attrs:
            [
              ("variable", `String (Fmt.to_to_string V.pp value))
            ; ("unit", `String V.name)
            ; ("pages", `Int (Int64.to_int pages))
            ; ("memory_target_pages", `Int (Int64.to_int pages))
            ; ("overhead_pages", `Int (Int64.to_int overhead_pages))
            ; ( "xapi_estimated_pages"
              , `Int (Int64.to_int memory_overhead_pages_xapi)
              )
            ]
          "overhead"
    ) ;
    let diff = Int64.sub overhead_pages memory_overhead_pages_xapi in
    let vms_required =
      if diff > 0L then begin
        let vms = div_round_up headroom_pages diff in
        Opentelemetry.(
          Logs.emit
            [
              Logs.make_strf ~severity:Severity_number_warn
                "Memory overhead was underestimated by XAPI: %Ld < %Ld, diff: \
                 %Ld.@,\
                 VMs required for failure: %Ld"
                memory_overhead_pages_xapi overhead_pages diff vms
            ]
        ) ;
        Scope.set_status scope
          Span_status.(
            make ~message:"memory overhead underestimated"
              ~code:Status_code_error
          ) ;
        vms
      end else
        Int64.max_int
    in
    (value, overhead_pages, vms_required)
  in
  Opentelemetry.(
    Logs.emit
      [
        Logs.make_strf ~severity:Severity_number_info
          "Measuring VM %S impact on memory usage" V.name
      ]
  ) ;
  let overhead_pages =
    V.values t ~host ~vm |> Seq.map measure_overhead_pages |> List.of_seq
  in
  match overhead_pages with
  | [] ->
      assert false
  | (x0, y0, vms0) :: rest ->
      let x0 = V.to_int64 x0 in
      let deltas =
        rest
        |> List.map @@ fun (x, y, vms) ->
           let x = V.to_int64 x in
           let x = Int64.sub x x0 and y = Int64.sub y y0 in
           let ratio =
             if x > 0L then
               Int64.to_float y /. Int64.to_float x
             else
               0.
           in
           (x, y, ratio, vms)
      in
      Opentelemetry.(
        Logs.emit
          (Logs.make_strf ~severity:Severity_number_info
             "%s,memory_overhead_pages,coeff,vms" V.name
          :: (deltas
             |> List.map @@ fun (x, y, r, vms) ->
                Logs.make_strf ~severity:Severity_number_info "%Ld,%Ld,%g,%Ld" x
                  y r vms
             )
          )
      ) ;
      let max_coeff =
        List.fold_left (fun rmax (_, _, r, _) -> Float.max rmax r) 0. deltas
      and min_vms =
        List.fold_left
          (fun rmin (_, _, _, vms) -> Int64.min rmin vms)
          vms0 deltas
      in
      let op, max_coeff_int =
        if Float.round max_coeff >= 1. then
          ("*", Float.ceil max_coeff |> Int64.of_float)
        else
          ("/", Float.floor (1. /. max_coeff) |> Int64.of_float)
      in
      Opentelemetry.(
        Logs.emit
          [
            Logs.make_strf ~severity:Logs.Severity_number_info2
              "VM memory_overhead_pages = ... + %s * %g =~ ... + %s %s %Ld"
              V.name max_coeff V.name op max_coeff_int
          ]
      ) ;
      if min_vms < Int64.max_int then begin
        Opentelemetry.(
          Logs.emit
            [
              Logs.make_strf ~severity:Logs.Severity_number_warn
                "With %Ld VMs it might be possible to trigger OOM\n\
                \        error"
                min_vms
            ]
        ) ;
        if min_vms <= 1000L then
          Scope.set_status scope
            Span_status.(
              make ~message:"free memory\n          underestimated"
                ~code:Status_code_error
            ) ;
        let x, _, _ = overhead_pages |> List.rev |> List.hd in
        try_to_trigger_failure t ~host ~vm (module V) x (Int64.to_int min_vms)
      end

module VCPU = struct
  type t = int64

  let to_int64 = Fun.id

  let name = "vcpu"

  let pp = Fmt.int64

  let set t ~vm value =
    (* API is weird, there is no order of the 2 set calls that works for all
       values, must satisfy [0 < vCPUs_at_startup <= vCPUs_max],
       but there is no VM.set_vcpu call like VM.set_memory to set both in an
       atomic call.
       *)
    let old = Api.VM.call_get t VM.get_VCPUs_at_startup ~self:vm in
    if old <= value then begin
      (* increasing number of vCPUs: change max first *)
        Api.VM.call_set t VM.set_VCPUs_max ~self:vm ~value ;
        Api.VM.call_set t VM.set_VCPUs_at_startup ~self:vm ~value
    end else begin
      (* decreasing number of vcpus: change on startup first *)
        Api.VM.call_set t VM.set_VCPUs_at_startup ~self:vm ~value ;
        Api.VM.call_set t VM.set_VCPUs_max ~self:vm ~value
    end

  let values t ~host ~vm:_ =
    let limit =
      let host_cpus = call t @@ Host.get_host_CPUs ~self:host |> List.length in
      (* can't use more vCPUs than the host has, and we support a maximum of 64 *)
      min 64 host_cpus
    in
    let all = Seq.init limit (( + ) 1) |> Seq.map Int64.of_int in
    (* There is some non-determinism (due to alignment/fragmentation?), so test
     all values that we can, and then repeat it all again.
       This should allow us to find a better maximum.
     *)
    Seq.append all all
  (*    points_between 1L limit *)
end

module Pagetables = struct
  type t = MiB of int64

  let name = "pagetables"

  let to_bytes (MiB mib) = Int64.shift_left mib 20

  let entries_in_pagetable () =
    (* 4096 / 8 = 512 on x86-64 *)
    Int64.div (pagesize ()) (Sys.word_size / 8 |> Int64.of_int)

  let to_int64 t =
    let ( ++ ) = Int64.add in
    let bytes = to_bytes t in
    let pagesize = pagesize () in
    let entries_in_pagetable = entries_in_pagetable () in
    let pages = Int64.div bytes pagesize in
    (* pagetables for ranges of 2MiB, 1GiB, 512GiB, 256TiB, see
       https://wiki.osdev.org/Page_Tables#48-bit_virtual_address_space *)
    let pt = div_round_up pages entries_in_pagetable in
    let pd = div_round_up pt entries_in_pagetable in
    let pdp = div_round_up pd entries_in_pagetable in
    let pml4 = div_round_up pdp entries_in_pagetable in
    (* for future-proofing, we don't actually support this much memory *)
    let pml5 = div_round_up pml4 entries_in_pagetable in
    (* Assuming that you'd have to allocate as 1GiB pages initially,
       then shatter them to 2MiB pages, and then finally to 4KiB pages.
     *)
    let pagetables_4k = pt ++ pd ++ pdp ++ pml4 ++ pml5
    and pagetables_2m = pd ++ pdp ++ pml4 ++ pml5
    and pagetables_1g = pdp ++ pml4 ++ pml5 in
    Opentelemetry.(
      Logs.emit
        [
          Logs.make_strf ~severity:Severity_number_debug
            "%Ld bytes = %Ld pages;%Ld PT, %Ld PD, %Ld PDP, %Ld PML4, %Ld \
             PML5;%Ld pagesize pagetables = %Ld;%Ld pagesize pagetables = \
             %Ld;%Ld pagesize pagetables = %Ld"
            bytes pages pt pd pdp pml4 pml5 pagesize pagetables_4k
            Int64.(mul pagesize entries_in_pagetable)
            pagetables_2m
            Int64.(mul (mul pagesize entries_in_pagetable) entries_in_pagetable)
            pagetables_1g
        ]
    ) ;
    Int64.(pt ++ mul 2L pd ++ mul 3L pdp ++ mul 3L pml4 ++ mul 3L pml5)

  let of_bytes bytes = MiB (Int64.shift_right bytes 20)

  let set t ~vm mib = call t @@ VM.set_memory ~self:vm ~value:(to_bytes mib)

  let pp ppf (MiB mib as t) =
    Fmt.pf ppf "%Ld pagetables (for %Ld pages = %Ld MiB)" (to_int64 t)
      (Int64.div (to_bytes t) @@ pagesize ())
      mib

  let values t ~host ~vm =
    let total = call t @@ Host.compute_free_memory ~host in
    let value_max =
      call t @@ VM.maximise_memory ~total ~approximate:false ~self:vm
    in
    let start =
      [27; 28; 29; 30; 31; 39; 40]
      |> List.to_seq
      |> Seq.map (Int64.shift_left 1L)
      |> Seq.filter (fun v -> v < value_max)
    in
    Seq.append start Seq.(return value_max) |> Seq.map of_bytes
end

let specialise (name, speed, test) (module V : Variable) =
  let name = Printf.sprintf "%s: %s" name V.name in
  (name, speed, test (module V : Variable))

let variables = [(module VCPU : Variable); (module Pagetables : Variable)]

let tests_cleanup () =
  let open Qt_filter in
  [("Cleanup", `Slow, cleanup)] |> conn

let tests () =
  let open Qt_filter in
  List.concat
    [
      [("Fill mem 1VM", `Slow, boot1); ("VM memory overhead", `Slow, calibrate)]
      |> conn
      |> vm_template Qt.VM.Template.other
      |> List.concat_map (fun tc -> List.map (specialise tc) variables)
    ; [
        ("Fill mem pow2", `Slow, fill_mem_test)
      ; ("Host memory leak", `Slow, host_mem_leak)
      ; ("Fill mem pow2", `Slow, fill_mem_test)
      ]
      |> conn
      |> vm_template Qt.VM.Template.other
    ]
