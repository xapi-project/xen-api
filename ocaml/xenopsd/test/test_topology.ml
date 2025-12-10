open Topology

module D = Debug.Make (struct let name = "test_topology" end)

module Distances = struct
  type t = int * int array array

  let example numa : t =
    let distances =
      Array.init numa (fun i ->
          Array.init numa (fun j -> 10 + (11 * abs (j - i)))
      )
    in
    (numa, distances)

  let opteron : t =
    (* e.g. AMD Opteron 6272 *)
    let numa = 8 in
    let distances =
      [|
         [|10; 16; 16; 22; 16; 22; 16; 22|]
       ; [|16; 10; 22; 16; 16; 22; 22; 16|]
       ; [|16; 22; 10; 16; 16; 16; 16; 16|]
       ; [|22; 16; 16; 10; 16; 16; 22; 22|]
       ; [|16; 16; 16; 16; 10; 16; 16; 22|]
       ; [|22; 22; 16; 16; 16; 10; 22; 16|]
       ; [|16; 22; 16; 22; 16; 22; 10; 16|]
       ; [|22; 16; 16; 22; 22; 16; 16; 10|]
      |]
    in
    (numa, distances)

  (* A node without CPUs has this distance value ((2ˆ32) - 1) *)
  let empty = 0xFFFFFFFF

  let unreachable_last : t =
    let numa = 2 in
    let distances = [|[|10; empty|]; [|empty; empty|]|] in
    (numa, distances)

  let unreachable_middle : t =
    let numa = 3 in
    let distances =
      [|[|10; empty; 20|]; [|empty; empty; empty|]; [|20; empty; 10|]|]
    in
    (numa, distances)

  let unreachable_two : t =
    let numa = 3 in
    let distances =
      [|[|empty; empty; 20|]; [|empty; empty; empty|]; [|10; empty; 10|]|]
    in
    (numa, distances)

  let none_reachable : t =
    let numa = 2 in
    let distances = [|[|empty; empty|]; [|empty; empty|]|] in
    (numa, distances)
end

let make_numa_common ~logical_per_physical ~cores_per_numa
    (distances : Distances.t) =
  (* cores_per_numa refers to logical cores, i.e. cpus *)
  let numa, distances = distances in
  let cpu_to_node =
    Array.init (cores_per_numa * numa) (fun cpu -> cpu / cores_per_numa)
  and node_cores =
    (* core here refers to physical *)
    Array.init numa (fun _ -> cores_per_numa / logical_per_physical)
  in
  Option.map
    (fun d -> (cores_per_numa * numa, d))
    (NUMA.make ~distances ~cpu_to_node ~node_cores)

let make_numa ~numa ~cores =
  let cores_per_numa = cores / numa in
  match
    make_numa_common ~logical_per_physical:2 ~cores_per_numa
      (Distances.example numa)
  with
  | None ->
      Alcotest.fail "Synthetic matrix can't fail to load"
  | Some d ->
      d

let make_numa_amd ~cores_per_numa =
  match
    make_numa_common ~cores_per_numa ~logical_per_physical:2 Distances.opteron
  with
  | None ->
      Alcotest.fail "Synthetic matrix can't fail to load"
  | Some d ->
      d

type t = {worst: int; average: float; nodes: NUMA.node list; best: int}

let pp =
  Fmt.(
    Dump.record
      [
        Dump.field "worst" (fun t -> t.worst) int
      ; Dump.field "average" (fun t -> t.average) float
      ; Dump.field "nodes" (fun t -> t.nodes) (Dump.list NUMA.pp_dump_node)
      ; Dump.field "best" (fun t -> t.best) int
      ]
  )

let sum_costs l =
  D.debug "====" ;
  List.fold_left
    (fun accum cost ->
      {
        worst= max accum.worst cost.worst
      ; average= accum.average +. cost.average
      ; nodes= cost.nodes @ accum.nodes
      ; best= min accum.best cost.best
      }
    )
    {worst= min_int; average= 0.; nodes= []; best= max_int}
    l

let vm_access_costs host all_vms (vcpus, nodes, cpuset) =
  let nodes = List.of_seq nodes in
  let all_vms = ((vcpus, nodes), cpuset) :: all_vms in
  let n = List.length nodes in
  let costs =
    cpuset
    |> CPUSet.elements
    |> List.map (fun c ->
           let distances =
             List.map
               (fun node -> NUMA.distance host (NUMA.node_of_cpu host c) node)
               nodes
           in
           let worst = List.fold_left max 0 distances in
           let best = List.fold_left min max_int distances in
           let average = float (List.fold_left ( + ) 0 distances) /. float n in
           {worst; best; nodes= []; average}
       )
    |> sum_costs
  in
  D.debug "Costs: %s" (Fmt.to_to_string pp costs) ;
  let cpus = float @@ CPUSet.cardinal cpuset in
  let nodes = all_vms |> List.concat_map (fun ((_, nodes), _) -> nodes) in
  {costs with average= costs.average /. cpus; nodes}

let cost_not_worse ~default c =
  let worst = max default.worst c.worst in
  let best = min default.best c.best in
  let average = min default.average c.average in
  D.debug "Default access times: %s; New plan: %s"
    (Fmt.to_to_string pp default)
    (Fmt.to_to_string pp c) ;
  Alcotest.(
    check int "The worst-case access time must not be changed from default"
      default.worst worst
  ) ;
  Alcotest.(check int "Best case access time must not change" best c.best) ;
  Alcotest.(
    check (float 1e-3)
      "Average access times could improve, but must not be worse" average
      c.average
  ) ;
  if c.best < default.best then
    D.debug "The new plan has improved the best-case access time!" ;
  if c.worst < default.worst then
    D.debug "The new plan has improved the worst-case access time!" ;
  if c.average < default.average then
    D.debug "The new plan has improved the average access time!"

let balancing nodes ~vms =
  (* We expect to use many NUMA nodes when we have more VMs, more elaborate
     checks could be done on how well balanced the VMs are across NUMA nodes.
     Though in this case the default would always be best since it uses all
     nodes. *)
  let nodes_used = List.sort_uniq compare nodes |> List.length in
  min vms nodes_used |> float

let check_aggregate_costs_not_worse (default, next, plans) ~cores ~vms =
  let default = sum_costs default in
  let next = sum_costs next in
  cost_not_worse ~default next ;
  let balancing_default = balancing default.nodes ~vms in
  let balancing_next = balancing next.nodes ~vms in
  let balancing_best = max balancing_next balancing_default in
  Alcotest.(
    check (float 1e-3) "Balancing could improve" balancing_best balancing_next
  ) ;
  if balancing_next > balancing_default then D.debug "Balancing has improved!" ;
  let used_cpus =
    plans
    |> List.map snd
    |> List.fold_left CPUSet.union CPUSet.empty
    |> CPUSet.cardinal
  in
  Alcotest.(check int "All vCPUs are used " cores used_cpus)

let default_mem = Int64.shift_left 1L 30

(* higher than default_mem *)
let mem3 = Int64.div (Int64.mul 4L (Int64.shift_left 1L 34)) 3L

let test_allocate ?(mem = default_mem) (expected_cores, h) ~vms () =
  let memsize = Int64.shift_left 1L 34 in
  let nodes =
    NUMA.nodes h
    |> List.of_seq
    |> List.map (fun n -> NUMA.resource h n ~memory:memsize)
    |> Array.of_list
  in
  D.debug "NUMA: %s" (Fmt.to_to_string NUMA.pp_dump h) ;
  let cores = NUMA.all_cpus h |> CPUSet.cardinal in
  Alcotest.(check int "Core count matches" expected_cores cores) ;
  let vm_cores = max 2 (cores / vms) in
  List.init vms (fun i -> i + 1)
  |> List.fold_left
       (fun (costs_old, costs_new, plans) i ->
         D.debug "Planning VM %d" i ;
         let vm = NUMARequest.make ~memory:mem ~vcpus:vm_cores ~cores:0 in
         match Softaffinity.plan h nodes ~vm with
         | None ->
             Alcotest.fail "No NUMA plan"
         | Some (cpu_plan, mem_plan) ->
             D.debug
               "NUMA allocation succeeded for VM %d: [CPUS: %s]; [nodes: %s]" i
               (Fmt.to_to_string CPUSet.pp_dump cpu_plan)
               (Fmt.to_to_string Fmt.(Dump.list NUMA.pp_dump_node) mem_plan) ;
             let usednodes =
               cpu_plan
               |> CPUSet.elements
               |> List.map (NUMA.node_of_cpu h)
               |> List.sort_uniq compare
               |> List.to_seq
             in
             let costs_numa_aware =
               vm_access_costs h plans (vm_cores, usednodes, cpu_plan)
             in
             let costs_default =
               vm_access_costs h plans (vm_cores, NUMA.nodes h, NUMA.all_cpus h)
             in
             cost_not_worse ~default:costs_default costs_numa_aware ;
             ( costs_default :: costs_old
             , costs_numa_aware :: costs_new
             , ((vm_cores, List.of_seq usednodes), cpu_plan) :: plans
             )
       )
       ([], [], [])
  |> check_aggregate_costs_not_worse ~cores ~vms

let () = Printexc.record_backtrace true

let symmetric_specs =
  let make ~vms ~numa ~cores =
    let name =
      Printf.sprintf "Allocation of %d VM(s) on %d node(s), %d cores" vms numa
        cores
    in
    (name, vms, None, make_numa ~numa ~cores)
  in
  [
    make ~vms:1 ~numa:1 ~cores:2
  ; make ~vms:10 ~numa:1 ~cores:8
  ; make ~vms:1 ~numa:2 ~cores:4
  ; make ~vms:10 ~numa:2 ~cores:4
  ; make ~vms:1 ~numa:4 ~cores:16
  ; make ~vms:10 ~numa:4 ~cores:16
  ; make ~vms:40 ~numa:16 ~cores:256
  ; make ~vms:40 ~numa:32 ~cores:256
  ; make ~vms:80 ~numa:64 ~cores:256
  ; make ~vms:10 ~numa:1 ~cores:8
  ; make ~vms:10 ~numa:1 ~cores:8
  ]

let amd_specs =
  let make ~vms ~cores_per_numa ?mem () =
    let name =
      Printf.sprintf
        "Allocation of %d VM(s) on asymmetric nodes, %d cores per node" vms
        cores_per_numa
    in
    (name, vms, mem, make_numa_amd ~cores_per_numa)
  in
  [
    make ~vms:10 ~cores_per_numa:4 (); make ~vms:6 ~mem:mem3 ~cores_per_numa:4 ()
  ]

let allocate_tests =
  let test (name, vms, mem, spec) =
    (name, `Quick, test_allocate ~vms ?mem spec)
  in
  ("VM Allocation", List.map test (symmetric_specs @ amd_specs))

let distances_tests =
  let specs =
    [
      ("Last node is unreachable", Distances.unreachable_last, Some [(10., [0])])
    ; ( "Node in the middle is unreachable"
      , Distances.unreachable_middle
      , Some [(10., [0]); (10., [2]); (15., [0; 2])]
      )
    ; ( "The first two nodes are unreachable"
      , Distances.unreachable_two
      , Some [(10., [2])]
      )
    ; ("All nodes are unreachable", Distances.none_reachable, None)
    ]
  in
  let to_actual spec =
    spec
    |> Seq.map (fun (d, nodes) ->
           (d, Seq.map (function NUMA.Node n -> n) nodes |> List.of_seq)
       )
    |> List.of_seq
  in
  let test_of_spec (name, distances, expected) =
    let test () =
      let numa_t =
        make_numa_common ~logical_per_physical:1 ~cores_per_numa:1 distances
      in
      match (expected, numa_t) with
      | None, None ->
          ()
      | Some _, None ->
          Alcotest.fail "Synthetic matrix can't fail to load"
      | None, Some _ ->
          Alcotest.fail "Synthetic matrix loaded when it wasn't supposed to"
      | Some expected, Some (_, numa_t) ->
          let actual = NUMA.candidates numa_t |> to_actual in
          Alcotest.(check @@ list @@ pair (float Float.epsilon) (list int))
            "Candidates must match" expected actual
    in
    (name, `Quick, test)
  in
  ("Distance matrices", List.map test_of_spec specs)

let () =
  Debug.log_to_stdout () ;
  Alcotest.run "Topology" [allocate_tests; distances_tests]
