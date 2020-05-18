open Topology

module D = Debug.Make (struct let name = "test_topology" end)

let make_numa ~numa ~cores =
  let distances =
    Array.init numa (fun i ->
        Array.init numa (fun j -> 10 + (11 * abs (j - i))))
  in
  let cores_per_numa = cores / numa in
  let cpu_to_node = Array.init cores (fun core -> core / cores_per_numa) in
  (cores, NUMA.make ~distances ~cpu_to_node)

let make_numa_amd ~cores_per_numa =
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
  let cpu_to_node =
    Array.init (cores_per_numa * numa) (fun core -> core / cores_per_numa)
  in
  (cores_per_numa * numa, NUMA.make ~distances ~cpu_to_node)

type t = {worst: int; average: float; nodes: NUMA.node list; best: int}

let pp =
  Fmt.(
    Dump.record
      [
        Dump.field "worst" (fun t -> t.worst) int
      ; Dump.field "average" (fun t -> t.average) float
      ; Dump.field "nodes" (fun t -> t.nodes) (Dump.list NUMA.pp_dump_node)
      ; Dump.field "best" (fun t -> t.best) int
      ])

let sum_costs l =
  D.debug "====" ;
  List.fold_left
    (fun accum cost ->
      {
        worst= max accum.worst cost.worst
      ; average= accum.average +. cost.average
      ; nodes= cost.nodes @ accum.nodes
      ; best= min accum.best cost.best
      })
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
           {worst; best; nodes= []; average})
    |> sum_costs
  in
  D.debug "Costs: %s" (Fmt.to_to_string pp costs) ;
  let cpus = float @@ CPUSet.cardinal cpuset in
  let nodes =
    all_vms |> List.map (fun ((_, nodes), _) -> nodes) |> List.flatten
  in
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
      default.worst worst) ;
  Alcotest.(check int "Best case access time must not change" best c.best) ;
  Alcotest.(
    check (float 1e-3)
      "Average access times could improve, but must not be worse" average
      c.average) ;
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
    check (float 1e-3) "Balancing could improve" balancing_best balancing_next) ;
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
         let vm = NUMARequest.make ~memory:mem ~vcpus:vm_cores in
         match Softaffinity.plan h nodes ~vm with
         | None ->
             Alcotest.fail "No NUMA plan"
         | Some plan ->
             D.debug "NUMA allocation succeeded for VM %d: %s" i
               (Fmt.to_to_string CPUSet.pp_dump plan) ;
             let usednodes =
               plan
               |> CPUSet.elements
               |> List.map (NUMA.node_of_cpu h)
               |> List.sort_uniq compare
               |> List.to_seq
             in
             let costs_numa_aware =
               vm_access_costs h plans (vm_cores, usednodes, plan)
             in
             let costs_default =
               vm_access_costs h plans (vm_cores, NUMA.nodes h, NUMA.all_cpus h)
             in
             cost_not_worse ~default:costs_default costs_numa_aware ;
             ( costs_default :: costs_old
             , costs_numa_aware :: costs_new
             , ((vm_cores, List.of_seq usednodes), plan) :: plans ))
       ([], [], [])
  |> check_aggregate_costs_not_worse ~cores ~vms

let () = Printexc.record_backtrace true

let suite =
  ( "topology test"
  , [
      ( "Allocation of 1 VM on 1 node"
      , `Quick
      , test_allocate ~vms:1 @@ make_numa ~numa:1 ~cores:2 )
    ; ( "Allocation of 10 VMs on 1 node"
      , `Quick
      , test_allocate ~vms:10 @@ make_numa ~numa:1 ~cores:8 )
    ; ( "Allocation of 1 VM on 2 nodes"
      , `Quick
      , test_allocate ~vms:1 @@ make_numa ~numa:2 ~cores:4 )
    ; ( "Allocation of 10 VM on 2 nodes"
      , `Quick
      , test_allocate ~vms:10 @@ make_numa ~numa:2 ~cores:4 )
    ; ( "Allocation of 1 VM on 4 nodes"
      , `Quick
      , test_allocate ~vms:1 @@ make_numa ~numa:4 ~cores:16 )
    ; ( "Allocation of 10 VM on 4 nodes"
      , `Quick
      , test_allocate ~vms:10 @@ make_numa ~numa:4 ~cores:16 )
    ; ( "Allocation of 40 VM on 16 nodes"
      , `Quick
      , test_allocate ~vms:40 @@ make_numa ~numa:16 ~cores:256 )
    ; ( "Allocation of 40 VM on 32 nodes"
      , `Quick
      , test_allocate ~vms:40 @@ make_numa ~numa:32 ~cores:256 )
    ; ( "Allocation of 40 VM on 64 nodes"
      , `Quick
      , test_allocate ~vms:80 @@ make_numa ~numa:64 ~cores:256 )
    ; ( "Allocation of 10 VM on assymetric nodes"
      , `Quick
      , test_allocate ~vms:10 (make_numa_amd ~cores_per_numa:4) )
    ; ( "Allocation of 10 VM on assymetric nodes"
      , `Quick
      , test_allocate ~vms:6 ~mem:mem3 (make_numa_amd ~cores_per_numa:4) )
    ] )
