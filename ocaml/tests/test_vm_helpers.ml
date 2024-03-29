(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

module T = Test_common
module VGPU_T = Test_vgpu_common
open Xapi_vm_helpers

(* Normally we provide the actual exn thrown (ie error code AND params) and test with Alcotest.check_raises
 * but these errors are a bit involved and basically require duplicating the logic for returning the params *)
let assert_raises_api_error expected_error_string f =
  match f () with
  | exception Api_errors.(Server_error (actual_error, _)) ->
      Alcotest.(check string)
        "check for API error" expected_error_string actual_error
  | _ ->
      Alcotest.fail ("check for API error, expecting " ^ expected_error_string)

(*--- Helper functions ---*)
let on_pool_of_k1s
    (f : Context.t -> API.ref_host -> API.ref_host -> API.ref_host -> 'a) =
  (* Note: f c h h' h'' applied to hosts with the same number of k1s as 's
     	 *           +----------+  +----------+  +----------+
     	 *           |          |  |   +--+   |  |+--+  +--+|
     	 *           |          |  |   |K1|   |  ||K1|  |K1||
     	 *           |          |  |   +--+   |  |+--+  +--+|
     	 *           +----------+  +----------+  +----------+
     	 *                h             h'            h''
  *)
  let __context = T.make_test_database () in
  let h = List.hd (Db.Host.get_all ~__context) in
  (* Make two more hosts *)
  match (T.make_host ~__context (), T.make_host ~__context ()) with
  | h', h'' ->
      let gPU_group = T.make_gpu_group ~__context () in
      let rec make_k1s_on (host, num) =
        if num > 0 then
          let (_ : API.ref_PGPU) =
            VGPU_T.make_pgpu ~__context ~host ~gPU_group VGPU_T.default_k1
          in
          make_k1s_on (host, num - 1)
      in
      List.iter make_k1s_on [(h, 0); (h', 1); (h'', 2)] ;
      f __context h h' h''

let make_vm_with_vgpu_in_group ~__context vgpu_type gpu_group_ref =
  let vgpu_ref = VGPU_T.make_vgpu ~__context ~resident_on:Ref.null vgpu_type
  and vm_ref = T.make_vm ~__context () in
  Db.VGPU.set_GPU_group ~__context ~self:vgpu_ref ~value:gpu_group_ref ;
  Db.VGPU.set_VM ~__context ~self:vgpu_ref ~value:vm_ref ;
  vm_ref

(*--- Xapi_vm_helpers.assert_gpus_available ---*)
let test_gpus_available_succeeds () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      assert_gpus_available ~__context ~self:vm ~host:h'
  )

let test_gpus_available_fails_no_pgpu () =
  on_pool_of_k1s (fun __context h _ _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      assert_raises_api_error Api_errors.vm_requires_vgpu (fun () ->
          assert_gpus_available ~__context ~self:vm ~host:h
      )
  )

let test_gpus_available_fails_disabled_type () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
      List.iter
        (fun p -> Db.PGPU.set_enabled_VGPU_types ~__context ~self:p ~value:[])
        pgpus ;
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      assert_raises_api_error Api_errors.vm_requires_vgpu (fun () ->
          assert_gpus_available ~__context ~self:vm ~host:h'
      )
  )

let test_gpus_available_fails_no_capacity () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
      (* Fill up all the PGPUs *)
      List.iter
        (fun p ->
          ignore
            VGPU_T.(
              make_vgpu ~__context ~resident_on:p Xapi_vgpu_type.passthrough_gpu
            )
        )
        pgpus ;
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      assert_raises_api_error Api_errors.vm_requires_vgpu (fun () ->
          assert_gpus_available ~__context ~self:vm ~host:h'
      )
  )

(*--- Xapi_vm_helpers.group_hosts_by_best_pgpu ---*)
let assert_list_is_set l =
  let rec inner ac = function
    | x :: xs ->
        Alcotest.(check bool) "List is not set" false (List.mem x ac) ;
        inner (x :: ac) xs
    | [] ->
        ()
  in
  inner [] l

let assert_host_group_coherent g =
  match g with
  | [] ->
      ()
  | (_, c) :: _ ->
      assert_list_is_set (List.map fst g) ;
      Alcotest.(check bool)
        "Score not same for all hosts in group" true
        (List.for_all (fun x -> snd x = c) g)

let assert_host_groups_equal g g' =
  match g' with
  | [] ->
      ()
  | _ ->
      let extract_host_strings g =
        let hosts = List.map fst g in
        List.sort String.compare (List.map Ref.string_of hosts)
      in
      Alcotest.(check (slist string String.compare))
        "check host strings" (extract_host_strings g) (extract_host_strings g') ;
      let score_of g = snd (List.hd g) in
      Alcotest.(check int64) "check host score" (score_of g) (score_of g')

let rec assert_equivalent expected_grouping actual_grouping =
  match (expected_grouping, actual_grouping) with
  | [], [] ->
      ()
  | [], xx ->
      Alcotest.fail
        (Printf.sprintf "%d more groups than expected." (List.length xx))
  | xx, [] ->
      Alcotest.fail
        (Printf.sprintf "%d fewer groups than expected." (List.length xx))
  | e :: es, g :: gs ->
      assert_host_group_coherent g ;
      assert_host_groups_equal e g ;
      assert_equivalent es gs

let assert_host_groups_equal_for_vgpu g g' =
  match g' with
  | [] ->
      ()
  | _ ->
      let extract_host_strings hosts =
        List.sort String.compare (List.map Ref.string_of hosts)
      in
      Alcotest.(check (slist string String.compare))
        "check host strings" (extract_host_strings g) (extract_host_strings g')

let rec assert_equivalent_for_vgpu expected_grouping actual_grouping =
  match (expected_grouping, actual_grouping) with
  | [], [] ->
      ()
  | [], xx ->
      Alcotest.fail
        (Printf.sprintf "%d more groups than expected." (List.length xx))
  | xx, [] ->
      Alcotest.fail
        (Printf.sprintf "%d fewer groups than expected." (List.length xx))
  | e :: es, g :: gs ->
      assert_host_groups_equal_for_vgpu e g ;
      assert_equivalent_for_vgpu es gs

let assert_grouping ~__context gpu_group ~visible_hosts vgpu_type g =
  let vgpu = VGPU_T.make_vgpu ~__context ~gPU_group:gpu_group vgpu_type in
  let host_lists = rank_hosts_by_best_vgpu ~__context vgpu visible_hosts in
  assert_equivalent_for_vgpu g host_lists

let check_expectations ~__context gpu_group visible_hosts vgpu_type
    expected_grouping =
  assert_grouping ~__context gpu_group ~visible_hosts vgpu_type
    expected_grouping

let test_group_hosts_bf () =
  on_pool_of_k1s
    VGPU_T.(
      fun __context _ h' h'' ->
        let gpu_group = List.hd (Db.GPU_group.get_all ~__context) in
        Db.GPU_group.set_allocation_algorithm ~__context ~self:gpu_group
          ~value:`breadth_first ;
        match
          Db.Host.get_PGPUs ~__context ~self:h'
          @ Db.Host.get_PGPUs ~__context ~self:h''
        with
        | [h'_p; h''_p; h''_p'] ->
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'']; [h']] ;
            check_expectations ~__context gpu_group [h'; h''] k140q
              [[h'']; [h']] ;
            ignore (make_vgpu ~__context ~resident_on:h''_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'']; [h']] ;
            check_expectations ~__context gpu_group [h'; h''] k140q [[h'; h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h''_p' k140q) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h']; [h'']] ;
            check_expectations ~__context gpu_group [h''] k140q [[h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h'_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'; h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h'_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'']; [h']]
        | _ ->
            Alcotest.fail
              "Test-failure: Unexpected number of pgpus in test setup"
    )

let test_group_hosts_df () =
  on_pool_of_k1s
    VGPU_T.(
      fun __context _ h' h'' ->
        let gpu_group = List.hd (Db.GPU_group.get_all ~__context) in
        Db.GPU_group.set_allocation_algorithm ~__context ~self:gpu_group
          ~value:`depth_first ;
        match
          Db.Host.get_PGPUs ~__context ~self:h'
          @ Db.Host.get_PGPUs ~__context ~self:h''
        with
        | [h'_p; h''_p; h''_p'] ->
            check_expectations ~__context gpu_group [h'; h''] k100 [[h']; [h'']] ;
            check_expectations ~__context gpu_group [h'; h''] k140q
              [[h']; [h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h''_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h']; [h'']] ;
            check_expectations ~__context gpu_group [h'; h''] k140q [[h'; h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h''_p' k140q) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'']; [h']] ;
            check_expectations ~__context gpu_group [h''] k140q [[h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h'_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h'; h'']] ;
            ignore (make_vgpu ~__context ~resident_on:h'_p k100) ;
            check_expectations ~__context gpu_group [h'; h''] k100 [[h']; [h'']]
        | _ ->
            Alcotest.fail
              "Test-failure: Unexpected number of pgpus in test setup"
    )

let on_pool_of_intel_i350
    (f : Context.t -> API.ref_host -> API.ref_host -> API.ref_host -> 'a) =
  (* Note: f c h h' h'' applied to hosts with the same number of Intel I350 as 's
      Due to one host at most have one pif in a network, for h'', the remaining vfs for each pif doesn't matter.
      What really matters is on same sriov network,on different host h' and h'' pif's remaining vfs.
   *           +------------+  +----------------+ +--------------+
   *           |            |  | +----+ +----+  | |+----+  +----+|
   *           |            |  | |I350| | K1 |  | ||I350|  |I350||
   *           |            |  | +----+ +----+  | |+----+  +----+|
   *           +------------+  +----------------+ +--------------+
   *                 h                h'              h''
   *)
  let __context = T.make_test_database () in
  let h = List.hd (Db.Host.get_all ~__context) in
  (* Make two more hosts *)
  let h' = T.make_host ~__context () in
  let h'' = T.make_host ~__context () in
  let sriov_network1 =
    T.make_network ~__context ~name_description:"sriov1" ~bridge:"" ()
  in
  let sriov_network2 =
    T.make_network ~__context ~name_description:"sriov2" ~bridge:"" ()
  in
  let make_sriov_on (host, network) =
    let local_network =
      T.make_network ~__context ~name_description:"local_network" ()
    in
    let pf = T.make_pci ~__context ~host ~functions:1L ~driver_name:"igb" () in
    let physical_PIF = T.make_pif ~__context ~host ~network:local_network () in
    Db.PIF.set_PCI ~__context ~self:physical_PIF ~value:pf ;
    let logical_pif =
      T.create_sriov_pif ~__context ~pif:physical_PIF ~network ()
    in
    Db.PIF.set_currently_attached ~__context ~self:logical_pif ~value:true ;
    T.make_vfs_on_pf ~__context ~pf ~num:8L
  in
  List.iter make_sriov_on
    [(h', sriov_network1); (h'', sriov_network1); (h'', sriov_network2)] ;
  (* make one k1 on h' *)
  let gPU_group = T.make_gpu_group ~__context () in
  let (_ : API.ref_PGPU) =
    VGPU_T.(make_pgpu ~__context ~host:h' ~gPU_group default_k1)
  in
  f __context h h' h''

let make_vm_with_vif ~__context ~network =
  let vm = T.make_vm ~__context () in
  let (_ : API.ref_VIF) = T.make_vif ~__context ~vM:vm ~network () in
  vm

let make_allocated_vfs ~__context ~vm ~pci ~num =
  let rec allocate_vf num =
    if num > 0L then
      let _ = Pciops.reserve_free_virtual_function ~__context vm pci in
      allocate_vf (Int64.sub num 1L)
  in
  allocate_vf num

(* --- Xapi_network_sriov_helpers.is_sriov_network --- *)
let test_is_sriov_network_succeeds () =
  on_pool_of_intel_i350 (fun __context _ h' _ ->
      let local_logical_PIFs =
        Db.PIF.get_records_where ~__context
          ~expr:
            (And
               ( Eq (Field "host", Literal (Ref.string_of h'))
               , Eq (Field "physical", Literal "false")
               )
            )
      in
      let network =
        List.map
          (fun (rf, _) -> Db.PIF.get_network ~__context ~self:rf)
          local_logical_PIFs
        |> List.hd
      in
      Alcotest.(check bool)
        "check is sriov_network" true
        (Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network)
  )

(*--- Xapi_vm_helpers.assert_netsriov_available ---*)
let test_netsriov_available_succeeds () =
  on_pool_of_intel_i350 (fun __context _ h' _ ->
      let sriov_networks =
        List.find_all
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
      in
      let network_on_h =
        List.find
          (fun network ->
            let pifs = Db.Network.get_PIFs ~__context ~self:network in
            List.exists
              (fun pif -> h' = Db.PIF.get_host ~__context ~self:pif)
              pifs
          )
          sriov_networks
      in
      let vm = make_vm_with_vif ~__context ~network:network_on_h in
      assert_netsriov_available ~__context ~self:vm ~host:h'
  )

(* If a host without a SR-IOV network was chosen,then the assert_can_see_networks will raise `vm_requires_net` before `assert_netsriov_available` *)
let test_netsriov_available_fails_no_netsriov () =
  on_pool_of_intel_i350 (fun __context h _ _ ->
      let sriov_network =
        List.find
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
      in
      let vm = make_vm_with_vif ~__context ~network:sriov_network in
      assert_raises_api_error Api_errors.vm_requires_net (fun () ->
          assert_can_see_networks ~__context ~self:vm ~host:h
      )
  )

let test_netsriov_available_fails_no_capacity () =
  on_pool_of_intel_i350 (fun __context _ h' _ ->
      let sriov_networks =
        List.find_all
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
      in
      let network_on_h =
        List.find
          (fun network ->
            let pifs = Db.Network.get_PIFs ~__context ~self:network in
            List.exists
              (fun pif -> h' = Db.PIF.get_host ~__context ~self:pif)
              pifs
          )
          sriov_networks
      in
      let vm = make_vm_with_vif ~__context ~network:network_on_h in
      match
        Xapi_network_sriov_helpers.get_local_underlying_pif ~__context
          ~network:network_on_h ~host:h'
      with
      | None ->
          Alcotest.fail "Cannot get underlying pif from sr-iov network"
      | Some pif ->
          let pci = Db.PIF.get_PCI ~__context ~self:pif in
          make_allocated_vfs ~__context ~vm ~pci ~num:8L ;
          assert_raises_api_error Api_errors.network_sriov_insufficient_capacity
            (fun () -> assert_netsriov_available ~__context ~self:vm ~host:h'
          )
  )

let assert_grouping_of_sriov ~__context ~network ~expection_groups =
  let host_lists =
    Xapi_network_sriov_helpers.group_hosts_by_best_sriov ~__context ~network
  in
  assert_equivalent expection_groups host_lists

let test_group_hosts_netsriov () =
  on_pool_of_intel_i350 (fun __context _ h' h'' ->
      let sriov_networks =
        List.find_all
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
        |> List.sort (fun a b ->
               compare
                 (List.length (Db.Network.get_PIFs ~__context ~self:a))
                 (List.length (Db.Network.get_PIFs ~__context ~self:b))
           )
      in
      match sriov_networks with
      (* we create 2 sriov networks,one include h and h'' and the other only has h'' *)
      | n1 :: n2 :: _ ->
          (* n1 only have sriov on h'' *)
          assert_grouping_of_sriov ~__context ~network:n1
            ~expection_groups:[[(h'', 8L)]] ;
          (* n2 has sriovs on h' and h'' *)
          assert_grouping_of_sriov ~__context ~network:n2
            ~expection_groups:[[(h', 8L); (h'', 8L)]]
      | _ ->
          Alcotest.fail
            "Test-failure: Unexpected number of sriov network in test"
  )

let test_group_hosts_netsriov_unattached () =
  on_pool_of_intel_i350 (fun __context _ h' h'' ->
      let sriov_networks =
        List.find_all
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
        |> List.sort (fun a b ->
               compare
                 (List.length (Db.Network.get_PIFs ~__context ~self:a))
                 (List.length (Db.Network.get_PIFs ~__context ~self:b))
           )
      in
      match sriov_networks with
      | n1 :: n2 :: _ ->
          (* n1 only have sriov on h'' *)
          let pif = List.hd (Db.Network.get_PIFs ~__context ~self:n1) in
          Db.PIF.set_currently_attached ~__context ~self:pif ~value:false ;
          assert_grouping_of_sriov ~__context ~network:n1
            ~expection_groups:[[(h'', 0L)]] ;
          let pif =
            List.find
              (fun self -> h'' = Db.PIF.get_host ~__context ~self)
              (Db.Network.get_PIFs ~__context ~self:n2)
          in
          Db.PIF.set_currently_attached ~__context ~self:pif ~value:false ;
          assert_grouping_of_sriov ~__context ~network:n2
            ~expection_groups:[[(h', 8L)]; [(h'', 0L)]]
      | _ ->
          Alcotest.fail
            "Test-failure: Unexpected number of sriov network in test"
  )

let test_group_hosts_netsriov_with_allocated () =
  on_pool_of_intel_i350 (fun __context _ h' h'' ->
      let sriov_networks =
        List.find_all
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
        |> List.sort (fun a b ->
               compare
                 (List.length (Db.Network.get_PIFs ~__context ~self:a))
                 (List.length (Db.Network.get_PIFs ~__context ~self:b))
           )
      in
      match sriov_networks with
      (* we create 2 sriov networks,one include h and h'' and the other only has h'' *)
      | _ :: n2 :: _ -> (
          (* n2 has sriovs on h' and h'' *)
          let network_on_h =
            List.find
              (fun network ->
                let pifs = Db.Network.get_PIFs ~__context ~self:network in
                List.exists
                  (fun pif -> h' = Db.PIF.get_host ~__context ~self:pif)
                  pifs
              )
              sriov_networks
          in
          let vm = make_vm_with_vif ~__context ~network:network_on_h in
          match
            Xapi_network_sriov_helpers.get_local_underlying_pif ~__context
              ~network:network_on_h ~host:h'
          with
          | None ->
              Alcotest.fail
                "Test-failure: Cannot get underlying pif from sr-iov network"
          | Some pif ->
              let pci = Db.PIF.get_PCI ~__context ~self:pif in
              make_allocated_vfs ~__context ~vm ~pci ~num:2L ;
              assert_grouping_of_sriov ~__context ~network:n2
                ~expection_groups:[[(h'', 8L)]; [(h', 6L)]]
        )
      | _ ->
          Alcotest.fail
            "Test-failure: Unexpected number of sriov network in test"
  )

let test_get_group_key_vgpu () =
  on_pool_of_intel_i350 (fun __context _ _ _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      match Xapi_vm_helpers.get_group_key ~__context ~vm with
      | `VGPU _ ->
          ()
      | _ ->
          Alcotest.fail "Test-failure: Unexpected Group Key in test"
  )

let test_get_group_key_netsriov () =
  on_pool_of_intel_i350 (fun __context _ _ _ ->
      let sriov_network =
        List.find
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
      in
      let vm = make_vm_with_vif ~__context ~network:sriov_network in
      match Xapi_vm_helpers.get_group_key ~__context ~vm with
      | `Netsriov _ ->
          ()
      | _ ->
          Alcotest.fail "Test-failure: Unexpected Group Key in test"
  )

let test_get_group_key_vgpu_and_netsriov () =
  on_pool_of_intel_i350 (fun __context _ _ _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context VGPU_T.k100 group in
      let sriov_network =
        List.find
          (fun network ->
            Xapi_network_sriov_helpers.is_sriov_network ~__context ~self:network
          )
          (Db.Network.get_all ~__context)
      in
      let (_ : API.ref_VIF) =
        T.make_vif ~__context ~vM:vm ~network:sriov_network ()
      in
      match Xapi_vm_helpers.get_group_key ~__context ~vm with
      | `VGPU _ ->
          ()
      | _ ->
          Alcotest.fail "Test-failure: Unexpected Group Key in test"
  )

let test =
  [
    ("test_gpus_available_succeeds", `Quick, test_gpus_available_succeeds)
  ; ( "test_gpus_available_fails_no_pgpu"
    , `Quick
    , test_gpus_available_fails_no_pgpu
    )
  ; ( "test_gpus_available_fails_disabled_type"
    , `Quick
    , test_gpus_available_fails_disabled_type
    )
  ; ( "test_gpus_available_fails_no_capacity"
    , `Quick
    , test_gpus_available_fails_no_capacity
    )
  ; ("test_group_hosts_bf", `Quick, test_group_hosts_bf)
  ; ("test_group_hosts_df", `Quick, test_group_hosts_df)
  ; ("test_is_sriov_network_succeeds", `Quick, test_is_sriov_network_succeeds)
  ; ( "test_netsriov_available_succeeds"
    , `Quick
    , test_netsriov_available_succeeds
    )
  ; ( "test_netsriov_available_fails_no_netsriov"
    , `Quick
    , test_netsriov_available_fails_no_netsriov
    )
  ; ( "test_netsriov_available_fails_no_capacity"
    , `Quick
    , test_netsriov_available_fails_no_capacity
    )
  ; ("test_group_hosts_netsriov", `Quick, test_group_hosts_netsriov)
  ; ( "test_group_hosts_netsriov_unattached"
    , `Quick
    , test_group_hosts_netsriov_unattached
    )
  ; ( "test_group_hosts_netsriov_with_allocated"
    , `Quick
    , test_group_hosts_netsriov_with_allocated
    )
  ; ("test_get_group_key_vgpu", `Quick, test_get_group_key_vgpu)
  ; ("test_get_group_key_netsriov", `Quick, test_get_group_key_netsriov)
  ; ( "test_get_group_key_vgpu_and_netsriov"
    , `Quick
    , test_get_group_key_vgpu_and_netsriov
    )
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test VM Helpers suite" [("Test_vm_helpers", test)]
