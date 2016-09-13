
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

open OUnit
open Test_common
open Test_vgpu_common
open Xapi_vm_helpers

(*--- Helper functions ---*)
let on_pool_of_k1s (f : Context.t -> API.ref_host -> API.ref_host -> API.ref_host -> 'a) =
  (* Note: f c h h' h'' applied to hosts with the same number of k1s as 's
     	 *           +----------+  +----------+  +----------+
     	 *           |          |  |   +--+   |  |+--+  +--+|
     	 *           |          |  |   |K1|   |  ||K1|  |K1||
     	 *           |          |  |   +--+   |  |+--+  +--+|
     	 *           +----------+  +----------+  +----------+
     	 *                h             h'            h''
     	 *)
  let __context = make_test_database () in
  let h = List.hd (Db.Host.get_all ~__context) in
  (* Make two more hosts *)
  match (make_host ~__context (), make_host ~__context ()) with
  | (h', h'') ->
    let gPU_group = make_gpu_group ~__context () in
    let rec make_k1s_on (host, num) =
      if num > 0 then
        let _ = make_pgpu ~__context ~host ~gPU_group default_k1 in
        make_k1s_on (host, (num - 1))
    in
    List.iter make_k1s_on [(h, 0); (h', 1); (h'', 2)];
    f __context h h' h''

let make_vm_with_vgpu_in_group ~__context vgpu_type gpu_group_ref =
  let vgpu_ref = make_vgpu ~__context ~resident_on:Ref.null vgpu_type
  and vm_ref = make_vm ~__context () in
  Db.VGPU.set_GPU_group ~__context ~self:vgpu_ref ~value:gpu_group_ref;
  Db.VGPU.set_VM ~__context ~self:vgpu_ref ~value:vm_ref;
  vm_ref

(*--- Xapi_vm_helpers.assert_gpus_available ---*)
let test_gpus_available_succeeds () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context k100 group in
      assert_gpus_available ~__context ~self:vm ~host:h')

let test_gpus_available_fails_no_pgpu () =
  on_pool_of_k1s (fun __context h _ _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let vm = make_vm_with_vgpu_in_group ~__context k100 group in
      assert_raises_api_error Api_errors.vm_requires_gpu (fun () ->
          assert_gpus_available ~__context ~self:vm ~host:h))

let test_gpus_available_fails_disabled_type () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
      List.iter (fun p ->
          Db.PGPU.set_enabled_VGPU_types ~__context ~self:p ~value:[])
        pgpus;
      let vm = make_vm_with_vgpu_in_group ~__context k100 group in
      assert_raises_api_error Api_errors.vm_requires_gpu (fun () ->
          assert_gpus_available ~__context ~self:vm ~host:h'))

let test_gpus_available_fails_no_capacity () =
  on_pool_of_k1s (fun __context _ h' _ ->
      let group = List.hd (Db.GPU_group.get_all ~__context) in
      let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
      (* Fill up all the PGPUs *)
      List.iter (fun p ->
          ignore (make_vgpu ~__context ~resident_on:p Xapi_vgpu_type.passthrough_gpu))
        pgpus;
      let vm = make_vm_with_vgpu_in_group ~__context k100 group in
      assert_raises_api_error Api_errors.vm_requires_gpu
        (fun () -> assert_gpus_available ~__context ~self:vm ~host:h'))

(*--- Xapi_vm_helpers.group_hosts_by_best_pgpu ---*)
let assert_list_is_set l =
  let rec inner ac = function
    | [] -> ()
    | x :: xs ->
      if (List.mem x ac) then assert_failure "List is not set"
      else inner (x :: ac) xs
  in
  inner [] l

let assert_host_group_coherent g =
  match g with
  | [] -> assert_failure "Empty host group"
  | (h, c) :: _ ->
    assert_list_is_set (List.map fst g);
    assert_bool "Score not same for all hosts in group"
      (List.for_all (fun x -> snd x = c) g)

let assert_host_groups_equal g g' =
  let extract_host_strings g =
    let hosts = List.map fst g in
    List.sort String.compare (List.map Ref.string_of hosts)
  in
  assert_equal (extract_host_strings g) (extract_host_strings g');
  let score_of g = snd (List.hd g) in
  assert_equal (score_of g) (score_of g')

let rec assert_equivalent expected_grouping actual_grouping =
  match (expected_grouping, actual_grouping) with
  | [], [] -> ()
  | [], xx -> assert_failure (Printf.sprintf "%d more groups than expected." (List.length xx))
  | xx, [] -> assert_failure (Printf.sprintf "%d less groups than expected." (List.length xx))
  | e :: es, g :: gs ->
    assert_host_group_coherent g;
    assert_host_groups_equal e g;
    assert_equivalent es gs

let assert_grouping ~__context gpu_group vgpu_type g =
  let vgpu_type_ref = Xapi_vgpu_type.find_or_create ~__context vgpu_type in
  let host_lists = group_hosts_by_best_pgpu_in_group ~__context gpu_group vgpu_type_ref in
  try assert_equivalent g host_lists
  with e ->
    let item_to_string (h, c) = Printf.sprintf "(%s, %Ld)" (Ref.string_of h) c in
    let group_to_string g = Printf.sprintf "[ %s ]"
        (String.concat "; " (List.map item_to_string g)) in
    let groups_to_string gs = Printf.sprintf "[ %s ]"
        (String.concat "; " (List.map group_to_string gs)) in
    let diff_string = Printf.sprintf "Expected: %s\nActual: %s\n"
        (groups_to_string g) (groups_to_string host_lists) in
    assert_failure (diff_string ^ Printexc.to_string e)

let rec assert_expectations ~__context gpu_group = function
  | [] -> ()
  | (vgpu_type, expected_grouping) :: remaining ->
    assert_grouping ~__context gpu_group vgpu_type expected_grouping;
    assert_expectations ~__context gpu_group remaining

let test_group_hosts_bf () =
  on_pool_of_k1s (fun __context h h' h'' ->
      let gpu_group = List.hd (Db.GPU_group.get_all ~__context) in
      Db.GPU_group.set_allocation_algorithm ~__context ~self:gpu_group ~value:`breadth_first;
      match Db.Host.get_PGPUs ~__context ~self:h' @ Db.Host.get_PGPUs ~__context ~self:h'' with
      | [h'_p; h''_p; h''_p'] ->
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',8L);(h'',8L)] ];
          k140q, [ [(h',4L);(h'',4L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h''_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',8L);(h'',8L)] ];
          k140q, [ [(h',4L);(h'',4L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h''_p' k140q);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',8L)]; [(h'',7L)] ];
          k140q, [ [(h',4L)]; [(h'',3L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h'_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',7L);(h'',7L)] ];
          k140q, [ [(h'',3L)]; ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h'_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h'',7L)]; [(h',6L)] ];
          k140q, [ [(h'',3L)]; ];
        ];
      | _ -> failwith "Test-failure: Unexpected number of pgpus in test setup"
    )

let test_group_hosts_df () =
  on_pool_of_k1s (fun __context h h' h'' ->
      let gpu_group = List.hd (Db.GPU_group.get_all ~__context) in
      Db.GPU_group.set_allocation_algorithm ~__context ~self:gpu_group ~value:`depth_first;
      match Db.Host.get_PGPUs ~__context ~self:h' @ Db.Host.get_PGPUs ~__context ~self:h'' with
      | [h'_p; h''_p; h''_p'] ->
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',8L);(h'',8L)] ];
          k140q, [ [(h',4L);(h'',4L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h''_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h'',7L)]; [(h',8L)] ];
          k140q, [ [(h',4L);(h'',4L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h''_p' k140q);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h'',7L)]; [(h',8L)] ];
          k140q, [ [(h'',3L)]; [(h',4L)] ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h'_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',7L);(h'',7L)] ];
          k140q, [ [(h'',3L)]; ];
        ];
        ignore (make_vgpu ~__context ~resident_on:h'_p k100);
        assert_expectations ~__context gpu_group [
          k100,  [ [(h',6L)]; [(h'',7L)] ];
          k140q, [ [(h'',3L)]; ];
        ];
      | _ -> failwith "Test-failure: Unexpected number of pgpus in test setup"
    )

let test =
  "test_vm_helpers" >:::
  [
    "test_gpus_available_succeeds" >:: test_gpus_available_succeeds;
    "test_gpus_available_fails_no_pgpu" >:: test_gpus_available_fails_no_pgpu;
    "test_gpus_available_fails_disabled_type" >:: test_gpus_available_fails_disabled_type;
    "test_gpus_available_fails_no_capacity" >:: test_gpus_available_fails_no_capacity;

    "test_group_hosts_bf" >:: test_group_hosts_bf;
    "test_group_hosts_df" >:: test_group_hosts_df;
  ]
