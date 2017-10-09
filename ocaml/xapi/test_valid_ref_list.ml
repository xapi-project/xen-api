
let assert_equal l1 l2 = Ounit_comparators.StringList.(assert_equal l1 l2)

let with_vm_list f () =
  let __context = Mock.make_context_with_new_db "Mock context" in

  let vm1 = Test_common.make_vm ~__context ~name_label:"a" ~name_description:"d_a" () in

  let vm2 = Ref.null in

  let vm3 = Test_common.make_vm ~__context ~name_label:"c" ~name_description:"d_c" () in
  Db.VM.destroy ~__context ~self:vm3;

  let vm4 = Test_common.make_vm ~__context ~name_label:"d" ~name_description:"d_d" () in

  f __context [vm1; vm2; vm3; vm4]

let test_exists =
  with_vm_list (fun __context l ->
      let f vm = Db.VM.get_name_label ~__context ~self:vm = "a" in
      OUnit.assert_equal true (Valid_ref_list.exists f l);
      let f vm = Db.VM.get_name_label ~__context ~self:vm = "c" in
      OUnit.assert_equal false (Valid_ref_list.exists f l)
    )

let test_valid_ref_filter =
  with_vm_list (fun __context ->
      function
      | [vm1; vm2; vm3; vm4] as l ->
        let assert_equal l1 l2 =
          let as_strings = List.map Ref.string_of in
          assert_equal (l1 |> as_strings) (l2 |> as_strings)
        in
        let f vm = Db.VM.get_name_label ~__context ~self:vm = "c" in
        assert_equal [] (Valid_ref_list.filter f l);
        let f vm = Db.VM.get_name_label ~__context ~self:vm = "d" in
        assert_equal [vm4] (Valid_ref_list.filter f l);
        let f vm =
          let name = Db.VM.get_name_label ~__context ~self:vm in
          name = "a" || name = "d"
        in
        assert_equal [vm1; vm4] (Valid_ref_list.filter f l)
      | _ -> failwith "The test list should contain 4 VMs"
    )

let test_for_all =
  with_vm_list (fun __context l ->
      let f vm =
        let name = Db.VM.get_name_label ~__context ~self:vm in
        name = "a" || name = "c"
      in
      OUnit.assert_equal false (Valid_ref_list.for_all f l);
      let f vm =
        let name = Db.VM.get_name_label ~__context ~self:vm in
        name = "a" || name = "d"
      in
      OUnit.assert_equal true (Valid_ref_list.for_all f l)
    )

let test_map =
  with_vm_list (fun __context l ->
      let f vm = Db.VM.get_name_label ~__context ~self:vm in
      assert_equal ["a"; "d"] (Valid_ref_list.map f l)
    )

let test_flat_map =
  with_vm_list (fun __context l ->
      let f vm = [(Db.VM.get_name_label ~__context ~self:vm); (Db.VM.get_name_description ~__context ~self:vm)] in
      assert_equal ["a"; "d_a"; "d"; "d_d"] (Valid_ref_list.flat_map f l)
    )

let test =
  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_valid_ref_list" >:::
  [ "test_map" >:: test_map
  ; "test_exists" >:: test_exists
  ; "test_filter" >:: test_valid_ref_filter
  ; "test_for_all" >:: test_for_all
  ; "test_flat_map" >:: test_flat_map
  ]
