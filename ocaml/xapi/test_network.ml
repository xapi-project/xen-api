
let test_purpose_setters =
  let assert_equal ~msg p1 p2 = Ounit_comparators.NetworkPurposeSet.(assert_equal ~msg (of_list p1) (of_list p2)) in

  let with_test f =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let network ~purpose = Test_common.make_network ~__context ~purpose () in
    let add_purpose = Xapi_network.add_purpose ~__context in
    let remove_purpose = Xapi_network.remove_purpose ~__context in
    let get_purpose = Db.Network.get_purpose ~__context in
    f network add_purpose remove_purpose get_purpose
  in

  let test_add_purpose () =
    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[] in
        add_purpose ~self:network1 ~value:`nbd;
        assert_equal
          ~msg:"Should be able to add 'nbd' purpose when none of the networks have a purpose"
          [`nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[`nbd] in
        add_purpose ~self:network1 ~value:`nbd;
        assert_equal
          ~msg:"Should be able to add 'nbd' purpose when the other network has the same purpose"
          [`nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[] in
        add_purpose ~self:network1 ~value:`insecure_nbd;
        assert_equal
          ~msg:"Should be able to add 'insecure_nbd' purpose when none of the networks have a purpose"
          [`insecure_nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[`insecure_nbd] in
        add_purpose ~self:network1 ~value:`insecure_nbd;
        assert_equal
          ~msg:"Should be able to add 'insecure_nbd' purpose when the other network has the same purpose"
          [`insecure_nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[`nbd] in
        add_purpose ~self:network1 ~value:`nbd;
        assert_equal
          ~msg:"add_purpose should be idempotent and should do nothing when adding an existing purpose"
          [`nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[`nbd] in
        OUnit.assert_raises
          ~msg:"Should not be allowed to add 'insecure_nbd' to a network that already has the 'nbd' purpose"
          (* The first parameter of this error is the new purpose we're trying to add, the second is the conflicting one. *)
          Api_errors.(Server_error (network_incompatible_purposes, ["insecure_nbd"; "nbd"]))
          (fun () -> add_purpose ~self:network1 ~value:`insecure_nbd);
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[`nbd] in
        OUnit.assert_raises
          ~msg:"Should not be allowed to add 'insecure_nbd' when another network already has the 'nbd' purpose"
          Api_errors.(Server_error (network_incompatible_purposes, ["insecure_nbd"; "nbd"]))
          (fun () -> add_purpose ~self:network1 ~value:`insecure_nbd);
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[`insecure_nbd] in
        OUnit.assert_raises
          ~msg:"Should not be allowed to add 'nbd' to a network that already has the 'insecure_nbd' purpose"
          Api_errors.(Server_error (network_incompatible_purposes, ["nbd"; "insecure_nbd"]))
          (fun () -> add_purpose ~self:network1 ~value:`nbd);
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        let _network2 : _ API.Ref.t = network ~purpose:[`insecure_nbd] in
        OUnit.assert_raises
          ~msg:"Should not be allowed to add 'nbd' when another network already has the 'insecure_nbd' purpose"
          Api_errors.(Server_error (network_incompatible_purposes, ["nbd"; "insecure_nbd"]))
          (fun () -> add_purpose ~self:network1 ~value:`nbd);
      )
  in

  let test_remove_purpose () =
    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[`nbd] in
        remove_purpose ~self:network1 ~value:`insecure_nbd;
        assert_equal
          ~msg:"remove_purpose should be idempotent and should do nothing when removing an invalid purpose that is not present"
          [`nbd]
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[] in
        remove_purpose ~self:network1 ~value:`nbd;
        assert_equal
          ~msg:"remove_purpose should be idempotent and should do nothing when removing a valid purpose that isn't present"
          []
          (get_purpose ~self:network1)
      );

    with_test (fun network add_purpose remove_purpose get_purpose ->
        let network1 = network ~purpose:[`nbd] in
        remove_purpose ~self:network1 ~value:`nbd;
        assert_equal
          ~msg:"remove_purpose should successfully remove a purpose that is present"
          []
          (get_purpose ~self:network1)
      )
  in

  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_purpose_setters" >:::
  [ "test_add_purpose" >:: test_add_purpose
  ; "test_remove_purpose" >:: test_remove_purpose
  ]

let test =
  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_network" >:::
  [ test_purpose_setters ]
