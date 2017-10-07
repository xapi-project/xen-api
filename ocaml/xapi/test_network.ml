
let test_purpose_setters () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  let network1 = Test_common.make_network ~__context ~purpose:[] () in
  let network2 = Test_common.make_network ~__context ~purpose:[] () in
  let _network3 : _ API.Ref.t = Test_common.make_network ~__context ~purpose:[] () in

  let assert_equal ~msg p1 p2 = Ounit_comparators.NetworkPurposeSet.(assert_equal ~msg (of_list p1) (of_list p2)) in

  (* Purposes: network1: [], network2: [], (_network3: []) *)

  Xapi_network.add_purpose ~__context ~self:network1 ~value:`nbd;
  assert_equal
    ~msg:"Should be able to add 'nbd' purpose when none of the networks have a purpose"
    [`nbd]
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [nbd], network2: [] *)

  Xapi_network.add_purpose ~__context ~self:network2 ~value:`nbd;
  assert_equal
    ~msg:"Should be able to add 'nbd' purpose when the other network has the same purpose"
    [`nbd]
    (Db.Network.get_purpose ~__context ~self:network2);

  (* network1: [nbd], network2: [nbd] *)

  Xapi_network.add_purpose ~__context ~self:network2 ~value:`nbd;
  assert_equal
    ~msg:"add_purpose should be idempotent and should do nothing when adding an existing purpose"
    [`nbd]
    (Db.Network.get_purpose ~__context ~self:network2);

  (* network1: [nbd], network2: [nbd] *)

  Xapi_network.remove_purpose ~__context ~self:network1 ~value:`insecure_nbd;
  assert_equal
    ~msg:"remove_purpose should be idempotent and should do nothing when removing an invalid purpose that is not present"
    [`nbd]
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [nbd], network2: [nbd] *)

  Xapi_network.remove_purpose ~__context ~self:network1 ~value:`nbd;
  assert_equal
    ~msg:"remove_purpose should successfully remove the purpose, if present"
    []
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [], network2: [nbd] *)

  Xapi_network.remove_purpose ~__context ~self:network1 ~value:`nbd;
  assert_equal
    ~msg:"remove_purpose should be idempotent and should do nothing when removing an already removed purpose"
    []
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [], network2: [nbd] *)

  OUnit.assert_raises
    ~msg:"Should not be allowed to add 'insecure_nbd' to a network that already has the 'nbd' purpose"
    (* The first parameter of this error is the new purpose we're trying to add, the second is the conflicting one. *)
    Api_errors.(Server_error (network_incompatible_purposes, ["insecure_nbd"; "nbd"]))
    (fun () -> Xapi_network.add_purpose ~__context ~self:network2 ~value:`insecure_nbd);

  (* network1: [], network2: [nbd] *)

  OUnit.assert_raises
    ~msg:"Should not be allowed to add 'insecure_nbd' when another network already has the 'nbd' purpose"
    Api_errors.(Server_error (network_incompatible_purposes, ["insecure_nbd"; "nbd"]))
    (fun () -> Xapi_network.add_purpose ~__context ~self:network1 ~value:`insecure_nbd);

  (* network1: [], network2: [nbd] *)

  Xapi_network.remove_purpose ~__context ~self:network2 ~value:`nbd;
  assert_equal
    ~msg:"remove_purpose should successfully remove the purpose, if present"
    []
    (Db.Network.get_purpose ~__context ~self:network2);

  (* network1: [], network2: [] *)

  Xapi_network.add_purpose ~__context ~self:network1 ~value:`insecure_nbd;
  OUnit.assert_equal
    ~msg:"Should be able to add 'insecure_nbd' purpose when none of the networks have a purpose"
    [`insecure_nbd]
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [insecure_nbd], network2: [] *)

  Xapi_network.add_purpose ~__context ~self:network2 ~value:`insecure_nbd;
  assert_equal
    ~msg:"Should be able to add 'insecure_nbd' purpose when the other network has the same purpose"
    [`insecure_nbd]
    (Db.Network.get_purpose ~__context ~self:network2);

  (* network1: [insecure_nbd], network2: [insecure_nbd] *)

  Xapi_network.remove_purpose ~__context ~self:network1 ~value:`insecure_nbd;
  assert_equal
    ~msg:"remove_purpose should successfully remove the purpose, if present"
    []
    (Db.Network.get_purpose ~__context ~self:network1);

  (* network1: [], network2: [insecure_nbd] *)

  OUnit.assert_raises
    ~msg:"Should not be allowed to add 'nbd' to a network that already has the 'insecure_nbd' purpose"
    Api_errors.(Server_error (network_incompatible_purposes, ["nbd"; "insecure_nbd"]))
    (fun () -> Xapi_network.add_purpose ~__context ~self:network2 ~value:`nbd);

  (* network1: [], network2: [insecure_nbd] *)

  OUnit.assert_raises
    ~msg:"Should not be allowed to add 'nbd' when another network already has the 'insecure_nbd' purpose"
    Api_errors.(Server_error (network_incompatible_purposes, ["nbd"; "insecure_nbd"]))
    (fun () -> Xapi_network.add_purpose ~__context ~self:network1 ~value:`nbd)

let test =
  let ((>:::), (>::)) = OUnit.((>:::), (>::)) in
  "test_network" >:::
  [ "test_purpose_setters" >:: test_purpose_setters ]
