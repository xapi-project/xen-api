(** This module tests xapi-related PKI code
 *)

let hashables =
  [
    ( "foobar"
    , "C3:AB:8F:F1:37:20:E8:AD:90:47:DD:39:46:6B:3C:89:74:E5:92:C2:FA:38:3D:4A:39:60:71:4C:AE:F0:C4:F2"
    )
  ]

let pp_hash_test =
  List.map
    (fun (hashable, expected) ->
      let test_hash () =
        let digest =
          Digestif.SHA256.(digest_string hashable |> to_raw_string)
        in
        Alcotest.(check string)
          "fingerprints must match" expected
          (Certificates.pp_hash digest)
      in
      ( Printf.sprintf {|Validation of hash printing of "%s"|} hashable
      , `Quick
      , test_hash
      )
    )
    hashables

let test_pinned_client_auth_refused () =
  let __context = Test_common.make_test_database () in
  Alcotest.check_raises "pinning for client_auth"
    Api_errors.(
      Server_error (trusted_certificate_purpose_needs_ca, ["client_auth"])
    )
    (fun () ->
      Xapi_pool.install_trusted_certificate ~__context ~self:Ref.null ~ca:false
        ~cert:"" ~purpose:[`client_auth]
    )

let test_client_auth_needs_permission () =
  let __context = Test_common.make_test_database () in
  let install ?rbac_permissions purpose =
    let session_id = Test_common.make_session ~__context ?rbac_permissions () in
    let __context = Context.make ~session_id "test" in
    match
      Xapi_pool.install_trusted_certificate ~__context ~self:Ref.null ~ca:true
        ~cert:"" ~purpose
    with
    | () ->
        "installed"
    | exception Api_errors.Server_error (code, _) ->
        code
  in
  let permission =
    Rbac_static.permission_pool_install_trusted_certificate_client_auth
      .Db_actions.role_name_label
  in
  (* the empty certificate is rejected only after the permission check *)
  Alcotest.(check string)
    "client_auth without the permission" Api_errors.rbac_permission_denied
    (install [`client_auth]) ;
  Alcotest.(check string)
    "client_auth with the permission" Api_errors.trusted_certificate_invalid
    (install ~rbac_permissions:[permission] [`client_auth]) ;
  Alcotest.(check string)
    "other purposes without the permission"
    Api_errors.trusted_certificate_invalid (install [`ldaps])

let test_enable_client_auth_needs_ca () =
  let __context = Test_common.make_test_database () in
  let add_cert _type purpose =
    Db.Certificate.create ~__context ~ref:(Ref.make ())
      ~uuid:(Test_common.make_uuid ()) ~host:Ref.null
      ~not_before:Clock.Date.epoch ~not_after:Clock.Date.epoch ~fingerprint:""
      ~fingerprint_sha256:"" ~fingerprint_sha1:"" ~name:"" ~_type ~purpose
  in
  let has_ca () = Certificates.Db_util.has_trusted_ca ~__context `client_auth in
  Alcotest.check_raises "enable without a CA"
    Api_errors.(
      Server_error (trusted_certificate_purpose_has_no_ca, ["client_auth"])
    )
    (fun () ->
      Xapi_pool.enable_client_certificate_auth ~__context
        ~self:(Helpers.get_pool ~__context)
        ~name:"client"
    ) ;
  add_cert `pinned [`client_auth] ;
  add_cert `ca [`ldaps] ;
  Alcotest.(check bool) "no CA for client_auth" false (has_ca ()) ;
  add_cert `ca [`client_auth] ;
  Alcotest.(check bool) "a CA for client_auth" true (has_ca ())

let test =
  pp_hash_test
  @ [
      ("pinned client_auth refused", `Quick, test_pinned_client_auth_refused)
    ; ( "client_auth needs its permission"
      , `Quick
      , test_client_auth_needs_permission
      )
    ; ("enable client_auth needs a CA", `Quick, test_enable_client_auth_needs_ca)
    ]
