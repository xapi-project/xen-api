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

let outcome =
  Alcotest.of_pp (fun ppf x ->
      Format.pp_print_string ppf
        ( match x with
        | `Changed ->
            "changed"
        | `Empty ->
            "empty"
        | `Unchanged ->
            "unchanged"
        )
  )

let test_combine_bundles () =
  let file contents =
    let path = Filename.temp_file "bundle" ".pem" in
    Out_channel.with_open_bin path (fun oc -> output_string oc contents) ;
    path
  in
  let legacy = file "LEGACY" in
  let purpose = file "PURPOSE" in
  let empty = file "" in
  let target = file "" in
  let missing = target ^ ".missing" in
  let combine = Xapi_stunnel_server.combine_bundles ~target in
  let contents () = In_channel.with_open_bin target In_channel.input_all in
  let remove path = try Sys.remove path with Sys_error _ -> () in
  Fun.protect ~finally:(fun () ->
      List.iter remove [legacy; purpose; empty; target]
  )
  @@ fun () ->
  Alcotest.check outcome "two bundles" `Changed
    (combine [legacy; missing; purpose]) ;
  Alcotest.(check string) "concatenated" "LEGACY\nPURPOSE" (contents ()) ;
  Alcotest.check outcome "same bundles" `Unchanged (combine [legacy; purpose]) ;
  Alcotest.check outcome "one bundle" `Changed (combine [legacy]) ;
  Alcotest.(check string) "legacy only" "LEGACY" (contents ()) ;
  Alcotest.check outcome "nothing to trust" `Empty (combine [empty; missing]) ;
  Alcotest.(check bool) "target removed" false (Sys.file_exists target)

let test_pinned_client_auth_refused () =
  let __context = Test_common.make_test_database () in
  Alcotest.check_raises "pinning for client_certificate_auth"
    Api_errors.(Server_error (invalid_value, ["ca"; "false"]))
    (fun () ->
      Xapi_pool.install_trusted_certificate ~__context ~self:Ref.null ~ca:false
        ~cert:"" ~purpose:[`client_certificate_auth]
    )

let test =
  pp_hash_test
  @ [
      ("combine_bundles", `Quick, test_combine_bundles)
    ; ( "pinned client_certificate_auth refused"
      , `Quick
      , test_pinned_client_auth_refused
      )
    ]
