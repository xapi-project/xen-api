module D = Debug.Make (struct let name = "test_tracing" end)

let attr_to_string (key, value) = Printf.sprintf "%s:%s" key value

let attr_compare attr1 attr2 =
  String.compare (attr_to_string attr1) (attr_to_string attr2)

let attr_testable =
  let pp = Fmt.of_to_string attr_to_string in

  Alcotest.testable pp (fun (k1, v1) (k2, v2) -> k1 = k2 && v1 = v2)

let assert_provider_enabled provider flag =
  Alcotest.(check bool)
    "Check state of trace provider" flag
    (Tracing.TracerProvider.get_enabled provider)

let assert_provider_attrs provider attributes =
  Alcotest.(check (slist attr_testable attr_compare))
    "Check attributes of trace provider" attributes
    (Tracing.TracerProvider.get_attributes provider)

let assert_provider_name_label provider label =
  Alcotest.(check string)
    "Check label of trace provider" label
    (Tracing.TracerProvider.get_name_label provider)

let assert_provider_endpoints provider endpoints =
  Alcotest.(check (list string))
    "Check endpoints of trace provider" endpoints
    (provider
    |> Tracing.TracerProvider.get_endpoints
    |> List.map Tracing.endpoint_to_string
    )

let assert_observe_mode flag =
  Alcotest.(check bool)
    "Check observe mode of library" flag (Tracing.get_observe ())

let uuid1, uuid2, uuid3 =
  Uuidx.(to_string (make ()), to_string (make ()), to_string (make ()))

let http_endpoint = "http://example.com:9411/api/v2/spans"

let with_observe_mode_check flag f = f () ; assert_observe_mode flag

let get_provider name_label =
  Tracing.TracerProvider.get_tracer_providers ()
  |> List.filter (fun provider ->
         String.equal
           (Tracing.TracerProvider.get_name_label provider)
           name_label
     )
  |> List.hd

let create_with (enabled, attributes, endpoints, name_label, uuid) =
  let () =
    Tracing.TracerProvider.create ~enabled ~attributes ~endpoints ~name_label
      ~uuid
  in
  get_provider name_label

let test_destroy_all_providers uuids =
  let () = List.iter (fun uuid -> Tracing.TracerProvider.destroy ~uuid) uuids in
  assert_observe_mode false

let test_create_and_destroy () =
  let test_create_with (enabled, attributes, endpoints, name_label, uuid) =
    let provider =
      create_with (enabled, attributes, endpoints, name_label, uuid)
    in
    assert_provider_enabled provider enabled ;
    assert_provider_attrs provider attributes ;
    assert_provider_endpoints provider endpoints ;
    assert_provider_name_label provider name_label
  in

  let uuids = [uuid1; uuid2; uuid3] in

  let provider_confs_enable_observe =
    [
      ( true
      , [("enabled", "true")]
      , [Tracing.bugtool_name]
      , "dummy_test_provider_1"
      , uuid1
      )
    ; ( false
      , []
      , [Tracing.bugtool_name; http_endpoint]
      , "dummy_test_provider_2"
      , uuid2
      )
    ; ( false
      , [("enabled", "false"); ("is_test", "true")]
      , [http_endpoint]
      , "dummy_test_provider_3"
      , uuid3
      )
    ]
  in
  let provider_confs_disable_observe =
    [
      ( false
      , [("enabled", "false")]
      , [Tracing.bugtool_name]
      , "dummy_test_provider_1"
      , uuid1
      )
    ; ( false
      , []
      , [Tracing.bugtool_name; http_endpoint]
      , "dummy_test_provider_2"
      , uuid2
      )
    ; ( false
      , [("enabled", "false"); ("is_test", "true")]
      , [http_endpoint]
      , "dummy_test_provider_3"
      , uuid3
      )
    ]
  in

  (* We start with no tracer providers, therefore, we expect the observe mode to
     be disbled. *)
  assert_observe_mode false ;

  let test_provider_conf conf expected_mode_state =
    with_observe_mode_check expected_mode_state (fun () ->
        List.iter test_create_with conf
    ) ;
    test_destroy_all_providers uuids ;
    with_observe_mode_check expected_mode_state (fun () ->
        List.iter test_create_with (List.rev conf)
    ) ;
    test_destroy_all_providers uuids
  in

  test_provider_conf provider_confs_enable_observe true ;
  test_provider_conf provider_confs_disable_observe false

let test_set_tracer_provider () =
  let test_set_with provider (enabled, attributes, endpoints, uuid) =
    Tracing.TracerProvider.set ~enabled ~attributes ~endpoints ~uuid () ;
    let updated_provider =
      provider |> Tracing.TracerProvider.get_name_label |> get_provider
    in
    assert_provider_enabled updated_provider enabled ;
    assert_provider_attrs updated_provider attributes ;
    assert_provider_endpoints updated_provider endpoints
  in

  let provider1 =
    create_with
      (false, [], [Tracing.bugtool_name], "dummy_test_provider_1", uuid1)
  in

  let provider2 =
    create_with
      (false, [], [Tracing.bugtool_name], "dummy_test_provider_2", uuid2)
  in

  let new_provider1_confs =
    ( ( true
      , [("test_set", "true")]
      , [Tracing.bugtool_name; http_endpoint]
      , uuid1
      )
    , (false, [], [Tracing.bugtool_name; http_endpoint], uuid1)
    )
  in

  let new_provider2_confs =
    ( ( true
      , [("test_set", "true"); ("dummy_key", "dummy_value")]
      , [Tracing.bugtool_name; http_endpoint]
      , uuid2
      )
    , (false, [], [Tracing.bugtool_name; http_endpoint], uuid2)
    )
  in

  assert_observe_mode false ;

  with_observe_mode_check true (fun () ->
      test_set_with provider1 (fst new_provider1_confs)
  ) ;

  with_observe_mode_check true (fun () ->
      test_set_with provider2 (fst new_provider2_confs)
  ) ;
  with_observe_mode_check true (fun () ->
      test_set_with provider1 (snd new_provider1_confs)
  ) ;

  with_observe_mode_check false (fun () ->
      test_set_with provider2 (snd new_provider2_confs)
  ) ;

  test_destroy_all_providers [uuid1; uuid2]

let test =
  [
    ("Create and destroy tracer providers", `Quick, test_create_and_destroy)
  ; ("Set tracer provider", `Quick, test_set_tracer_provider)
  ]

let () = Alcotest.run "Tracing library" [("trace providers", test)]
