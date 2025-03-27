module D = Debug.Make (struct let name = __MODULE__ end)

let test_identity () =
  let specs =
    [
      ((Some "XenCenter2024", "u1000"), "u1000/XenCenter2024")
    ; ((None, "u1001"), "u1001")
    ; ((None, "Special!@#"), "Special")
    ; ((Some "With-Hyphen", "123"), "123/WithHyphen")
    ; ((Some "", ""), "root")
    ; ((Some " Xen Center 2024 ", ", u 1000 "), "u1000/XenCenter2024")
    ; ((Some "Xen Center ,/@.~# 2024", "root"), "root/XenCenter2024")
    ; ((Some "XenCenter 2024.3.18", ""), "root/XenCenter2024318")
    ; ((Some "", "S-R-X-Y1-Y2-Yn-1-Yn"), "SRXY1Y2Yn1Yn")
    ; ( (Some "XenCenter2024", "S-R-X-Y1-Y2-Yn-1-Yn")
      , "SRXY1Y2Yn1Yn/XenCenter2024"
      )
    ]
  in

  let test_make ((user_agent, subject_sid), expected_identity) =
    let actual_identity =
      Tgroup.Description.Identity.(make ?user_agent subject_sid |> to_string)
    in
    Alcotest.(check string)
      "Check expected identity" expected_identity actual_identity
  in
  List.iter test_make specs

let test_of_creator () =
  let dummy_identity =
    Tgroup.Description.Identity.make ~user_agent:"XenCenter2024" "root"
  in
  let specs =
    [
      ((None, None, None, None), "external/unauthenticated")
    ; ((Some true, None, None, None), "external/intrapool")
    ; ( ( Some true
        , Some Tgroup.Description.Endpoint.External
        , Some dummy_identity
        , Some "sm"
        )
      , "external/intrapool"
      )
    ; ( ( Some true
        , Some Tgroup.Description.Endpoint.Internal
        , Some dummy_identity
        , Some "sm"
        )
      , "external/intrapool"
      )
    ; ( ( None
        , Some Tgroup.Description.Endpoint.Internal
        , Some dummy_identity
        , Some "cli"
        )
      , "internal/cli"
      )
    ; ( (None, None, Some dummy_identity, Some "sm")
      , "external/authenticated/root/XenCenter2024"
      )
    ]
  in
  let test_make ((intrapool, endpoint, identity, originator), expected_group) =
    let originator =
      Option.map Tgroup.Description.Originator.of_string originator
    in
    let actual_group =
      Tgroup.Description.(
        Creator.make ?intrapool ?endpoint ?identity ?originator ()
        |> of_creator
        |> to_string
      )
    in
    Alcotest.(check string) "Check expected group" expected_group actual_group
  in
  List.iter test_make specs

let tests =
  [
    ("identity make", `Quick, test_identity)
  ; ("group of creator", `Quick, test_of_creator)
  ]

let () = Alcotest.run "Tgroup library" [("Thread classification", tests)]
