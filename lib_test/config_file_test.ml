let test_config_file () =
  let open Xcp_service.Config_file in
  let tests =
    [
      ("", None)
    ; ("# Foo", None)
    ; ("whatever", None)
    ; ("foo=true", Some ("foo", "true"))
    ; ("n=2 # and no more", Some ("n", "2"))
    ; ("n = 2 \t# and no more", Some ("n", "2"))
    ; ("n = 'test'  # comment", Some ("n", "test"))
    ; ("n = \"test\" # comment", Some ("n", "test"))
    ; ( "   n\t\t \t   =       'foo bar baz'\t\t\t  # comment"
      , Some ("n", "foo bar baz") )
    ; ( "   n\t\t \t   =       foo bar baz\t\t\t  # comment"
      , Some ("n", "foo bar baz") )
    ; ("n = 'foo bar baz ' # comment", Some ("n", "foo bar baz "))
    ]
  in
  List.iter
    (fun (x, y) ->
      Alcotest.(check (option (pair string string)))
        ("parse output for " ^ x) (parse_line x) y)
    tests

let tests = [("check config file parsing", `Quick, test_config_file)]
