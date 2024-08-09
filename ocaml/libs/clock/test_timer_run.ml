let tests_timer = List.map QCheck_alcotest.to_alcotest Test_timer.tests

let () =
  Alcotest.run "Timer" [("Timer", tests_timer); ("Span", Test_timer.tests_span)]
