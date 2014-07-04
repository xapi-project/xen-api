open OUnit

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test channel passing";

  let tests = Channel_test.tests 
    @ Config_file_test.tests
  in

  let suite = "xcp-idl" >::: tests in

  run_test_tt ~verbose:!verbose suite
