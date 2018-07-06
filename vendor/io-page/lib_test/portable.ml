open OUnit

let test_alignment () =
  for i = 0 to 1000 do
    let page = Io_page.get 1 in
    let addr = Io_page.get_addr page in
    assert_equal ~printer:Nativeint.to_string 0n Nativeint.(rem addr 4096n)
  done

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test Io_page allocator";

  let suite = "io_page" >::: [
    "check_alignment" >:: test_alignment;
  ] in
  run_test_tt ~verbose:!verbose suite
