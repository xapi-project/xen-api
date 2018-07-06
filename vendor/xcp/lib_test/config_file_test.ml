open OUnit

let test_config_file () =
	let open Xcp_service.Config_file in
	let tests = [
	  "", None;
	  "# Foo", None;
	  "whatever", None;
	  "foo=true", Some ("foo","true");
	  "n=2 # and no more", Some ("n","2");
	  "n = 2 \t# and no more", Some ("n","2");
	  "n = 'test'  # comment", Some ("n","test");
	  "n = \"test\" # comment", Some ("n","test");
	  "   n\t\t \t   =       'foo bar baz'\t\t\t  # comment", Some ("n","foo bar baz");
	  "   n\t\t \t   =       foo bar baz\t\t\t  # comment", Some ("n","foo bar baz");
	  "n = 'foo bar baz ' # comment", Some ("n","foo bar baz ");
	] in
	List.iter (fun (x,y) -> assert_equal ~printer:(function | Some (x,y) -> Printf.sprintf "key: '%s', val: '%s'" x y | None -> "Nothing found") (parse_line x) y) tests

let tests =
  "xcp-config-file" >:::
    [
      "check config file parsing" >:: test_config_file;
    ]
