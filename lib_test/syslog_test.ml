open OUnit

let assert_threshold level (err, warn, info, debug) =
	let open Syslog in
	assert ((is_masked ~threshold:Err     level) = err);
	assert ((is_masked ~threshold:Warning level) = warn);
	assert ((is_masked ~threshold:Info    level) = info);
	assert ((is_masked ~threshold:Debug   level) = debug)

let test_is_masked () =
	let open Syslog in
	assert_threshold Debug   (true,  true,  true,  false);
	assert_threshold Info    (true,  true,  false, false);
	assert_threshold Warning (true,  false, false, false);
	assert_threshold Err     (false, false, false, false)

let tests =
  "debug" >:::
    [
      "Test Syslog.is_masked" >:: test_is_masked; 
    ]
