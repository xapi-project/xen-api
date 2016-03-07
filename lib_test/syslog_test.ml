open OUnit

(* Test which log levels are masked off by each threshhold level.
   Levels are ordered by severity - Err is the highest, Debug the
   lowest.   If the log threshold is set to Warning, the less
   severe levels (Info and Debug) should be masked out. *)

let assert_masked ~threshold (err, warn, info, debug) =
	let open Syslog in
	assert ((is_masked ~threshold:threshold Err)     = err);
	assert ((is_masked ~threshold:threshold Warning) = warn);
	assert ((is_masked ~threshold:threshold Info)    = info);
	assert ((is_masked ~threshold:threshold Debug)   = debug)

let test_is_masked () =
	let open Syslog in
	assert_masked ~threshold:Debug   (false,  false, false, false);
	assert_masked ~threshold:Info    (false,  false, false, true);
	assert_masked ~threshold:Warning (false,  false, true,  true);
	assert_masked ~threshold:Err     (false,  true,  true,  true)

let tests =
  "debug" >:::
    [
      "Test Syslog.is_masked" >:: test_is_masked; 
    ]
