open OUnit


let assert_levels brand (err, warn, info, debug) =
        assert_equal (Debug.is_disabled brand Syslog.Err)     err;
        assert_equal (Debug.is_disabled brand Syslog.Warning) warn;
        assert_equal (Debug.is_disabled brand Syslog.Info)    info;
        assert_equal (Debug.is_disabled brand Syslog.Debug)   debug
  

let test_default_levels () =
        Debug.reset_levels ();

        assert_levels "some brand" (false, false, false, false);
        assert_levels "some other brand" (false, false, false, false)


let test_debug_enable_disable () =
        Debug.reset_levels ();

        Debug.disable "xapi" ~level:Syslog.Info;
        assert_levels "xapi" (false, false, true, false);

        Debug.enable "xapi" ~level:Syslog.Info;
        assert_levels "xapi" (false, false, false, false)


let test_debug_enable_disable_independent () =
        Debug.reset_levels ();

        Debug.enable "xapi" ~level:Syslog.Warning;
        assert_levels "xapi" (false, false, false, false);

        Debug.disable "xenopsd" ~level:Syslog.Err;
        assert_levels "xenopsd" (true, false, false, false);
        assert_levels "xapi"    (false, false, false, false);

        Debug.enable "xenopsd" ~level:Syslog.Err;
        assert_levels "xenopsd" (false, false, false, false);
        assert_levels "xapi"    (false, false, false, false)


let test_debug_set_level () =
        Debug.reset_levels ();

        assert_levels "xapi"    (false, false, false, false);
        assert_levels "other"   (false, false, false, false);

        Debug.set_level Syslog.Debug;
        assert_levels "xapi"    (false, false, false, false);
        assert_levels "other"   (false, false, false, false);

        Debug.set_level Syslog.Info;
        assert_levels "xapi"    (false, false, false, true);
        assert_levels "other"   (false, false, false, true);

        Debug.set_level Syslog.Warning;
        assert_levels "xapi"    (false, false, true, true);
        assert_levels "other"   (false, false, true, true);

        Debug.set_level Syslog.Err;
        assert_levels "xapi"    (false, true, true, true);
        assert_levels "other"   (false, true, true, true)


let test_debug_set_level_multiple_loggers () = 
        let _ = (module Debug.Make(struct let name = "aaaa" end) : Debug.DEBUG) in
        let _ = (module Debug.Make(struct let name = "bbbb" end) : Debug.DEBUG) in
        Debug.reset_levels ();

        assert_levels "aaaa"    (false, false, false, false);
        assert_levels "bbbb"    (false, false, false, false);

        (* Set level explicitly on aaaa *)
        Debug.disable ~level:Syslog.Err "aaaa";
        assert_levels "aaaa"    (true, false, false, false);
        assert_levels "bbbb"    (false, false, false, false);

        (* Set default level.   Err should still be disabled for aaaa *)
        Debug.set_level Syslog.Warning;
        assert_levels "aaaa"    (true, false, true, true);
        assert_levels "bbbb"    (false, false, true, true)

let tests =
  "debug" >:::
    [
      "Test default levels" >:: test_default_levels; 
      "Test Debug.enable / disable" >:: test_debug_enable_disable;
      "Test Debug.enable / disable on different brands" >:: test_debug_enable_disable_independent;
      "Test Debug.set_level" >:: test_debug_set_level;
      "Test Debug.set_level (multiple loggers)">:: test_debug_set_level_multiple_loggers;
    ]
