let assert_levels brand (crit, err, warn, info, debug) =
  Alcotest.(check bool)
    (brand ^ " critical")
    (Debug.is_disabled brand Syslog.Crit)
    crit ;
  Alcotest.(check bool)
    (brand ^ " error")
    (Debug.is_disabled brand Syslog.Err)
    err ;
  Alcotest.(check bool)
    (brand ^ " warning")
    (Debug.is_disabled brand Syslog.Warning)
    warn ;
  Alcotest.(check bool)
    (brand ^ " info")
    (Debug.is_disabled brand Syslog.Info)
    info ;
  Alcotest.(check bool)
    (brand ^ " debug")
    (Debug.is_disabled brand Syslog.Debug)
    debug

let test_default_levels () =
  assert_levels "some unused brand" (false, false, false, false, false) ;
  assert_levels "some other unused brand" (false, false, false, false, false)

let test_debug_disable () =
  Debug.set_level Syslog.Debug ;
  Debug.disable "xapi" ~level:Syslog.Info ;
  assert_levels "xapi" (false, false, false, true, false) ;
  Debug.disable "xenopsd" ~level:Syslog.Err ;
  assert_levels "xenopsd" (false, true, false, false, false) ;
  assert_levels "xapi" (false, false, false, true, false)

let test_debug_set_level () =
  Debug.set_level Syslog.Debug ;
  assert_levels "brand1" (false, false, false, false, false) ;
  assert_levels "other" (false, false, false, false, false) ;
  Debug.set_level Syslog.Info ;
  assert_levels "brand1" (false, false, false, false, true) ;
  assert_levels "other" (false, false, false, false, true) ;
  Debug.set_level Syslog.Warning ;
  assert_levels "brand1" (false, false, false, true, true) ;
  assert_levels "other" (false, false, false, true, true) ;
  Debug.set_level Syslog.Err ;
  assert_levels "brand1" (false, false, true, true, true) ;
  assert_levels "other" (false, false, true, true, true) ;
  Debug.set_level Syslog.Crit ;
  assert_levels "brand1" (false, true, true, true, true) ;
  assert_levels "other" (false, true, true, true, true)

let test_debug_set_level_multiple_loggers () =
  let _ = (module Debug.Make (struct let name = "aaaa" end) : Debug.DEBUG) in
  let _ = (module Debug.Make (struct let name = "bbbb" end) : Debug.DEBUG) in
  Debug.set_level Syslog.Debug ;
  assert_levels "aaaa" (false, false, false, false, false) ;
  assert_levels "bbbb" (false, false, false, false, false) ;
  (* Set level explicitly on aaaa *)
  Debug.disable ~level:Syslog.Err "aaaa" ;
  assert_levels "aaaa" (false, true, false, false, false) ;
  assert_levels "bbbb" (false, false, false, false, false) ;
  (* Set default level. Err should still be disabled for aaaa *)
  Debug.set_level Syslog.Warning ;
  assert_levels "aaaa" (false, true, false, true, true) ;
  assert_levels "bbbb" (false, false, false, true, true)

let tests =
  let open Alcotest in
  [
    test_case "Test default levels" `Quick test_default_levels
  ; test_case "Test Debug.disable" `Quick test_debug_disable
  ; test_case "Test Debug.set_level" `Quick test_debug_set_level
  ; test_case "Test Debug.set_level (multiple loggers)" `Quick
      test_debug_set_level_multiple_loggers
  ]
