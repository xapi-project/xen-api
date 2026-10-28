module Tls = Migrate_connect.Migration_tls

let tls = Alcotest.testable (Fmt.of_to_string Tls.to_string) ( = )

(* A temporary flag directory with [migration_ktls] holding [content], or
   without it if [content] is [None] *)
let with_flag content f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "test_migrate_connect.%d" (Unix.getpid ()))
  in
  let flag = Filename.concat dir "migration_ktls" in
  Unix.mkdir dir 0o700 ;
  Option.iter
    (fun c -> Out_channel.with_open_bin flag (fun oc -> output_string oc c))
    content ;
  let cleanup () =
    if Sys.file_exists flag then Sys.remove flag ;
    Unix.rmdir dir
  in
  Fun.protect ~finally:cleanup (fun () -> f dir)

let check name ~conf ~flag expected =
  ( name
  , `Quick
  , fun () ->
      Migrate_connect.migration_tls := Tls.of_string conf ;
      with_flag flag (fun dir ->
          Alcotest.check tls name expected
            (Migrate_connect.transport ~feature_flags_path:dir)
      )
  )

let tests =
  [
    check "unset, no flag" ~conf:"" ~flag:None Tls.Stunnel
  ; check "unset, flag 1" ~conf:"" ~flag:(Some "1\n") Tls.Ktls
  ; check "unset, flag TRUE" ~conf:"" ~flag:(Some " TRUE \r\n") Tls.Ktls
  ; check "unset, flag 0" ~conf:"" ~flag:(Some "0\n") Tls.Stunnel
  ; check "unset, flag unrecognised" ~conf:"" ~flag:(Some "yes\n") Tls.Stunnel
  ; check "unset, flag on two lines" ~conf:"" ~flag:(Some "1\n1\n") Tls.Stunnel
  ; check "unset, flag split by newline" ~conf:"" ~flag:(Some "t\nrue\n")
      Tls.Ktls
  ; check "unset, flag too large" ~conf:""
      ~flag:(Some (String.make 4096 ' ' ^ "1"))
      Tls.Stunnel
  ; check "stunnel, flag 1" ~conf:"stunnel" ~flag:(Some "1\n") Tls.Stunnel
  ; check "ktls, flag 0" ~conf:"ktls" ~flag:(Some "0\n") Tls.Ktls
  ; check "ktls, no flag" ~conf:" KTLS " ~flag:None Tls.Ktls
  ; check "unknown value, flag 1" ~conf:"bogus" ~flag:(Some "1\n") Tls.Stunnel
  ]

let () = Alcotest.run "Migrate_connect" [("transport", tests)]
