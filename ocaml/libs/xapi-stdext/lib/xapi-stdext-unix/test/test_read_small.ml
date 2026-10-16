module Unixext = Xapi_stdext_unix.Unixext

let max_bytes = 8

(* The reason in an [Error] is for logs, so only the constructor is compared *)
let result = Alcotest.(result (option string) pass)

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "test_read_small.%d" (Unix.getpid ()))
  in
  Unix.mkdir dir 0o700 ;
  Fun.protect ~finally:(fun () -> Unixext.rm_rec dir) (fun () -> f dir)

let write path content =
  Out_channel.with_open_bin path (fun oc -> output_string oc content)

(* [setup dir] creates the entries and returns the path to read *)
let check name setup expected =
  ( name
  , `Quick
  , fun () ->
      with_temp_dir (fun dir ->
          let path = setup dir in
          Alcotest.check result name expected
            (Unixext.read_small_regular_file ~max_bytes path)
      )
  )

let file content dir =
  let path = Filename.concat dir "flag" in
  write path content ; path

let entry make dir =
  let path = Filename.concat dir "flag" in
  make path ; path

let tests =
  [
    check "small file" (file "1\n") (Ok (Some "1\n"))
  ; check "file of max_bytes" (file "12345678") (Ok (Some "12345678"))
  ; check "file over max_bytes" (file "123456789") (Error "")
  ; check "missing file" (fun dir -> Filename.concat dir "flag") (Ok None)
  ; check "parent is a file"
      (fun dir -> Filename.concat (file "1" dir) "flag")
      (Ok None)
  ; check "symlink to a small file"
      (fun dir ->
        let target = Filename.concat dir "target" in
        write target "true" ;
        entry (Unix.symlink target) dir
      )
      (Ok (Some "true"))
  ; check "dangling symlink" (entry (Unix.symlink "/nonexistent/flag")) (Ok None)
  ; check "symlink to a device" (entry (Unix.symlink "/dev/null")) (Error "")
  ; check "directory" (entry (fun path -> Unix.mkdir path 0o700)) (Error "")
  ; check "FIFO with no writer"
      (entry (fun path -> Unix.mkfifo path 0o600))
      (Error "")
  ]

let () = Alcotest.run "Unixext.read_small_regular_file" [("read", tests)]
