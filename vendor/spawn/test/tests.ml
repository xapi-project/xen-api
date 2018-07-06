let show_raise f =
  try
    ignore (f () : int)
  with exn ->
    Printf.printf "raised %s" (Printexc.to_string exn)

let%expect_test "non-existing program" =
  show_raise (fun () ->
    Spawn.spawn () ~prog:"/doesnt-exist" ~argv:["blah"]);
  [%expect {|
    raised Unix.Unix_error(Unix.ENOENT, "execve", "/doesnt-exist")
  |}]

let%expect_test "non-existing dir" =
  show_raise (fun () ->
    Spawn.spawn () ~prog:"/bin/true" ~argv:["true"]
      ~cwd:(Path "/doesnt-exist"));
  [%expect {|
    raised Unix.Unix_error(Unix.ENOENT, "chdir", "/doesnt-exist")
  |}]

let wait pid =
  match snd (Unix.waitpid [] pid) with
  | WEXITED   0 -> ()
  | WEXITED   n -> Printf.ksprintf failwith "exited with code %d" n
  | WSIGNALED n -> Printf.ksprintf failwith "got signal %d" n
  | WSTOPPED  _ -> assert false

let%expect_test "cwd:Fd" =
  let fd = Unix.openfile "/tmp" [O_RDONLY] 0 in
  wait (Spawn.spawn () ~prog:"/bin/pwd" ~argv:["pwd"] ~cwd:(Fd fd));
  Unix.close fd;
  [%expect {|
    /tmp
  |}]

let%expect_test "cwd:Fd (invalid)" =
  show_raise (fun () ->
    Spawn.spawn () ~prog:"/bin/pwd" ~argv:["pwd"] ~cwd:(Fd Unix.stdin));
  [%expect {|
    raised Unix.Unix_error(Unix.ENOTDIR, "fchdir", "")
  |}]

let%expect_test "inheriting stdout with close-on-exec set" =
  Unix.set_close_on_exec Unix.stdout;
  wait (Spawn.spawn () ~prog:"/bin/echo" ~argv:["echo"; "hello world"]);
  [%expect {| hello world |}]
