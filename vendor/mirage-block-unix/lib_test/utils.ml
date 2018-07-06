(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

let ignore_string (_: string) = ()

let log fmt =
  Printf.ksprintf
    (fun s ->
       output_string stderr s;
       output_string stderr "\n";
       flush stderr;
    ) fmt
let debug fmt = log fmt
let warn fmt = debug fmt
let error fmt = debug fmt

let alloc bytes =
  let pages = Io_page.(to_cstruct (get ((bytes + 4095) / 4096))) in
  Cstruct.sub pages 0 bytes

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let rm_f x =
  try
    Unix.unlink x;
    debug "rm %s" x
  with _ ->
    debug "%s already deleted" x;
    ()

let canonicalise x =
  if not(Filename.is_relative x)
  then x
  else begin
    let rec split acc remaining =
      try
        let i = String.index remaining ':' in
        let first = String.sub remaining 0 i in
        if i < String.length remaining then begin
          split (first :: acc) (String.sub remaining (i + 1) (String.length remaining - i - 1))
        end else List.rev acc
      with Not_found ->
        List.rev (remaining :: acc) in
    (* Search the PATH for the executable *)
    let paths = split [] (Sys.getenv "PATH") in
    let first_hit = List.fold_left (fun found path -> match found with
        | Some hit -> found
        | None ->
          let possibility = Filename.concat path x in
          if Sys.file_exists possibility
          then Some possibility
          else None
      ) None paths in
    match first_hit with
    | None ->
      warn "Failed to find %s on $PATH ( = %s)" x (Sys.getenv "PATH");
      x
    | Some hit -> hit
  end

exception Bad_exit of int * string * string list * string * string

let rec waitpid pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid

let run ?(env= [| |]) ?stdin cmd args =
  let cmd = canonicalise cmd in
  debug "%s %s" cmd (String.concat " " args);
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let to_close = ref [ null ] in
  let close fd =
    if List.mem fd !to_close then begin
      to_close := List.filter (fun x -> x <> fd) !to_close;
      Unix.close fd
    end in
  let read_all fd =
    let b = Buffer.create 128 in
    let tmp = Bytes.make 4096 '\000' in
    let finished = ref false in
    while not !finished do
      let n = Unix.read fd tmp 0 (Bytes.length tmp) in
      Buffer.add_subbytes b tmp 0 n;
      finished := n = 0
    done;
    Buffer.contents b in
  let close_all () = List.iter close !to_close in
  try
    (* stdin is a pipe *)
    let stdin_readable, stdin_writable = Unix.pipe () in
    to_close := stdin_readable :: stdin_writable :: !to_close;
    (* stdout buffers to a temp file *)
    let stdout_filename = Filename.temp_file (Filename.basename Sys.argv.(0)) "stdout" in
    let stdout_readable = Unix.openfile stdout_filename [ Unix.O_RDONLY; Unix.O_CREAT; ] 0o0600 in
    Unix.set_close_on_exec stdout_readable;
    let stdout_writable = Unix.openfile stdout_filename [ Unix.O_WRONLY ] 0o0600 in
    to_close := stdout_readable :: stdout_writable :: !to_close;
    Unix.unlink stdout_filename;
    (* stderr buffers to a temp file *)
    let stderr_filename = Filename.temp_file (Filename.basename Sys.argv.(0)) "stderr" in
    let stderr_readable = Unix.openfile stderr_filename [ Unix.O_RDONLY; Unix.O_CREAT; ] 0o0600 in
    Unix.set_close_on_exec stderr_readable;
    let stderr_writable = Unix.openfile stderr_filename [ Unix.O_WRONLY ] 0o0600 in
    to_close := stderr_readable :: stderr_writable :: !to_close;
    Unix.unlink stderr_filename;

    let pid = Unix.create_process_env cmd (Array.of_list (cmd :: args)) env stdin_readable stdout_writable stderr_writable in
    close stdin_readable;
    close stdout_writable;
    close stderr_writable;

    (* pump the input to stdin while the output is streaming to the unlinked files *)
    begin match stdin with
      | None -> ()
      | Some txt ->
        let n = Unix.write stdin_writable txt 0 (Bytes.length txt) in
        if n <> (Bytes.length txt)
        then failwith (Printf.sprintf "short write to process stdin: only wrote %d bytes" n);
    end;
    close stdin_writable;
    let _, status = waitpid pid in

    let stdout = read_all stdout_readable in
    let stderr = read_all stderr_readable in
    close_all ();

    match status with
    | Unix.WEXITED 0 ->
      stdout
    | Unix.WEXITED n ->
      raise (Bad_exit(n, cmd, args, stdout, stderr))
    | _ ->
      failwith (Printf.sprintf "%s %s failed" cmd (String.concat " " args))
  with e ->
    close_all ();
    raise e

let find_unused_file () =
  (* Find a filename which doesn't exist *)
  let rec does_not_exist i =
    let name = Printf.sprintf "%s/mirage-block-test.%d.%d"
        (Filename.get_temp_dir_name ()) (Unix.getpid ()) i in
    if Sys.file_exists name
    then does_not_exist (i + 1)
    else name in
  does_not_exist 0

let with_temp_file f =
  let path = find_unused_file () in
  finally
    (fun () ->
       let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o0644 in
       finally
         (fun () ->
            ignore(Unix.lseek fd 1048575 Unix.SEEK_CUR);
            ignore(Unix.write fd (Bytes.make 1 '\000') 0 1)) (* will write at least 1 *)
         (fun () -> Unix.close fd);
       f path
    ) (fun () ->
        rm_f path
      )

let with_hdiutil path f =
  let dev = String.trim (run "hdiutil" [ "attach"; "-imagekey"; "diskimage-class=CRawDiskImage"; "-nomount"; path ]) in
  finally
    (fun () -> f dev)
    (fun () ->
       let rec loop = function
         | 0 -> failwith (Printf.sprintf "hdiutil detach %s keeps failing" dev)
         | n ->
           try
             ignore_string (run "hdiutil" [ "detach"; dev ])
           with _ ->
             Unix.sleep 1;
             loop (n - 1) in
       loop 5
    )

let with_losetup path f =
  let dev =
    ignore_string (run "sudo" [ "losetup"; "-f"; path ]);
    (* /dev/loop0: [fd00]:1973802 (/tmp/SR.createc04251volume) *)
    let line = run "sudo" [ "losetup"; "-j"; path ] in
    try
      let i = String.index line ' ' in
      String.sub line 0 (i - 1)
    with e ->
      error "Failed to parse output of losetup -j: [%s]" line;
      ignore_string (run "losetup" [ "-d"; path ]);
      failwith (Printf.sprintf "Failed to parse output of losetup -j: [%s]" line) in
  finally
    (fun () -> f dev)
    (fun () ->
       ignore_string (run "sudo" [ "losetup"; "-d"; dev ])
    )

let with_temp_volume path f =
  (if String.trim (run "uname" []) = "Darwin"
   then with_hdiutil
   else with_losetup) path f

exception Cstruct_differ

let cstruct_equal a b =
  let check_contents a b =
    try
      for i = 0 to Cstruct.len a - 1 do
        let a' = Cstruct.get_char a i in
        let b' = Cstruct.get_char b i in
        if a' <> b' then raise Cstruct_differ
      done;
      true
    with _ -> false in
  (Cstruct.len a = (Cstruct.len b)) && (check_contents a b)

