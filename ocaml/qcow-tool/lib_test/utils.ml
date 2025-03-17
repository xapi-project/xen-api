(*
 * Copyright (C) 2016 Unikernel Systems
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

let debug fmt =
  Printf.ksprintf (fun s ->
      Printf.fprintf stderr "%s\n%!" s
    ) fmt

let read_lines oc =
  let rec aux acc =
    let line =
      try Some (input_line oc)
      with End_of_file -> None
    in
    match line with
    | Some l -> aux (l :: acc)
    | None   -> List.rev acc
  in
  aux []

let or_failwith = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let ignore_output (_: (string list * string list)) = ()

type process = int * (in_channel * out_channel * in_channel) * string

let check_exit_status cmdline = function
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED n -> debug "%s failed" cmdline; Error (`Msg (cmdline ^ ": " ^ (string_of_int n)))
  | Unix.WSIGNALED n -> debug "%s killed by signal %d" cmdline n; Error (`Msg (cmdline ^ " killed by signal %d" ^ (string_of_int n)))
  | Unix.WSTOPPED n -> debug "%s stopped by signal %d" cmdline n; Error (`Msg (cmdline ^ " stopped by signal %d" ^ (string_of_int n)))

let start cmd args : process =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  let pid = Unix.create_process cmd (Array.of_list (cmd :: args)) stdin_r stdout_w stderr_w in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  let ic = Unix.out_channel_of_descr stdin_w in
  let oc = Unix.in_channel_of_descr stdout_r in
  let ec = Unix.in_channel_of_descr stderr_r in
  pid, (oc, ic, ec), Printf.sprintf "%s %s" cmd (String.concat " " args)

let signal (pid, _, _) s = Unix.kill pid s

let wait' (pid, (oc, ic, ec), cmdline) =
  close_out ic;
  close_in oc;
  close_in ec;
  let _, exit_status =
    let rec loop () =
      try
        Unix.waitpid [] pid
      with Unix.Unix_error(Unix.EINTR, _, _) -> loop () in
    loop () in
  check_exit_status cmdline exit_status

let wait (pid, (oc, ic, ec), cmdline) =
  or_failwith @@ wait' (pid, (oc, ic, ec), cmdline)

let run cmd args =
  let pid, (oc, ic, ec), cmdline = start cmd args in
  let out = read_lines oc in
  let err = read_lines ec in
  match wait' (pid, (oc, ic, ec), cmdline) with
  | Ok _ -> out, err
  | Error (`Msg m) -> failwith (m ^ "\n" ^ (String.concat "\n" out) ^ "\n" ^ (String.concat "\n" err))

(* No need for data integrity during tests *)
module UnsafeBlock = struct
  include Block
  let flush _ = Lwt.return (Ok ())
end

let truncate path =
  let open Lwt.Infix in
  Lwt_unix.openfile path [ Unix.O_CREAT; Unix.O_TRUNC ] 0o0644
  >>= fun fd ->
  Lwt_unix.close fd

(* Create a temporary directory for our images. We want these to be
   manually examinable afterwards, so we give images human-readable names *)
let test_dir =
  (* a bit racy but if we lose, the test will simply fail *)
  let path = Filename.temp_file "ocaml-qcow" "" in
  Unix.unlink path;
  Unix.mkdir path 0o0755;
  debug "Creating temporary files in %s" path;
  path

let malloc (length: int) =
  let npages = (length + 4095)/4096 in
  Cstruct.sub Io_page.(to_cstruct (get npages)) 0 length

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.0)
end
