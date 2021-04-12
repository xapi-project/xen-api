(** file descriptor with location *)
type raw = Unix.file_descr * string

type t = raw Safe.t

let borrow_exn t = Safe.borrow_exn t |> fst

let ( ! ) = borrow_exn

let release (chan, _) = Unix.close chan

(* Calling functions that may take locks inside a finaliser can lead to
   deadlocks, see https://github.com/ocaml/ocaml/issues/8794 and
   https://github.com/xapi-project/xcp-idl/pull/288.

   This can be worked around by running the finaliser code on a separate thread,
   however that needs lockless data structures for safety (Mutex and Event is
   not safe to use within a finaliser). Although I have code for this it is too
   complex/error-prone to be used for code that is rarely run.

   Instead have leak tracing only for Unix file descriptors where we print
   errors to stderr instead of using a logging library. This is similar to what
   OCaml already provides for channels, see [Sys.enable_runtime_warnings]. *)
let on_finalise_leaked (chan, loc) =
  let enabled = Sys.runtime_warnings_enabled () in
  if enabled then
    Printf.eprintf "[unix_fd]: resource leak detected, allocated at %s\n%!" loc ;
  try Unix.close chan
  with e ->
    if enabled then (
      Printexc.print_backtrace stderr ;
      Printf.eprintf "[unix_fd]: close failed: %s (allocated at %s)\n%!"
        (Printexc.to_string e) loc
    )

let within chan ~loc =
  Safe.within @@ Safe.create ~on_finalise_leaked ~release (chan, loc)

let pair (fd1, fd2) ~loc f =
  within ~loc fd1 (fun fd1 -> within ~loc fd2 (fun fd2 -> f fd1 fd2))

let with_pipe () = pair @@ Unix.pipe ()

let with_socketpair domain typ proto ~loc f =
  let fd1, fd2 = Unix.socketpair domain typ proto in
  within ~loc fd1 (fun fd1 -> within ~loc fd2 (fun fd2 -> f fd1 fd2))

let with_fd alloc = alloc () |> within

let with_open_connection addr ~loc f =
  let open Unix in
  with_fd ~loc (fun () ->
      socket ~cloexec:true (domain_of_sockaddr addr) SOCK_STREAM 0)
  @@ fun s -> connect !s addr ; f s

let with_ic fd =
  (* A file descriptor cannot be safely shared between an [in] and [out] channel.
   * Unix.open_connection does this but if you close both channels you get EBADF.
   *)
  Safe.within
  @@ Safe.create ~release:close_in_noerr
       (Unix.in_channel_of_descr (Unix.dup fd))

let with_oc fd =
  Safe.within
  @@ Safe.create ~release:close_out_noerr
       (Unix.out_channel_of_descr (Unix.dup fd))

let with_channels t f =
  let fd = !t in
  with_ic fd @@ fun ic ->
  with_oc fd @@ fun oc ->
  (* the channels are using [dup]-ed FDs, close original now *)
  Safe.safe_release t ;
  f Safe.(borrow_exn ic, borrow_exn oc)

let safe_close = Safe.safe_release
