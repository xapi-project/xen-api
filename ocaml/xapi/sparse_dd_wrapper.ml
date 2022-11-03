(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* Allows xapi to drive the sparse_dd program *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "sparse_dd_wrapper" end)

open D

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

type progress =
  | Started of Forkhelpers.pidty
  | Continuing of float
  | Finished of exn option

type t = {
    m: Mutex.t
  ; c: Condition.t
  ; pid: Forkhelpers.pidty
  ; finished: bool ref
  ; cancelled: bool ref
  ; exn: exn option ref
}

(* Store sparse_dd pids on disk so we can kill them after a xapi restart *)
module State = struct
  type pids = int list [@@deriving rpc]

  let filename = ref "/var/run/nonpersistent/xapi/sparse_dd_pids.json"

  let m = Mutex.create ()

  let load () =
    try Unixext.string_of_file !filename |> Jsonrpc.of_string |> pids_of_rpc
    with _ -> []

  let save pids =
    rpc_of_pids pids
    |> Jsonrpc.to_string
    |> Unixext.write_string_to_file !filename

  let unsafe_add pid =
    let pids = load () in
    save (pid :: pids)

  let unsafe_remove pid =
    let pids = load () in
    save (List.filter (fun x -> x <> pid) pids)

  let add pid = with_lock m (fun () -> unsafe_add pid)

  let remove pid = with_lock m (fun () -> unsafe_remove pid)

  let list () = with_lock m load
end

exception Cancelled

(** Use the new external sparse_dd program *)
let dd_internal progress_cb base prezeroed verify_cert infile outfile size =
  let pipe_read, pipe_write = Unix.pipe () in
  let to_close = ref [pipe_read; pipe_write] in
  let close x =
    if List.mem x !to_close then (
      Unix.close x ;
      to_close := List.filter (fun y -> y <> x) !to_close
    )
  in
  finally
    (fun () ->
      try
        match
          Forkhelpers.with_logfile_fd "sparse_dd" (fun log_fd ->
              let sparse_dd_path = !Xapi_globs.sparse_dd in
              let verify_args =
                match verify_cert with
                | None ->
                    []
                | Some {Stunnel.sni= None; cert_bundle_path; _} ->
                    ["-verify-dest"; "-cert-bundle-path"; cert_bundle_path]
                | Some {Stunnel.sni= Some sni; cert_bundle_path; _} ->
                    [
                      "-verify-dest"
                    ; "-cert-bundle-path"
                    ; cert_bundle_path
                    ; "-sni"
                    ; sni
                    ]
              in
              let args =
                List.concat
                  [
                    [
                      "-machine"
                    ; "-src"
                    ; infile
                    ; "-dest"
                    ; outfile
                    ; "-size"
                    ; Int64.to_string size
                    ; "-good-ciphersuites"
                    ; Constants.good_ciphersuites
                    ]
                  ; (if prezeroed then ["-prezeroed"] else [])
                  ; (match base with None -> [] | Some x -> ["-base"; x])
                  ; verify_args
                  ]
              in
              debug "%s %s" sparse_dd_path (String.concat " " args) ;
              let pid =
                Forkhelpers.safe_close_and_exec None (Some pipe_write)
                  (Some log_fd) [] sparse_dd_path args
              in
              let intpid = Forkhelpers.getpid pid in
              State.add intpid ;
              close pipe_write ;
              progress_cb (Started pid) ;
              (* Read Progress: output from the binary *)
              let open Sparse_encoding in
              Chunk.fold
                (fun () chunk ->
                  let data = Bytes.to_string chunk.Chunk.data in
                  debug "sparse_dd: %s" data ;
                  try
                    Scanf.sscanf data "Progress: %d" (fun progress ->
                        progress_cb (Continuing (float_of_int progress /. 100.))
                    )
                  with e ->
                    Unix.kill (Forkhelpers.getpid pid) Sys.sigterm ;
                    raise e
                )
                () pipe_read ;
              let r = Forkhelpers.waitpid pid in
              State.remove intpid ;
              match r with
              | _, Unix.WEXITED 0 ->
                  progress_cb (Finished None)
              | _, Unix.WEXITED 5 ->
                  error "sparse_dd received NBD error" ;
                  failwith "sparse_dd NBD error"
              | _, Unix.WEXITED n ->
                  error "sparse_dd exit: %d" n ;
                  failwith "sparse_dd"
              | _ ->
                  error "sparse_dd exit with WSTOPPED or WSIGNALED" ;
                  failwith "sparse_dd"
          )
        with
        | Forkhelpers.Success _ ->
            progress_cb (Finished None)
        | Forkhelpers.Failure (log, exn) ->
            error "Failure from sparse_dd: %s raising %s" log
              (Printexc.to_string exn) ;
            raise
              (Api_errors.Server_error
                 (Api_errors.vdi_copy_failed, [Printexc.to_string exn])
              )
      with e ->
        progress_cb (Finished (Some e)) ;
        raise e
    )
    (fun () -> close pipe_read ; close pipe_write)

let dd ?(progress_cb = fun _ -> ()) ?base ~verify_cert prezeroed =
  dd_internal
    (function Continuing x -> progress_cb x | _ -> ())
    base prezeroed verify_cert

let start ?(progress_cb = fun _ -> ()) ?base ~verify_cert prezeroed infile
    outfile size =
  let m = Mutex.create () in
  let c = Condition.create () in
  let pid = ref None in
  let finished = ref false in
  let cancelled = ref false in
  let exn = ref None in
  let thread_progress_cb = function
    | Started pid' ->
        pid := Some pid' ;
        with_lock m (fun () -> Condition.broadcast c)
    | Continuing progress ->
        progress_cb progress
    | Finished exn' ->
        finished := true ;
        exn := exn' ;
        with_lock m (fun () -> Condition.broadcast c)
  in
  let _ =
    Thread.create
      (fun () ->
        dd_internal thread_progress_cb base prezeroed verify_cert infile outfile
          size
      )
      ()
  in
  with_lock m (fun () ->
      while !pid = None && !finished = false && !cancelled = false do
        Condition.wait c m
      done
  ) ;
  match (!pid, !exn) with
  | Some pid, None ->
      {m; c; pid; finished; cancelled; exn}
  | _, Some e ->
      raise e
  | _ ->
      failwith "Unexpected error in start_dd"

let wait t =
  with_lock t.m (fun () ->
      while !(t.finished) = false do
        Condition.wait t.c t.m
      done
  ) ;
  if !(t.cancelled) then raise Cancelled ;
  match !(t.exn) with Some exn -> raise exn | None -> ()

let cancel t =
  t.cancelled := true ;
  let pid = Forkhelpers.getpid t.pid in
  try Unix.kill pid Sys.sigkill with _ -> ()

(* This function will kill all sparse_dd pids that have been started by xapi *)
(* Only to be used on xapi restart *)
let killall () =
  let pids = State.list () in
  List.iter
    (fun pid ->
      try
        finally
          (fun () ->
            let exe = Unix.readlink (Printf.sprintf "/proc/%d/exe" pid) in
            debug "checking pid %d exe=%s globs=%s" pid exe
              !Xapi_globs.sparse_dd ;
            if Filename.basename exe = Filename.basename !Xapi_globs.sparse_dd
            then
              Unix.kill pid Sys.sigkill
            else
              ()
          )
          (fun () -> State.remove pid)
      with _ -> ()
    )
    pids
