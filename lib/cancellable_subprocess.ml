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
open Printf
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = "xenops" end)
open D

open Forkhelpers
let run (task: Xenops_task.task_handle) ?env ?stdin fds ?(syslog_stdout=NoSyslogging) cmd args =
  let stdinandpipes = Opt.map (fun str ->
      let (x,y) = Unix.pipe () in
      (str,x,y)) stdin in
  (* Used so that cancel -> kills subprocess -> Unix.WSIGNALED -> raise cancelled *)
  let cancelled = ref false in
  finally (fun () -> 
      match with_logfile_fd "execute_command_get_out" (fun out_fd ->
          with_logfile_fd "execute_command_get_err" (fun err_fd ->
              let t = safe_close_and_exec ?env (Opt.map (fun (_,fd,_) -> fd) stdinandpipes) (Some out_fd) (Some err_fd) fds ~syslog_stdout cmd args in
              let done_waitpid = ref false in
              finally
                (fun () ->
                   let pid' = Forkhelpers.getpid t in
                   Xenops_task.with_cancel task
                     (fun () ->
                        cancelled := true;
                        info "Cancelling: sending SIGKILL to %d" pid';
                        try Unix.kill pid' Sys.sigkill with _ -> ()
                     )
                     (fun () ->
                        Opt.iter (fun (str,_,wr) -> Unixext.really_write wr (Bytes.of_string str) 0 (String.length str)) stdinandpipes;
                        done_waitpid := true;
                        snd (Forkhelpers.waitpid t)
                     )
                ) (fun () -> if not(!done_waitpid) then Forkhelpers.dontwaitpid t)
            )) with
      | Success(out,Success(err,(status))) -> 
        begin
          match status with
          | Unix.WEXITED 0 -> (out,err)
          | Unix.WEXITED n -> raise (Spawn_internal_error(err,out,Unix.WEXITED n))
          | Unix.WSTOPPED n -> raise (Spawn_internal_error(err,out,Unix.WSTOPPED n))
          | Unix.WSIGNALED n ->
            if !cancelled then begin
              debug "Subprocess %s exited with signal %d and cancel requested; raising Cancelled" cmd n;
              Xenops_task.raise_cancelled task
            end else begin
              debug "Subprocess %s exited with signal %d" cmd n;
              raise (Spawn_internal_error(err,out,Unix.WSIGNALED n))
            end
        end
      | Success(_,Failure(_,exn))
      | Failure(_, exn) ->
        raise exn)
    (fun () -> Opt.iter (fun (_,x,y) -> Unix.close x; Unix.close y) stdinandpipes)
