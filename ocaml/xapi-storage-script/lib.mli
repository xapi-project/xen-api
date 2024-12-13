(* Copyright (C) Cloud Software Group Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Types : sig
  type backtrace = {error: string; files: string list; lines: int list}

  val rpc_of_backtrace : backtrace -> Rpc.t

  val backtrace_of_rpc : Rpc.t -> backtrace

  type error = {code: string; params: string list; backtrace: backtrace}

  val rpc_of_error : error -> Rpc.t

  val error_of_rpc : Rpc.t -> error
end

module Sys : sig
  type file = Regular | Directory | Other | Missing | Unknown

  val file_kind : follow_symlinks:bool -> string -> file Lwt.t

  val access :
       string
    -> Unix.access_permission list
    -> (unit, [> `not_executable of string * exn]) result Lwt.t

  val assert_is_executable :
       string
    -> (unit, [> `missing of string | `not_executable of string * exn]) result
       Lwt.t
  (** [assert_is_executable path] returns [Ok ()] when [path] is an executable
      regular file, [Error `not_executable] when the file is a non-executable
      regular file, and [Error `missing] otherwise. The [Errors] return the
      queried path as a string. *)

  val read_file_contents : string -> string Lwt.t

  val save : contents:string -> string -> unit Lwt.t

  val readdir : string -> string list Lwt.t

  val mkdir_p : ?perm:int -> string -> unit Lwt.t
end

module Signal : sig
  type t

  val to_string : t -> string
end

module Process : sig
  module Output : sig
    type exit_or_signal = Exit_non_zero of int | Signal of Signal.t

    type t = {
        exit_status: (unit, exit_or_signal) result
      ; pid: int
      ; stdout: string
      ; stderr: string
    }
  end

  val run :
       env:(string * string) list
    -> prog:string
    -> args:string list
    -> input:string
    -> Output.t Lwt.t
  (** Runs a cli program, writes [input] into its stdin, then closing the fd,
      and finally waits for the program to finish and returns the exit status,
      the pid, and its stdout and stderr. *)
end

module DirWatcher : sig
  type event =
    | Modified of string  (** File contents changed *)
    | Changed  (** Something in the directory changed, read anew *)

  val create :
    string -> ((Inotify.watch, string) Hashtbl.t * Lwt_inotify.t) Lwt.t

  val read :
    (Inotify.watch, string) Hashtbl.t * Lwt_inotify.t -> event list Lwt.t
end

module Clock : sig
  val after : seconds:float -> unit Lwt.t
end
