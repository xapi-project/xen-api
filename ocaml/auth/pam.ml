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

external authenticate : string -> string -> unit = "stub_XA_mh_authorize"

external change_password : string -> string -> unit = "stub_XA_mh_chpasswd"

type pam_handle

external authorize_start : unit -> pam_handle = "stub_XA_mh_authorize_start"

external authorize_stop : pam_handle -> unit = "stub_XA_mh_authorize_stop"

external authorize_run : pam_handle -> string -> string -> unit
  = "stub_XA_mh_authorize_run"

type t = {handle: pam_handle option Atomic.t; owner: Thread.t}

let authorize_start () =
  (* we can't easily attach a finalizer due to the requirement of running it in the same thread as start *)
  let pam_handle = authorize_start () in
  {handle= Atomic.make (Some pam_handle); owner= Thread.self ()}

let check_handle t =
  (* handle can only be used in same thread that created it *)
  assert (Thread.id (Thread.self ()) = Thread.id t.owner) ;
  match Atomic.get t.handle with
  | Some h ->
      h
  | None ->
      assert false (* use after free *)

let authorize_stop t =
  (* even if authorize_stop would fail we can only call it once, so mark it as freed now *)
  let h = check_handle t in
  Atomic.set t.handle None;
  authorize_stop h

let authorize_run t username password =
  let handle = check_handle t in
  authorize_run handle username password

external workaround : unit -> unit = "stub_XA_workaround"
