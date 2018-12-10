(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make(struct let name = "xenops_sandbox" end)
module Chroot : sig
  (* can access fields, but can only be created through of_domid and create *)
  type t = private {
    root: string;
    uid: int;
    gid: int;
  }
  module Path : sig
    type t
    val of_string : relative:string -> t
  end
  (** [absolute_path_outside chroot path] returns the absolute path outside the chroot *)
  val absolute_path_outside : t -> Path.t -> string

  (** [chroot_path_inside path] returns the path when inside the chroot *)
  val chroot_path_inside : Path.t -> string

  (** [of_domid daemon domid] describes a chroot for specified daemon and domain *)
  val of_domid : daemon:string -> domid:int -> t

  (** [create daemon domid paths] Creates the specified chroot with appropriate permissions,
   * and ensures that all [paths] are owned by the chrooted daemon and rw- *)
  val create : daemon:string -> domid:int -> Path.t list -> t

  (** [destroy chroot] Deletes the chroot *)
  val destroy: t -> unit
end = struct
  type t = {
    root: string;
    uid: int;
    gid: int;
  }

  module Path = struct
    type t = string
    let of_string ~relative =
      if not (Filename.is_implicit relative) then
        invalid_arg (Printf.sprintf "Expected implicit filename, but got '%s' (at %s)" relative __LOC__);
      relative
  end

  let absolute_path_outside chroot path =
    Filename.concat chroot.root path

  let chroot_path_inside path = Filename.concat "/" path

  let qemu_base_uid () = (Unix.getpwnam "qemu_base").Unix.pw_uid
  let qemu_base_gid () = (Unix.getpwnam "qemu_base").Unix.pw_gid

  let of_domid ~daemon ~domid =
    let root = Printf.sprintf "/var/run/xen/%s-root-%d" daemon domid in
    (* per VM uid/gid as for QEMU *)
    let uid = qemu_base_uid () + domid in
    let gid = qemu_base_gid () + domid in
    { root; uid; gid }

  let create ~daemon ~domid paths =
    let chroot = of_domid ~daemon ~domid in
    try
      Xenops_utils.Unixext.mkdir_rec chroot.root 0o755;
      (* we want parent dir to be 0o755 and this dir 0o750 *)
      Unix.chmod chroot.root 0o750;
      (* the chrooted daemon will have r-x permissions *)
      Unix.chown chroot.root 0 chroot.gid;
      D.debug "Created chroot %s" chroot.root;
      let prepare path =
        let fullpath = absolute_path_outside chroot path in
        Xenops_utils.Unixext.with_file fullpath [Unix.O_CREAT; Unix.O_EXCL] 0o600 (fun fd ->
            Unix.fchown fd chroot.uid chroot.gid)
      in
      List.iter prepare paths;
      chroot
    with e ->
      Backtrace.is_important e;
      D.warn "Failed to create chroot at %s for UID %d: %s" chroot.root chroot.uid
        (Printexc.to_string e);
      raise e

  let destroy chroot =
    Xenops_utils.best_effort (Printf.sprintf "removing chroot %s" chroot.root)
      (fun () -> Xenops_utils.FileFS.rmtree chroot.root)
end

module Varstore_guard = struct
  let daemon = "varstored"
  let varstored_chroot ~domid = Chroot.of_domid ~daemon ~domid
  let socket_path = Chroot.Path.of_string ~relative:"xapi-depriv-socket"

  (** [start dbg ~vm_uuid ~domid ~paths] prepares a chroot for [domid],
   * and asks varstore-guard to create a socket restricted to [vm_uuid].
   * Also creates empty files specified in [paths] owned by [domid] user.*)
  let start dbg ~vm_uuid ~domid ~paths =
    let chroot = Chroot.create ~daemon ~domid paths in
    let absolute_socket_path = Chroot.absolute_path_outside chroot socket_path in
    let vm_uuidm = match Uuidm.of_string vm_uuid with Some uuid -> uuid | None ->
     failwith (Printf.sprintf "Invalid VM uuid %s" vm_uuid) in
    Varstore_privileged_client.Client.create dbg vm_uuidm chroot.gid absolute_socket_path;
    chroot, Chroot.chroot_path_inside socket_path

  (** [prepare ~domid path] creates an empty [path] file owned by [domid] inside the chroot for [domid]
 * and returns the absolute path to it outside the chroot *)
  let prepare ~domid path =
    let chroot = Chroot.create ~daemon ~domid [path] in
    Chroot.absolute_path_outside chroot path

  let read ~domid path =
    let chroot = varstored_chroot ~domid in
    path |> Chroot.absolute_path_outside chroot |> Xenops_utils.Unixext.string_of_file

  let stop dbg ~domid =
      let chroot = varstored_chroot ~domid in
      if Sys.file_exists chroot.root then
        let gid = chroot.Chroot.gid in
        let absolute_socket_path = Chroot.absolute_path_outside chroot socket_path in
        Xenops_utils.best_effort "Stop listening on deprivileged socket" (fun () ->
            Varstore_privileged_client.Client.destroy dbg gid absolute_socket_path);
        Chroot.destroy chroot
end
