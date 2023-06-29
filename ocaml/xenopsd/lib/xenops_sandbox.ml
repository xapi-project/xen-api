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

module D = Debug.Make (struct let name = "xenops_sandbox" end)

module Chroot : sig
  (* can access fields, but can only be created through of_domid and create *)
  type t = private {root: string; uid: int; gid: int}

  module Path : sig
    type t

    val root : t

    val of_string : relative:string -> t

    val concat : t -> string -> t
  end

  val absolute_path_outside : t -> Path.t -> string

  val chroot_path_inside : Path.t -> string

  val create_dir : within:t -> int -> Path.t -> unit

  val of_domid :
       base:string
    -> base_uid:(unit -> int)
    -> base_gid:(unit -> int)
    -> daemon:string
    -> domid:int
    -> vm_uuid:string
    -> t
  (** [of_domid ~base ~base_uid ~base_gid ~daemon ~domid ~vm_uuid] describes
      a chroot for specified daemon and domain, with the correct permissions *)

  val create : t -> Path.t list -> unit
  (** [create chroot paths] Creates the specified chroot with appropriate
      permissions, and ensures that all [paths] are owned by the chrooted
      daemon and rw- *)

  val destroy : t -> unit
  (** [destroy chroot] Deletes the chroot *)
end = struct
  type t = {root: string; uid: int; gid: int}

  module Path = struct
    type t = string

    let root = "/"

    let of_string ~relative =
      if not (Filename.is_implicit relative) then
        invalid_arg
          (Printf.sprintf "Expected implicit filename, but got '%s' (at %s)"
             relative __LOC__
          ) ;
      relative

    let concat = Filename.concat
  end

  let absolute_path_outside chroot path = Filename.concat chroot.root path

  let chroot_path_inside path = Path.(concat root path)

  let create_dir ~within perm path =
    let fullpath = absolute_path_outside within path in
    Xenops_utils.Unixext.with_file fullpath [Unix.O_CREAT; Unix.O_EXCL] perm
      (fun fd -> Unix.fchown fd within.uid within.gid
    )

  let of_domid ~base ~base_uid ~base_gid ~daemon ~domid ~vm_uuid =
    let root =
      let dir =
        if domid = 0 then
          Printf.sprintf "%s-root-%d-%s" daemon domid vm_uuid
        else
          Printf.sprintf "%s-root-%d" daemon domid
      in
      Filename.concat base dir
    in
    (* per VM uid/gid as for QEMU *)
    let uid = base_uid () + domid in
    let gid = base_gid () + domid in
    {root; uid; gid}

  let create chroot paths =
    try
      Xenops_utils.Unixext.mkdir_rec chroot.root 0o755 ;
      (* we want parent dir to be 0o755 and this dir 0o750 *)
      Unix.chmod chroot.root 0o750 ;
      (* the chrooted daemon will have r-x permissions *)
      Unix.chown chroot.root 0 chroot.gid ;
      D.debug "Created chroot %s" chroot.root ;
      List.iter (create_dir ~within:chroot 0o600) paths
    with e ->
      Backtrace.is_important e ;
      D.warn "Failed to create chroot at %s for UID %d: %s" chroot.root
        chroot.uid (Printexc.to_string e) ;
      raise e

  let destroy chroot =
    Xenops_utils.best_effort (Printf.sprintf "removing chroot %s" chroot.root)
      (fun () -> Xenops_utils.FileFS.rmtree chroot.root
    )
end

module type SANDBOX = sig
  val create : domid:int -> vm_uuid:string -> Chroot.Path.t -> string

  val start :
       string
    -> vm_uuid:string
    -> domid:int
    -> paths:Chroot.Path.t list
    -> Chroot.t * string

  val read : domid:int -> Chroot.Path.t -> vm_uuid:string -> string

  val stop : string -> domid:int -> vm_uuid:string -> unit
end

module type GUARD = sig
  val daemon_name : string

  val base_directory : string

  val base_uid : unit -> int

  val base_gid : unit -> int

  val create : string -> vm_uuid:Uuidm.t -> gid:int -> path:string -> unit

  val destroy : string -> gid:int -> path:string -> unit
end

module Guard (G : GUARD) : SANDBOX = struct
  let daemon = G.daemon_name

  let socket_path = Chroot.Path.of_string ~relative:"xapi-depriv-socket"

  let chroot ~domid ~vm_uuid =
    Chroot.of_domid ~base:G.base_directory ~base_uid:G.base_uid
      ~base_gid:G.base_gid ~daemon ~domid ~vm_uuid

  let start dbg ~vm_uuid ~domid ~paths =
    let chroot = chroot ~domid ~vm_uuid in
    Chroot.create chroot paths ;
    let absolute_socket_path =
      Chroot.absolute_path_outside chroot socket_path
    in
    let vm_uuid =
      match Uuidm.of_string vm_uuid with
      | Some uuid ->
          uuid
      | None ->
          failwith (Printf.sprintf "Invalid VM uuid %s" vm_uuid)
    in
    G.create dbg ~vm_uuid ~gid:chroot.gid ~path:absolute_socket_path ;
    (chroot, Chroot.chroot_path_inside socket_path)

  let create ~domid ~vm_uuid path =
    let chroot = chroot ~domid ~vm_uuid in
    Chroot.create chroot [path] ;
    Chroot.absolute_path_outside chroot path

  let read ~domid path ~vm_uuid =
    let chroot = chroot ~domid ~vm_uuid in
    path
    |> Chroot.absolute_path_outside chroot
    |> Xenops_utils.Unixext.string_of_file

  let stop dbg ~domid ~vm_uuid =
    let chroot = chroot ~domid ~vm_uuid in
    if Sys.file_exists chroot.root then (
      D.debug "About to stop %s for %d (%s) %s" daemon domid vm_uuid __LOC__ ;
      let gid = chroot.Chroot.gid in
      let absolute_socket_path =
        Chroot.absolute_path_outside chroot socket_path
      in
      Xenops_utils.best_effort "Stop listening on deprivileged socket"
        (fun () -> G.destroy dbg ~gid ~path:absolute_socket_path
      ) ;
      Chroot.destroy chroot
    ) else
      D.warn "Can't stop %s for %d (%s): %s does not exist" daemon domid vm_uuid
        chroot.root
end

module Varstored : GUARD = struct
  let daemon_name = "varstored"

  let base_directory = "/var/run/xen"

  let base_uid () = (Unix.getpwnam "qemu_base").Unix.pw_uid

  let base_gid () = (Unix.getpwnam "qemu_base").Unix.pw_gid

  let create dbg ~vm_uuid ~gid ~path =
    Xapi_idl_guard_privileged.Client.varstore_create dbg vm_uuid gid path

  let destroy dbg ~gid ~path =
    Xapi_idl_guard_privileged.Client.varstore_destroy dbg gid path
end

module Swtpm : GUARD = struct
  let daemon_name = "swtpm"

  (* swtpm cannot run on /var/run because it's mounted using nodev and access
     is needed to /dev/urandom *)
  let base_directory = "/var/lib/xcp/run"

  let base_uid () = (Unix.getpwnam "swtpm_base").Unix.pw_uid

  (* swtpm runas only supports a uid, so use uid = gid *)
  let base_gid () = base_uid ()

  let create dbg ~vm_uuid ~gid ~path =
    Xapi_idl_guard_privileged.Client.vtpm_create dbg vm_uuid gid path

  let destroy dbg ~gid ~path =
    Xapi_idl_guard_privileged.Client.vtpm_destroy dbg gid path
end

module Varstore_guard = Guard (Varstored)
module Swtpm_guard = Guard (Swtpm)
