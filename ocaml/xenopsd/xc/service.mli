(* Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Qemu : sig
  module SignalMask : sig
    type t

    val create : unit -> t

    val set : t -> int -> unit

    val unset : t -> int -> unit

    val has : t -> int -> bool
  end

  val signal_mask : SignalMask.t

  val name : string

  val pid_path_signal : Xenctrl.domid -> string

  val pidfile_path : Xenctrl.domid -> string option
  (** path of file containing the pid value *)

  val pid_path : Xenctrl.domid -> string
  (** xenstore key containing the pid value *)

  val start_daemon :
       path:string
    -> args:string list
    -> domid:Xenctrl.domid
    -> ?fds:(string * Unix.file_descr) list
    -> unit
    -> Forkhelpers.pidty

  val pid : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option

  val is_running : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> bool
end

module Vgpu : sig
  val start_daemon :
       path:string
    -> args:string list
    -> domid:Xenctrl.domid
    -> ?fds:(string * Unix.file_descr) list
    -> unit
    -> Forkhelpers.pidty

  val pid : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option

  val is_running : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> bool

  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit

  val wait_path :
       pidalive:(string -> bool)
    -> task:Xenops_task.Xenops_task.task_handle
    -> name:string
    -> domid:int
    -> xs:Xenstore.Xs.xsh
    -> ready_path:Watch.path
    -> timeout:float
    -> cancel:Cancel_utils.key
    -> 'a
    -> unit
end

module PV_Vnc : sig
  exception Failed_to_start

  val vnc_port_path : Xenctrl.domid -> string

  val tc_port_path : Xenctrl.domid -> string

  val save : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit

  val get_statefile : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> string option

  val start :
       ?statefile:string
    -> xs:Xenstore.Xs.xsh
    -> ?ip:string
    -> Xenctrl.domid
    -> unit

  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit

  val get_vnc_port :
    xs:Xenstore.Xs.xsh -> Xenctrl.domid -> Xenops_utils.Socket.t option

  val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
end

module Varstored : sig
  val efivars_save_path : Xenops_sandbox.Chroot.Path.t

  val efivars_resume_path : Xenops_sandbox.Chroot.Path.t

  val start :
       xs:Xenstore.Xs.xsh
    -> nvram:Xenops_types.Nvram_uefi_variables.t
    -> ?restore:bool
    -> Xenops_task.Xenops_task.task_handle
    -> Xenctrl.domid
    -> unit

  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
end
