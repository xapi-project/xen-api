exception Service_failed of (string * string)

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
  val pidfile_path : Xenctrl.domid -> string option
  (** path of file containing the pid value *)

  val pid_path : Xenctrl.domid -> string
  (** xenstore key containing the pid value *)

  val start_daemon :
    path:string -> args:string list -> domid:Xenctrl.domid -> unit -> string

  val alive : string -> 'a -> bool

  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
end

module Swtpm : sig
  val start :
       xs:Xenstore.Xs.xsh
    -> vtpm_uuid:Varstore_privileged_interface.Uuidm.t
    -> index:int
    -> Xenops_task.Xenops_task.task_handle
    -> Xenctrl.domid
    -> string

  val restore : domid:int -> vm_uuid:string -> string -> unit

  val suspend : xs:Xenstore.Xs.xsh -> domid:int -> vm_uuid:string -> string

  val stop :
       string
    -> xs:Xenstore.Xs.xsh
    -> domid:int
    -> vm_uuid:string
    -> vtpm_uuid:Varstore_privileged_interface.Uuidm.t
    -> unit
end
