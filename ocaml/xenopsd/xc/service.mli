exception Service_failed of (string * string)

type t = {
    name: string
  ; domid: Xenctrl.domid
  ; exec_path: string
  ; chroot: Xenops_sandbox.Chroot.t
  ; timeout_seconds: float
  ; args: string list
  ; execute:
      path:string -> args:string list -> domid:Xenctrl.domid -> unit -> string
}

val start_and_wait_for_readyness :
  task:Xenops_task.Xenops_task.task_handle -> service:t -> unit
