(** Disks are attached to particular bus types: *)
type bus_type =
  | Xen  (** A xen paravirtualised bus *)
  | Scsi  (** A SCSI bus *)
  | Floppy  (** A floppy bus *)
  | Ide  (** An IDE bus *)

(** A valid device number *)
type t = private bus_type * int * int

val typ_of : t Rpc.Types.typ

val make : bus_type -> disk:int -> partition:int -> t option
(** [make bus ~disk ~partition] returns [Some device] when the parameters
    define a valid device number, or [None] otherwise. *)

val disk : t -> int
(** [disk t] returns the corresponding non-negative disk number *)

val bus : t -> bus_type
(** [bus t] returns the bus type of the device *)

val of_string : hvm:bool -> string -> t option
(** [of_string hvm name] returns the interface which best matches the [name] by
    applying the policy: first check if it is a disk_number, else fall back to a
    linux_device for backwards compatability *)

val to_debug_string : t -> string
(** [to_debug_string i] returns a pretty-printed interface *)

val to_linux_device : t -> string
(** [to_linux_device i] returns a possible linux string representation of
    interface [i] *)

val of_linux_device : string -> t option
(** [of_linux_device x] returns the interface corresponding to string [x] *)

val upgrade_linux_device : string -> string
(** [upgrade_linux_device x] upgrades hd* style device names to xvd* and leaves
    all other device names unchanged. *)

val to_xenstore_key : t -> int
(** [to_xenstore_key i] returns the xenstore key from interface [i] *)

val of_xenstore_key : int -> t
(** [of_xenstore_key key] returns an interface from a xenstore key *)

val of_disk_number : bool -> int -> t option
(** [of_disk_number hvm n] returns the interface corresponding to disk number
    [n] which depends on whether the guest is [hvm] or not. *)
