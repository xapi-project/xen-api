(** A wrapper library around bindings to [libpci].

    [Pci] exposes a subset of the functionality of [libpci] as part of
    "{{:http://mj.ucw.cz/sw/pciutils/}The PCI Utilities}" package shipped with
    most operating systems.

    Rather than being a completely transparent set of bindings, this library
    exposes an API to wrap some of the composite functions to ensure correct
    memory allocation and cleanup. This should reduce memory leaks and
    segfaults which are possible with incorrect use of [libpci]. *)

module Pci_dev : sig
  type t = {
    domain : int;
    bus : int;
    dev : int;
    func : int;
    vendor_id : int;
    device_id : int;
    device_class : int;
    irq : int;
    base_addr : nativeint list;
    size : nativeint list;
    rom_base_addr : nativeint;
    rom_size : nativeint;
    phy_slot : string option;
    subsystem_id : (int * int) option;
  }
end

module Pci_access : sig
  type t
end

val lookup_class_name : Pci_access.t -> int -> string option
(** [lookup_class_name a id] wraps pci_lookup_name with the right flags to
    lookup the name for the class whose identifier is [id] using the access
    value [a]. If [libpci] cannot find a match it returns "Class [id]". *)

val lookup_progif_name : Pci_access.t -> int -> int -> string option
(** [lookup_progif_name a c_id id] is like {!lookup_class_name} but returns
    the name of the programming interface with ID [id] within the class with ID
    [c_id]. *)

val lookup_vendor_name : Pci_access.t -> int -> string option
(** [lookup_vendor_name a id] is like {!lookup_class_name} but returns
    the name of the PCI vendor with ID [id]. *)

val lookup_device_name : Pci_access.t -> int -> int -> string option
(** [lookup_device_name a v_id id] is like {!lookup_class_name} but returns
    the name of the device with ID [id] by the vendor with ID [v_id]. *)

val lookup_subsystem_vendor_name : Pci_access.t -> int -> string option
(** [lookup_subsystem_vendor_name a id] is like {!lookup_class_name} but
    returns the name of the PCI vendor with ID [id]. *)

val lookup_subsystem_device_name : Pci_access.t -> int -> int -> int -> int -> string option
(** [lookup_subsystem_device_name a v_id d_id sv_id sd_id] is like
    {!lookup_class_name} but returns the name of the PCI subsystem of a device
    with ID [d_id] made by vendor with ID [v_id] whose subvendor and subdevice
    IDs are [sv_id] and [sd_id] respectively. *)

val with_access : ?cleanup:bool -> ?from_dump:string -> (Pci_access.t -> 'a) -> 'a
(** [with_access ~cleanup f] wraps the [libpci] calls to [pci_alloc],
    [pci_init] and [pci_cleanup] and constructs [(access:Pci_access.t)] and
    returns [f access]. If [cleanup] is [true] (default), [pci_cleanup] is
    called to ensure the allocated [access] is not leaked and in the case where
    [f a] raises an exception [exn], [pci_cleanup] is still called and [exn] is
    re-raised. *)

val get_devices : Pci_access.t -> Pci_dev.t list
(** [get_devices a] returns a list of devices found on the PCI bus for access
    value [a]. It wraps the [libpci] call to [pci_scan_bus] and calls
    [pci_fill_info] for each of the devices on the bus before returning a list
    of [Pci_dev.t]. *)
