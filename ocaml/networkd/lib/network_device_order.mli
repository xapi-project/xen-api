(** Generete an order for host network devices and keep the order as stable as possible. *)

exception Not_ethN of string

module Pciaddr : sig
  (** Type of the PCI address *)
  type t = Xcp_pci.address

  val of_string_exn : string -> t
  (** [of_string_exn s] is the PCI address converted from [s]. Raises [Parse_error]
      if the [s] can't be accepted. *)

  (** [Parse_error s] is raised when [of_string_exn s] can't accept [s]. *)
  exception Parse_error of string
end

module Rule : sig
  type index =
    | Mac_addr of Macaddr.t
    | Pci_addr of Pciaddr.t
    | Label of string  (** Type of mapping *)

  (** Type of one mapping configuration. *)
  type t = {position: int; index: index}

  val read : path:string -> t list
  (** [read ~path] is the parsed rules from the content of file [path].
      The file [path] contains lines in the following format:
        <N>:<label|mac|pci>="<value>", where
        label: means the <value> is the name label of the device,
        mac: means the <value> is the MAC address of the device like 00:02:C9:ED:FD:F0,
        pci: means the <value> is the PCI address (in SBDF format) of the device locates at, like 0000:05:00.0. *)

  (** [Parse_error] is raised when a rule can't be created via [read]. *)
  exception Parse_error of string

  (** [Duplicate_position] is raised when duplicate position is specified in the rules. *)
  exception Duplicate_position

  val matches : mac:Macaddr.t -> pci:Pciaddr.t -> label:string -> t -> bool
  (** [true] if any of the [mac], [pci], or [label] meets the rule [t]. *)
end

module Dev : sig
  (** Type of an network deivce parsed from the output of biosdevname. *)
  type t = {
      name: Network_interface.iface
    ; mac: Network_interface.mac_address
    ; pci: Xcp_pci.address
    ; bios_eth_order: int
          (** The <N> in eth<N> which is the value of "BIOS device" from output of [biosdevname --policy all_ethN], is greater than or equal to 0 *)
    ; multi_nic: bool
          (** [true] if there are other devices locate at the same PCI address. Otherwise [false]. *)
  }

  (** [Missing_key k] is raised when no [k] can be found from the output of biosdevname. *)
  exception Missing_key of string

  (** [Duplicate_mac_addressk] is raised when duplicate MAC address is found from the output of biosdevname. *)
  exception Duplicate_mac_address

  val get_all : unit -> t list
  (** [get_all ()] is a map of network devices parsed from the output of biosdevname. The keys of the map are MAC addresses. *)
end

module OrderedDev : sig
  (** Type of an ordered network device. *)
  type t = Network_interface.ordered_iface

  val assert_no_duplicate_position : t list -> unit

  val assert_no_duplicate_mac : t list -> unit

  (** [Duplicate_position] is raised when duplicate position is found in calling [assert_no_duplicate_position l], where [l] is a list of [t]. *)
  exception Duplicate_position

  (** [Duplicate_mac_address] is raised when duplicate MAC address is found in calling [assert_no_duplicate_mac l], where [l] is a list of [t]. *)
  exception Duplicate_mac_address
end

val sort : OrderedDev.t list -> OrderedDev.t list * (string * string) list
(** [sort last_order] is to sort and generate an order based on [last_order], return the new order and a list for device name changes (old_name, new_name). *)

(* Below is exposed only for unit tests *)

val sort' :
     currents:Dev.t list
  -> rules:Rule.t list
  -> last_order:OrderedDev.t list
  -> OrderedDev.t list
