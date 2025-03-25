(** Generete an order for host network devices and keep the order as stable as possible. *)

val generate : ?force:bool -> unit -> (string * string) list
(** [generate force ()] is to generate order, save it in memory and files, and return a list for device name changes (old_name, new_name).
    When the [force] is [true], use the initial rules. By default it is [false]. *)

val order : unit -> (int * string option) list
(** [order ()] is the order recorded in memory and files. *)

val order_of_eths : unit -> (int * string option) list
(** [order_of_eths ()] is the order recorded in the names like eth<N>.
    This is for backwards compatibility. *)

exception Not_ethN of string

module PciAddr : sig
  (** Type of the PCI address *)
  type t = {segment: int; bus: int; dev: int; func: int}

  val of_string_exn : string -> t
  (** [of_string_exn s] is the PCI address converted from [s]. Raises [Parse_error]
      if the [s] can't be accepted. *)

  (** [Parse_error s] is raised when [of_string_exn s] can't accept [s]. *)
  exception Parse_error of string
end

module NetDevMapping : sig
  type index =
    | Mac_addr of Macaddr.t
    | Pci_addr of PciAddr.t
    | Label of string  (** Type of mapping *)

  (** Type of one mapping configuration. *)
  type t = {position: int; index: index}

  val mappings_of_file : path:string -> t list
  (** [mappings_of_file ~path] is the parsed mappings from the content of file [path].
      The file [path] contains lines in the following format:
        <N>:<label|mac|pci>:"<value>", where
        label: means the <value> is the name label of the device,
        mac: means the <value> is the MAC address of the device like 00:02:C9:ED:FD:F0,
        pci: means the <value> is the PCI address (in SBDF format) of the device locates at, like 0000:05:00.0. *)

  (** [Unsupported_type t] is raised when an unsupported type [t] is met in parsing the mapping file. *)
  exception Unsupported_type of string

  (** [Duplicate_position] is raised when duplicate position is specified in the mappings. *)
  exception Duplicate_position
end

module MacaddrMap : sig
  type key = Macaddr.t

  type 'a t = 'a Stdlib__Map.Make(Macaddr).t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module NetDev : sig
  (** Type of a network device. *)
  type t = {name: string; pci_addr: PciAddr.t; mac_addr: Macaddr.t}
end

module UnOrderedNetDev : sig
  (** Type of an network deivce parsed from the output of biosdevname. *)
  type t = {net_dev: NetDev.t; bios_eth_order: int}

  (** [Missing_key k] is raised when no [k] can be found from the output of biosdevname. *)
  exception Missing_key of string

  (** [Duplicate_mac_addressk] is raised when duplicate MAC address is found from the output of biosdevname. *)
  exception Duplicate_mac_address

  val get_all : unit -> t MacaddrMap.t
  (** [get_all ()] is a map of network devices parsed from the output of biosdevname. The keys of the map are MAC addresses. *)
end

module OrderedNetDev : sig
  (** Type of an ordered network device. *)
  type t = {net_dev: NetDev.t; position: int}

  (** [Parse_error msg] is raised when can't parse ordered network devices from files. *)
  exception Parse_error of string

  (** [Duplicate_position] is raised when duplicate position is found in an ordered list. *)
  exception Duplicate_position

  val order_of_file : path:string -> t MacaddrMap.t
  (** [order_of_file ~path] is a map of network devices parsed from the content of file [path]. The keys of the map ar MAC addresses. *)
end

(* Below is exposed only for unit tests *)

module OrderingNetDev : sig
  type t = {
      net_dev: NetDev.t
    ; position: int option
    ; bios_eth_order: int
    ; multinic: bool
  }
end

module ListToMacaddrMap : sig
  val to_11_map : by:('a -> MacaddrMap.key) -> 'a list -> 'a MacaddrMap.t
end

val generate_order :
     currents:UnOrderedNetDev.t MacaddrMap.t
  -> mappings:NetDevMapping.t list
  -> lasts:OrderedNetDev.t MacaddrMap.t
  -> olds:OrderedNetDev.t MacaddrMap.t
  -> int * OrderedNetDev.t list * OrderedNetDev.t list
