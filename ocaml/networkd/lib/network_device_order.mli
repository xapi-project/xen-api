(** Generete an order for host network devices and keep the order as stable as possible.
  *)

val generate : ?force:bool -> unit -> (string * string) list
(** Generate order, save it in memory, and return a list for device name changes (old_name, new_name).
    When the [force] is [true], use the initial rules. By default it is [false].
  *)

val order : unit -> (int * string option) list
(** Return the order recorded in memory. *)

val order_of_eths : unit -> (int * string option) list
(** Return the order recorded in the names like eth<N>.
    This is for backwards compatibility.
 *)

(* Below is exposed for unit tests *)

module PciAddr : sig
  type t = {segment: int; bus: int; dev: int; func: int}

  val to_string : t -> string

  val of_string_exn : string -> t

  val compare : t -> t -> int
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
  type t = {name: string; pci_addr: PciAddr.t; mac_addr: Macaddr.t}
end

module NetDevMapping : sig
  type index = Mac_addr of Macaddr.t | Pci_addr of PciAddr.t | Label of string

  type t = {position: int; index: index}
end

module UnOrderedNetDev : sig
  type t = {net_dev: NetDev.t; bios_eth_order: int}

  val get_all : unit -> t MacaddrMap.t
end

module OrderingNetDev : sig
  type t = {
      net_dev: NetDev.t
    ; position: int option
    ; bios_eth_order: int
    ; multinic: bool
  }
end

module OrderedNetDev : sig
  type t = {net_dev: NetDev.t; position: int}
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
