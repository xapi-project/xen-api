module type ETHIF = Mirage_protocols.ETHIF
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t
   and type macaddr = Macaddr.t

module type ARP = Mirage_protocols.ARP
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t
   and type ipaddr = Ipaddr.V4.t
   and type macaddr = Macaddr.t

module type IP = Mirage_protocols.IP
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t
   and type uipaddr = Ipaddr.t

module type IPV4 = IP
  with type ipaddr = Ipaddr.V4.t
   and type prefix = Ipaddr.V4.Prefix.t

(** IPv6 stack *)
module type IPV6 = IP
  with type ipaddr = Ipaddr.V6.t
   and type prefix = Ipaddr.V6.Prefix.t

(** ICMP module *)
module type ICMP = Mirage_protocols.ICMP
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t

(** ICMPV4 module *)
module type ICMPV4 = ICMP
  with type ipaddr = Ipaddr.V4.t

(** UDP stack *)
module type UDP = Mirage_protocols.UDP
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t

(** UDP stack over IPv4 *)
module type UDPV4 = UDP
  with type ipaddr = Ipaddr.V4.t

(** UDP stack over IPv6 *)
module type UDPV6 = UDP
  with type ipaddr = Ipaddr.V6.t

(** TCP stack *)
module type TCP = Mirage_protocols.TCP
  with type 'a io = 'a Lwt.t
   and type buffer = Cstruct.t

(** TCP module over IPv4 *)
module type TCPV4 = TCP
  with type ipaddr = Ipaddr.V4.t

(** TCP module over IPv6 *)
module type TCPV6 = TCP
  with type ipaddr = Ipaddr.V6.t

(** Configuration *)

type ipv4_config = {
  address : Ipaddr.V4.t;
  network : Ipaddr.V4.Prefix.t;
  gateway : Ipaddr.V4.t option;
}

type ipv6_config = {
  address : Ipaddr.V6.t list;
  netmasks : Ipaddr.V6.Prefix.t list;
  gateways : Ipaddr.V6.t list;
}

(** {1 DHCP client}
 *  A client which engages in lease transactions. *)
module type DHCP_CLIENT = sig
  type t = ipv4_config Lwt_stream.t
end

