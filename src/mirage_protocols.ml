module Ethif = struct
  type error = Mirage_device.error
  let pp_error = Mirage_device.pp_error
end

module Ip = struct
  type error = [
    | `No_route of string (** can't send a message to that destination *)
  ]
  let pp_error ppf = function
  | `No_route s -> Fmt.pf ppf "no route to destination: %s" s
end

module Arp = struct
  type error = [
    | `Timeout (** Failed to establish a mapping between an IP and a link-level address *)
  ]
  let pp_error ppf = function
  | `Timeout -> Fmt.pf ppf "could not determine a link-level address for the IP address given"
end

module Tcp = struct
  type error = [ `Timeout | `Refused]
  type write_error = [ error | Mirage_flow.write_error]

  let pp_error ppf = function
  | `Timeout -> Fmt.string ppf "connection attempt timed out"
  | `Refused -> Fmt.string ppf "connection attempt was refused"

  let pp_write_error ppf = function
  | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  | #error as e                   -> pp_error ppf e
end

module type ETHIF = sig
  type error = private [> Ethif.error]
  val pp_error: error Fmt.t
  type buffer
  type netif
  type macaddr
  include Mirage_device.S
  val write: t -> buffer -> (unit, error) result io
  val writev: t -> buffer list -> (unit, error) result io
  val mac: t -> macaddr
  val mtu: t -> int
  val input:
    arpv4:(buffer -> unit io) ->
    ipv4:(buffer -> unit io) ->
    ipv6:(buffer -> unit io) ->
    t -> buffer -> unit io
end
module type IP = sig
  type error = private [> Ip.error]
  val pp_error: error Fmt.t
  type buffer
  type ethif
  type ipaddr
  type prefix
  include Mirage_device.S
  type callback = src:ipaddr -> dst:ipaddr -> buffer -> unit io
  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    buffer -> unit io
  val allocate_frame: t -> dst:ipaddr -> proto:[`ICMP | `TCP | `UDP] -> buffer * int
  val write: t -> buffer -> buffer -> (unit, error) result io
  val writev: t -> buffer -> buffer list -> (unit, error) result io
  val checksum: buffer -> buffer list -> int
  val pseudoheader : t -> dst:ipaddr -> proto:[< `TCP | `UDP ] -> int -> buffer
  val src: t -> dst:ipaddr -> ipaddr
  val set_ip: t -> ipaddr -> unit io
  val get_ip: t -> ipaddr list
  type uipaddr
  val to_uipaddr: ipaddr -> uipaddr
  val of_uipaddr: uipaddr -> ipaddr option
  val mtu: t -> int
end

module type ARP = sig
  include Mirage_device.S
  type ipaddr
  type buffer
  type macaddr
  type repr
  type error = private [> Arp.error]
  val pp_error: error Fmt.t
  val to_repr : t -> repr io
  val pp : repr Fmt.t
  val get_ips : t -> ipaddr list
  val set_ips : t -> ipaddr list -> unit io
  val remove_ip : t -> ipaddr -> unit io
  val add_ip : t -> ipaddr -> unit io
  val query : t -> ipaddr -> (macaddr, error) result io
  val input : t -> buffer -> unit io
end

module type IPV4 = IP
module type IPV6 = IP

module type ICMP = sig
  include Mirage_device.S
  type ipaddr
  type buffer
  type error
  val pp_error: error Fmt.t
  val input : t -> src:ipaddr -> dst:ipaddr -> buffer -> unit io
  val write : t -> dst:ipaddr -> buffer -> (unit, error) result io
end

module type ICMPV4 = ICMP

module type UDP = sig
  type error
  val pp_error: error Fmt.t
  type buffer
  type ip
  type ipaddr
  type ipinput
  include Mirage_device.S
  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> buffer -> unit io
  val input: listeners:(dst_port:int -> callback option) -> t -> ipinput
  val write: ?src_port:int -> dst:ipaddr -> dst_port:int -> t -> buffer ->
    (unit, error) result io
end

module Keepalive = struct
  type t = {
    after: Duration.t;
    interval: Duration.t;
    probes: int;
  }
end

module type TCP = sig
  type error = private [> Tcp.error]
  type write_error = private [> Tcp.write_error]
  type buffer
  type ip
  type ipaddr
  type ipinput
  type flow
  include Mirage_device.S
  include Mirage_flow.S with
      type 'a io  := 'a io
  and type buffer := buffer
  and type flow   := flow
  and type error  := error
  and type write_error := write_error

  val dst: flow -> ipaddr * int
  val write_nodelay: flow -> buffer -> (unit, write_error) result io
  val writev_nodelay: flow -> buffer list -> (unit, write_error) result io
  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> (flow, error) result io
  type listener = {
    process: flow -> unit io;
    keepalive: Keepalive.t option;
  }
  val input: t -> listeners:(int -> listener option) -> ipinput
end
