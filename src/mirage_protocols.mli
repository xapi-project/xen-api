(** MirageOS signatures for network protocols

        {e %%VERSION%% } *)

(** {1 Ethernet stack}

    An Ethernet stack that parses frames from a network device and
    can associate them with IP address via ARP. *)

module Ethif : sig
  type error = Mirage_device.error
  val pp_error : error Fmt.t
end

module type ETHIF = sig

  type error = private [> Ethif.error]
  (** The type for ethernet interface errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type buffer
  (** The type for memory buffers. *)

  type netif
  (** The type for ethernet network interfaces. *)

  type macaddr
  (** The type for unique MAC identifiers. *)

  include Mirage_device.S

  val write: t -> buffer -> (unit, error) result io
  (** [write nf buf] outputs [buf] to netfront [nf]. *)

  val writev: t -> buffer list -> (unit, error) result io
  (** [writev nf bufs] output a list of buffers to netfront [nf] as a
      single packet. *)

  val mac: t -> macaddr
  (** [mac nf] is the MAC address of [nf]. *)

  val mtu: t -> int
  (** [mtu nf] is the Maximum Transmission Unit of the [nf] i.e. the maximum
      size of the payload, not including the ethernet frame header. *)

  val input:
    arpv4:(buffer -> unit io) ->
    ipv4:(buffer -> unit io) ->
    ipv6:(buffer -> unit io) ->
    t -> buffer -> unit io
  (** [listen nf fn] is a blocking operation that calls [fn buf]
      with every packet that is read from the interface.
      The function can be stopped by calling [disconnect]
      in the device layer. *)
end

(** {1 IP stack} *)

(** IP errors. *)
module Ip : sig

  type error = [
    | `No_route of string (** can't send a message to that destination *)
  ]

  val pp_error : error Fmt.t
end

(** An IP stack that parses Ethernet frames into IP packets *)
module type IP = sig

  type error = private [> Ip.error]
  (** The type for IP errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type buffer
    (** The type for memory buffers. *)

  type ethif
  (** The type for ethernet devices. *)

  type ipaddr
  (** The type for IP addresses. *)

  type prefix
  (** The type for IP prefixes. *)

  include Mirage_device.S

  type callback = src:ipaddr -> dst:ipaddr -> buffer -> unit io
  (** An input continuation used by the parsing functions to pass on
      an input packet down the stack.

      [callback ~src ~dst buf] will be called with [src] and [dst]
      containing the source and destination IP address respectively,
      and [buf] will be a buffer pointing at the start of the IP
      payload. *)

  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    buffer -> unit io
  (** [input ~tcp ~udp ~default ip buf] demultiplexes an incoming
      [buffer] that contains an IP frame. It examines the protocol
      header and passes the result onto either the [tcp] or [udp]
      function, or the [default] function for unknown IP protocols. *)

  val allocate_frame: t -> dst:ipaddr -> proto:[`ICMP | `TCP | `UDP] -> buffer * int
  (** [allocate_frame t ~dst ~proto] returns a pair [(pkt, len)] such that
      [Cstruct.sub pkt 0 len] is the IP header (including the link layer part) of a
      packet going to [dst] for protocol [proto].  The space in [pkt] after the
      first [len] bytes can be used by the client. *)

  val write: t -> buffer -> buffer -> (unit, error) result io
  (** [write t frame buf] writes the packet [frame :: buf :: []] to
      the address [dst]. *)

  val writev: t -> buffer -> buffer list -> (unit, error) result io
  (** [writev t frame bufs] writes the packet [frame :: bufs]. *)

  val checksum: buffer -> buffer list -> int
  (** [checksum frame bufs] computes the IP checksum of [bufs]
      computing the pseudo-header from the actual header [frame].  It
      assumes that frame is of the form returned by [allocate_frame],
      i.e., that it contains the link-layer part. *)

  val pseudoheader : t -> dst:ipaddr -> proto:[< `TCP | `UDP ] -> int -> buffer
  (** [pseudoheader t dst proto len] gives a pseudoheader suitable for use in
      TCP or UDP checksum calculation based on [t]. *)

  val src: t -> dst:ipaddr -> ipaddr
  (** [src ip ~dst] is the source address to be used to send a
      packet to [dst].  In the case of IPv4, this will always return
      the same IP, which is the only one set. *)

  val set_ip: t -> ipaddr -> unit io
  (** Set the IP address associated with this interface.  For IPv4,
      currently only supports a single IPv4 address, and aliases will
      be added in a future revision. *)

  val get_ip: t -> ipaddr list
  (** Get the IP addresses associated with this interface. For IPv4, only
   *  one IP address can be set at a time, so the list will always be of
   *  length 1 (and may be the default value, 0.0.0.0). *)

  type uipaddr
  (** The type for universal IP addresses. It supports all the
      possible versions. *)

  val to_uipaddr: ipaddr -> uipaddr
  (** Convert an IP address with a specific version (eg. V4) into a
      universal IP address. *)

  val of_uipaddr: uipaddr -> ipaddr option
  (** Project a universal IP address into the version supported by the
      current implementation. Return [None] if there is a version
      mismatch. *)

  val mtu: t -> int
  (** [mtu ip] is the Maximum Transmission Unit of the [ip] i.e. the maximum
      size of the payload, not including the IP header. *)

end

(** {1 ARP} *)

(** Arp error. *)
module Arp : sig
  type error = [ `Timeout ]
  val pp_error : error Fmt.t
end

module type ARP = sig
  include Mirage_device.S

  type ipaddr
  type buffer
  type macaddr
  type repr

  type error = private [> Arp.error]
  (** The type for ARP errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  (** Prettyprint cache contents *)
  val to_repr : t -> repr io
  val pp : repr Fmt.t

  (** [get_ips arp] gets the bound IP address list in the [arp]
      value. *)
  val get_ips : t -> ipaddr list

  (** [set_ips arp] sets the bound IP address list, which will transmit a
      GARP packet also. *)
  val set_ips : t -> ipaddr list -> unit io

  (** [remove_ip arp ip] removes [ip] to the bound IP address list in
      the [arp] value, which will transmit a GARP packet for any remaining IPs in
      the bound IP address list after the removal. *)
  val remove_ip : t -> ipaddr -> unit io

  (** [add_ip arp ip] adds [ip] to the bound IP address list in the
      [arp] value, which will transmit a GARP packet also. *)
  val add_ip : t -> ipaddr -> unit io

  (** [query arp ip] queries the cache in [arp] for an ARP entry
      corresponding to [ip], which may result in the sender sleeping
      waiting for a response. *)
  val query : t -> ipaddr -> (macaddr, error) result io

  (** [input arp frame] will handle an ARP frame. If it is a response,
      it will update its cache, otherwise will try to satisfy the
      request. *)
  val input : t -> buffer -> unit io
end

(** {1 IPv4 stack} *)
module type IPV4 = sig
  include IP
end

(** {1 IPv6 stack} *)
module type IPV6 = sig
  include IP
end

(** No Icmp module, as there are no exposed error polymorphic variants *)

(** {1 ICMP module} *)
module type ICMP = sig
  include Mirage_device.S

  type ipaddr
  (** The type for IP addresses. *)

  type buffer
  (** The type for buffers. *)

  type error (* entirely abstract since we expose none in an Icmp module *)
  (** The type for ICMP errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  val input : t -> src:ipaddr -> dst:ipaddr -> buffer -> unit io
  (** [input t src dst buffer] reacts to the ICMP message in
      [buffer]. *)

  val write : t -> dst:ipaddr -> buffer -> (unit, error) result io
  (** [write t dst buffer] sends the ICMP message in [buffer] to [dst]
      over IP. *)
end

module type ICMPV4 = sig
  include ICMP
end

(** {1 UDP stack} *)

(** No Udp module, as there are no exposed error polymorphic variants *)

(*    A UDP stack that can send and receive datagrams. *)
module type UDP = sig

  type error (* entirely abstract since we expose none in a Udp module *)
  (** The type for UDP errors. *)

  val pp_error: error Fmt.t
  (** [pp] is the pretty-printer for errors. *)

  type buffer
  (** The type for memory buffers. *)

  type ip
  (** The type for IPv4/6 stacks for this stack to connect to. *)

  type ipaddr
  (** The type for an IP address representations. *)

  type ipinput
  (** The type for input function continuation to pass onto the
      underlying {!IP} stack. This will normally be a NOOP for a
      conventional kernel, but a direct implementation will parse the
      buffer. *)

  include Mirage_device.S

  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> buffer -> unit io
  (** The type for callback functions that adds the UDP metadata for
      [src] and [dst] IP addresses, the [src_port] of the connection
      and the [buffer] payload of the datagram. *)

  val input: listeners:(dst_port:int -> callback option) -> t -> ipinput
  (** [input listeners t] demultiplexes incoming datagrams based on
      their destination port.  The [listeners] callback will either
      return a concrete handler or a [None], which results in the
      datagram being dropped. *)

  val write: ?src_port:int -> dst:ipaddr -> dst_port:int -> t -> buffer ->
    (unit, error) result io
  (** [write ~src_port ~dst ~dst_port udp data] is a thread
      that writes [data] from an optional [src_port] to a [dst]
      and [dst_port] IPv4 address pair. *)

end

(** {1 TCP stack} *)

(** TCP errors. *)
module Tcp : sig
  type error = [ `Timeout | `Refused]
  type write_error = [ error | Mirage_flow.write_error ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
end

(** Configuration for TCP keep-alives.
    Keep-alive messages are probes sent on an idle connection. If no traffic
    is received after a certain number of probes are sent, then the connection
    is assumed to have been lost. *)
module Keepalive: sig
  type t = {
    after: Duration.t;    (** initial delay before sending probes on an idle
                              connection *)
    interval: Duration.t; (** interval between successive probes *)
    probes: int;          (** total number of probes to send before assuming
                              that, if the connection is still idle it has
                              been lost *)
  }
  (** Configuration for TCP keep-alives *)
end

(** A TCP stack that can send and receive reliable streams using the
    TCP protocol. *)
module type TCP = sig

  type error = private [> Tcp.error]
  (** The type for TCP errors. *)

  type write_error = private [> Tcp.write_error]
  (** The type for TCP write errors. *)

  type buffer
  (** The type for memory buffers. *)

  type ip
  (** The type for IPv4 stacks for this stack to connect to. *)

  type ipaddr
  (** The type for IP address representations. *)

  type ipinput
  (** The type for input function continuation to pass onto the
      underlying {!IP} stack. This will normally be a NOOP for a
      conventional kernel, but a direct implementation will parse the
      buffer. *)

  type flow
  (** A flow represents the state of a single TCPv4 stream that is connected
      to an endpoint. *)

  include Mirage_device.S

  include Mirage_flow.S with
      type 'a io  := 'a io
  and type buffer := buffer
  and type flow   := flow
  and type error  := error
  and type write_error := write_error

  val dst: flow -> ipaddr * int
  (** Get the destination IPv4 address and destination port that a
      flow is currently connected to. *)

  val write_nodelay: flow -> buffer -> (unit, write_error) result io
  (** [write_nodelay flow buffer] writes the contents of [buffer]
      to the flow. The thread blocks until all data has been successfully
      transmitted to the remote endpoint.
      Buffering within the stack is minimized in this mode.
      Note that this API will change in a future revision to be a
      per-flow attribute instead of a separately exposed function. *)

  val writev_nodelay: flow -> buffer list -> (unit, write_error) result io
  (** [writev_nodelay flow buffers] writes the contents of [buffers]
      to the flow. The thread blocks until all data has been successfully
      transmitted to the remote endpoint.
      Buffering within the stack is minimized in this mode.
      Note that this API will change in a future revision to be a
      per-flow attribute instead of a separately exposed function. *)

  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> (flow, error) result io
  (** [create_connection ~keepalive t (addr,port)] opens a TCPv4 connection
      to the specified endpoint.

      If the optional argument [?keepalive] is provided then TCP keep-alive
      messages will be sent to the server when the connection is idle. If
      no responses are received then eventually the connection will be disconnected:
      [read] will return [Ok `Eof] and write will return [Error `Closed] *)

  type listener = {
    process: flow -> unit io; (** process a connected flow *)
    keepalive: Keepalive.t option; (** optional TCP keepalive configuration *)
  }
  (** A TCP listener on a particular port *)

  val input: t -> listeners:(int -> listener option) -> ipinput
  (** [input t listeners] returns an input function continuation to be
      passed to the underlying {!IP} stack.

      When the stack receives a TCP SYN (i.e. a connection request) to a
      particular [port], it will evaluate [listeners port]:

      - If [listeners port] is [None], the input function will return an RST
        to refuse the connection.
      - If [listeners port] is [Some listener] then the connection will be
        accepted and the resulting flow will be processed by [listener.process].
        If [listener.keepalive] is [Some configuration] then the TCP keep-alive
        [configuration] will be applied before calling [listener.process].
  *)


end
