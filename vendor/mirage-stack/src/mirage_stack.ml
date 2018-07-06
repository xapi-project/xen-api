module type V4 = sig
  type netif
  (** The type for network interface that is used to transmit and
      receive traffic associated with this stack. *)

  type 'netif config
  (** The type for the collection of user configuration specified to
      construct a stack. *)

  type ipv4addr
  (** The type for IPv4 addresses. *)

  type buffer
  (** The type for memory buffers. *)

  type udpv4
  (** The type for UDPv4 implementations. *)

  type tcpv4
  (** The type for TCPv4 implementations. *)

  type ipv4
  (** The type for IPv4 implementations. *)

  include Mirage_device.S

  module UDPV4: Mirage_protocols.UDP
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = udpv4
     and type ip = ipv4

  module TCPV4: Mirage_protocols.TCP
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = tcpv4
     and type ip = ipv4

  module IPV4: Mirage_protocols.IPV4
    with type +'a io = 'a io
     and type ipaddr = ipv4addr
     and type buffer = buffer
     and type t = ipv4

  val udpv4: t -> udpv4
  (** [udpv4 t] obtains a descriptor for use with the [UDPV4] module,
      usually to transmit traffic. *)

  val tcpv4: t -> tcpv4
  (** [tcpv4 t] obtains a descriptor for use with the [TCPV4] module,
      usually to initiate outgoing connections. *)

  val ipv4: t -> ipv4
  (** [ipv4 t] obtains a descriptor for use with the [IPV4] module,
      which can handle raw IPv4 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen_udpv4: t -> port:int -> UDPV4.callback -> unit
  (** [listen_udpv4 t ~port cb] registers the [cb] callback on the
      UDPv4 [port] and immediately return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash. *)

  val listen_tcpv4: ?keepalive:Mirage_protocols.Keepalive.t
    -> t -> port:int -> (TCPV4.flow -> unit io) -> unit
  (** [listen_tcpv4 ~keepalive t ~port cb] registers the [cb] callback
      on the TCPv4 [port] and immediatey return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash.
      If [~keepalive] is provided then these keepalive settings will be
      applied to the accepted connections before the callback is called. *)

  val listen: t -> unit io
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end
