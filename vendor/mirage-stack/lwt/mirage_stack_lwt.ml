type 'netif stackv4_config = {
  name: string;
  interface: 'netif;
}

type socket_stack_config =
  Ipaddr.V4.t list

module type V4 = sig
  include Mirage_stack.V4
  with type 'a io = 'a Lwt.t
   and type 'a config = 'a stackv4_config
   and type ipv4addr = Ipaddr.V4.t
   and type buffer = Cstruct.t
end
