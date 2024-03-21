module Ipaddr : sig
  include module type of Ipaddr

  val typ_of : t Rpc.Types.typ
end
