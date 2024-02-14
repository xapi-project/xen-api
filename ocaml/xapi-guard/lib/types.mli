module Service : sig
  type t = Varstored | Swtpm

  val typ_of : t Rpc.Types.typ

  val to_string : t -> string
end
