module Service = struct
  type t = Varstored | Swtpm [@@deriving rpcty]

  let to_string = function Varstored -> "Varstored" | Swtpm -> "Swtpm"
end
