
let xensource_oui = "00:16:3E"

let random_mac () =
  Printf.sprintf "%s:%02X:%02X:%02X" xensource_oui (Random.int 0x80) (Random.int 0x100) (Random.int 0x100) 
