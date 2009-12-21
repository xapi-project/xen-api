(* LVM uses CRC to verify data *)

let initial_crc = 0xf597a6cfl

let crctab = [| 
  0x00000000l; 0x1db71064l; 0x3b6e20c8l; 0x26d930acl;
  0x76dc4190l; 0x6b6b51f4l; 0x4db26158l; 0x5005713cl;
  0xedb88320l; 0xf00f9344l; 0xd6d6a3e8l; 0xcb61b38cl;
  0x9b64c2b0l; 0x86d3d2d4l; 0xa00ae278l; 0xbdbdf21cl;
|]

let crc buf init =
  let (>>) a b = Int32.shift_right_logical a b in
  let (^^) a b = Int32.logxor a b in
  let (&&&) a b = Int32.to_int (Int32.logand a b) in

  let size = String.length buf in
  let rec loop i cur =
    if i=size then cur else
      let a1 = cur ^^ (Int32.of_int (int_of_char buf.[i])) in
      let a2 = (a1 >> 4) ^^ crctab.( a1 &&& 0xfl ) in
      let a3 = (a2 >> 4) ^^ crctab.( a2 &&& 0xfl ) in
      loop (i+1) a3
  in

  loop 0 init
