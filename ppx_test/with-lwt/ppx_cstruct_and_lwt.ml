[%%cstruct type foo = {
  magic: uint8_t [@len 16];
}[@@little_endian]]

let%lwt foo = Lwt.return ()

