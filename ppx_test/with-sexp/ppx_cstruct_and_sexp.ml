[%%cstruct type foo = {
  magic: uint8_t [@len 16];
}[@@little_endian]]

open Sexplib.Std
type t = int [@@deriving sexp]
