open! Import

type t = char [@@deriving typerep]

module Z =
  Identifiable.Extend (Base.Char) (struct
    type t = char [@@deriving bin_io]
  end)

include (Z : module type of struct include Z end
         with module Replace_polymorphic_compare := Z.Replace_polymorphic_compare)

(* include [Base.Char] after the application of [Identifiable.Extend] to replace the [Comparable]
   functions with the pervasive versions *)
include (Base.Char
         : module type of struct include Base.Char end
         with type t := t)

module Replace_polymorphic_compare = Base.Char

let gen            = Quickcheck.Generator.char
let gen_digit      = Quickcheck.Generator.char_digit
let gen_lowercase  = Quickcheck.Generator.char_lowercase
let gen_uppercase  = Quickcheck.Generator.char_uppercase
let gen_alpha      = Quickcheck.Generator.char_alpha
let gen_alphanum   = Quickcheck.Generator.char_alphanum
let gen_print      = Quickcheck.Generator.char_print
let gen_whitespace = Quickcheck.Generator.char_whitespace
let obs            = Quickcheck.Observer.char
let shrinker       = Quickcheck.Shrinker.char
