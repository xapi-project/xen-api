#use "topfind";;
#require "base";;
#require "stdio";;

open Base
open Stdio
open Ppxlib

module N = Ppxlib_private.Name
[%%expect{|
module N = Ppxlib.Ppxlib_private.Name
|}]


let dot_suffixes name =
  Caml.Printf.sprintf "%s"
    (Sexp.to_string_hum
       (List.sexp_of_t String.sexp_of_t (N.dot_suffixes name)))
[%%expect{|
val dot_suffixes : string -> string = <fun>
|}]

let _ = dot_suffixes "foo.bar.baz"
[%%expect{|
- : string = "(baz bar.baz foo.bar.baz)"
|}]

let _ = dot_suffixes "foo.@bar.baz"
[%%expect{|
- : string = "(bar.baz foo.bar.baz)"
|}]


let split_path name =
    let a, b = N.split_path name in
    Caml.Printf.sprintf "%s"
      (Sexp.to_string_hum
         (List [sexp_of_string a; Option.sexp_of_t sexp_of_string b]))
[%%expect{|
val split_path : string -> string = <fun>
|}]

let _ = split_path "a.b.c"
[%%expect{|
- : string = "(a.b.c ())"
|}]

let _ = split_path "a.b.c.D"
[%%expect{|
- : string = "(a.b.c (D))"
|}]

let _ = split_path ".D"
[%%expect{|
- : string = "(\"\" (D))"
|}]
