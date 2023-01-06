Test using the examples from the OCaml manual: https://v2.ocaml.org/manual/intfc.html
Type abbreviations are not expanded:

  $ cat >test.ml <<EOF
  > external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
  > external seek_in_pair: in_channel * int -> unit = "caml_ml_seek_in_pair"
  > type int_endo = int -> int
  > external f : int_endo -> int_endo = "f"
  > external g : (int -> int) -> (int -> int) = "g"
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  CAMLprim value caml_ml_seek_in(value, value);
  CAMLprim value caml_ml_seek_in_pair(value);
  CAMLprim value f(value);
  CAMLprim value g(value, value);

With --verifier output a special attribute recognized by the static analyzer.
This is needed so it can find all the C stubs (CAMLprim is defined to empty in
headers, and thus normally disappears after preprocessing,
and the static analyzer works on preprocessed source code):
  $ lintcstubs_arity --verifier test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  #undef CAMLprim
  #define CAMLprim __attribute__((section("goblint-ocaml-cstub")))
  CAMLprim value caml_ml_seek_in(value, value);
  CAMLprim value caml_ml_seek_in_pair(value);
  CAMLprim value f(value);
  CAMLprim value g(value, value);


Arity <= 5 is implemented directly:
  $ cat >test.ml <<EOF
  > external input : in_channel -> bytes -> int -> int -> int = "input"
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  CAMLprim value input(value, value, value, value);

Arity > 5 is implemented differently in bytecode and native code:
  $ cat >test.ml <<EOF
  > external add_nat: nat -> int -> int -> nat -> int -> int -> int -> int
  >                 = "add_nat_bytecode" "add_nat_native"
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  CAMLprim value add_nat_native(value, value, value, value, value, value, value);
  CAMLprim value add_nat_bytecode(value *argv, int argn);

Native code can take some arguments unboxed, but that would require a typedtree
to be done correctly (it is possible to redefine 'type int = string'), so just print a warning here.
  $ cat >test.ml <<EOF
  > external foo
  > :  (float [@unboxed])
  > -> (float [@unboxed])
  > -> (float [@unboxed])
  > = "foo_byte" "foo"
  > external foo : float -> float -> float = "foo2_byte" "foo2" [@@unboxed]
  > external f : string -> (int [@untagged]) = "f_byte" "f"
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  File "test.ml", lines 1-5, characters 0-18:
  1 | external foo
  2 | :  (float [@unboxed])
  3 | -> (float [@unboxed])
  4 | -> (float [@unboxed])
  5 | = "foo_byte" "foo"
  Warning 22 [preprocessor]: Ignored primitive declaration "foo": has attributes
  File "test.ml", line 6, characters 0-71:
  6 | external foo : float -> float -> float = "foo2_byte" "foo2" [@@unboxed]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 22 [preprocessor]: Ignored primitive declaration "foo": has attributes
  File "test.ml", line 7, characters 0-55:
  7 | external f : string -> (int [@untagged]) = "f_byte" "f"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 22 [preprocessor]: Ignored primitive declaration "f": has attributes

Noalloc makes it possible to call C code directly, however unboxed is not
supported by this tool for the same reason as above:
  $ cat >test.ml <<EOF
  > external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  > [@@unboxed] [@@noalloc]
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  File "test.ml", lines 1-2, characters 0-23:
  1 | external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  2 | [@@unboxed] [@@noalloc]
  Warning 22 [preprocessor]: Ignored primitive declaration "sqrt": has attributes

Noalloc can also be used without unboxed:
  $ cat >test.ml <<EOF
  > external unsafe_blit: t -> int -> t -> int -> int -> unit =
  >  "caml_floatarray_blit" [@@noalloc]
  > EOF
  $ lintcstubs_arity test.ml
  #define CAML_NAME_SPACE
  #include <caml/mlvalues.h>
  CAMLprim value caml_floatarray_blit(value, value, value, value, value);
