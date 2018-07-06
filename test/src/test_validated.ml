open! Core_kernel
open! Import
open  Validated

module Raw = struct
  type t = int [@@deriving bin_io, compare, hash, sexp, typerep]

  let here = [%here]
  let validate _ = Validate.pass
  let validate_binio_deserialization = true

  let all = [ Int.min_value; -100; -1; 0; 1; 100; Int.max_value ]
end

module V = Make_bin_io_compare_hash_sexp (Raw)

module type Output = sig
  type t [@@deriving sexp_of]
  include Equal.S with type t := t
end

let for_all (type a) raw_fn validated_fn (module Output : Output with type t = a) =
  List.iter Raw.all ~f:(fun raw ->
    let expected = raw_fn raw in
    let actual   = validated_fn (V.create_exn raw) in
    require [%here] (Output.equal expected actual)
      ~if_false_then_print_s:(
        lazy [%message "Validated conversion not equal to raw conversion"
                         (expected : Output.t)
                         (actual : Output.t)
                         (raw : Raw.t)]))
;;

let for_all2 (type a) raw_fn validated_fn (module Output : Output with type t = a) =
  List.iter Raw.all ~f:(fun raw1 ->
    List.iter Raw.all ~f:(fun raw2 ->
      let expected = raw_fn raw1 raw2 in
      let actual   = validated_fn (V.create_exn raw1) (V.create_exn raw2) in
      require [%here] (Output.equal expected actual)
        ~if_false_then_print_s:(
          lazy [%message "Validated conversion not equal to raw conversion"
                           (expected : Output.t)
                           (actual : Output.t)
                           (raw1 : Raw.t)
                           (raw2 : Raw.t)])))
;;

let%expect_test "[bin_io]s match" =
  let save_bin_io (type a) (module M : Binable with type t = a) a =
    let buf = Bigstring.create 128 in
    let len = M.bin_write_t buf ~pos:0 a in
    Bigstring.to_string buf ~len
  in
  for_all
    (save_bin_io (module Raw))
    (save_bin_io (module V))
    (module String);
  [%expect {| |}];
;;

let%expect_test "[compare]s match" =
  for_all2 Raw.compare V.compare (module Int);
  [%expect {| |}];
;;

let%expect_test "[hash]s match" =
  for_all Raw.hash V.hash (module Int);
  [%expect {| |}];
;;

let%expect_test "[sexp]s match" =
  for_all [%sexp_of: Raw.t] [%sexp_of: V.t] (module Sexp);
  [%expect {| |}];
;;

(* [Add_typerep] *)
module V2 : sig
  type t = V.t [@@deriving typerep]
end = struct
  type t = V.t
  include Add_typerep (Raw) (V)
end
