let major_nbd = 43

(** This module type helps us to implement alternative modules for [Stat].
    In particular one that uses the previous ad-hoc functions that where
    incorrect, and one that we can use as reference in case the behaviour
    changes and possibly change the users of [Stat].
  *)
module type S = sig
  type device

  val device : major:int -> minor:int -> device option

  val encode_st_dev : device -> int

  val decode_st_dev : int -> device

  val major : device -> int

  val minor : device -> int

  val pp : Format.formatter -> device -> unit
end

module Stat : S = struct
  include Xapi_stdext_unix.Unixext.Stat

  let major {major; _} = major

  let minor {minor; _} = minor

  let pp =
    Fmt.(
      record ~sep:(any ", ")
        [
          field "major" (fun d -> d.major) int
        ; field "minor" (fun d -> d.minor) int
        ]
    )
end

module Stat_reference : S = struct
  type device = {major: int; minor: int}

  let ( << ) = Stdlib.( lsl )

  let ( >> ) = Stdlib.( lsr )

  let ( &^ ) = Stdlib.( land )

  let ( |^ ) = Stdlib.( lor )

  let device ~major ~minor =
    (* Linux's devids are 32-bit wide and the major and minor ones are 16-bit
       wide, but we can support well up to 32-bit-wide minors *)
    let minor_max = (1 << 32) - 1 in
    let major_max = (1 << 16) - 1 in
    if major < 0 || major_max < major || minor < 0 || minor_max < minor then
      None
    else
      Some {major; minor}

  let encode_st_dev {major; minor} =
    0
    |^ (major &^ 0x00000fff << 8)
    |^ (major &^ 0x7ffff000 << 32)
    |^ (minor &^ 0x000000ff << 0)
    |^ (minor &^ 0xffffff00 << 12)

  let decode_st_dev dev =
    (* follow glibc's implementation, with an exception: the most significant
       bit is ignored because ints are 63 bits in ocaml. In any case,
       [Unix.stat] returns a 63-bit int, so we can't do much in this code to
       avoid this. *)
    let major =
      0 |^ (dev &^ 0x7ffff00000000000 >> 32) |^ (dev &^ 0x00000000000fff00 >> 8)
    in
    let minor =
      0 |^ (dev &^ 0x00000ffffff00000 >> 12) |^ (dev &^ 0x00000000000000ff >> 0)
    in
    {major; minor}

  let major {major; _} = major

  let minor {minor; _} = minor

  let pp =
    Fmt.(
      record ~sep:(any ", ")
        [
          field "major" (fun d -> d.major) int
        ; field "minor" (fun d -> d.minor) int
        ]
    )
end

let hex = Alcotest.testable (Fmt.of_to_string (Format.sprintf "0x%x")) ( = )

let current_t = Alcotest.testable Stat.pp ( = )

let test_combinations f ~major:lst_a ~minor:lst_b =
  let test a b = (Printf.sprintf "major %i, minor %i" a b, `Quick, f a b) in
  List.concat_map (fun a -> List.map (test a) lst_b) lst_a

let spec_minor = [0; 31; 65; 256; 1025; 4098; (1 lsl 32) - 1]

let spec_major = [0; major_nbd; (1 lsl 16) - 1]

let test_reference =
  let test major minor () =
    let current = Stat.device ~major ~minor |> Option.get in
    let reference = Stat_reference.device ~major ~minor |> Option.get in
    let encoded_cur = Stat.encode_st_dev current in
    let encoded_ref = Stat_reference.encode_st_dev reference in

    Alcotest.check hex "Encode must match reference implementation" encoded_ref
      encoded_cur ;

    let decoded_cur = Stat.decode_st_dev encoded_ref in
    let decoded_ref = Stat_reference.decode_st_dev encoded_ref in

    Alcotest.(check @@ pair int int)
      "Decode must match reference implementation"
      Stat_reference.(major decoded_ref, minor decoded_ref)
      Stat.(major decoded_cur, minor decoded_cur)
  in
  let tests = test_combinations test ~major:spec_major ~minor:spec_minor in
  ("Compare with reference", tests)

let test_roundtrip =
  let test major minor () =
    let current = Stat.device ~major ~minor |> Option.get in
    let encoded_cur = Stat.encode_st_dev current in

    let decoded_cur = Stat.decode_st_dev encoded_cur in
    Alcotest.check current_t "Roundtripped current" current decoded_cur
  in
  let tests = test_combinations test ~major:spec_major ~minor:spec_minor in
  ("Roundtrip", tests)

let tests = [test_reference; test_roundtrip]

let () = Alcotest.run "Uniext.Stat suite" tests
