open! Import
open Std_internal

module Stable = struct
  module V1 = struct
    type t = float [@@deriving hash]

    let of_mult f = f
    let to_mult t = t

    let of_percentage f = f /. 100.
    let to_percentage t = t *. 100.

    let of_bp f = f /. 10_000.
    let to_bp t = t *. 10_000.

    let of_bp_int i = of_bp (Float.of_int i)
    let to_bp_int t = Float.to_int (to_bp t)

    module Format = struct
      type t =
        | Exponent of int
        | Exponent_E of int
        | Decimal of int
        | Ocaml
        | Compact of int
        | Compact_E of int
        | Hex of int
        | Hex_E of int
      [@@deriving sexp_of]

      let exponent   ~precision = Exponent    precision
      let exponent_E ~precision = Exponent_E  precision
      let decimal    ~precision = Decimal     precision
      let ocaml                 = Ocaml
      let compact    ~precision = Compact     precision
      let compact_E  ~precision = Compact_E   precision
      let hex        ~precision = Hex         precision
      let hex_E      ~precision = Hex_E       precision

      let format_float t =
        match t with
        | Exponent   precision -> sprintf "%.*e" precision
        | Exponent_E precision -> sprintf "%.*E" precision
        | Decimal    precision -> sprintf "%.*f" precision
        | Ocaml                -> sprintf   "%F"
        | Compact    precision -> sprintf "%.*g" precision
        | Compact_E  precision -> sprintf "%.*G" precision
        | Hex        precision -> sprintf "%.*h" precision
        | Hex_E      precision -> sprintf "%.*H" precision
    end

    let format x format =
      let x_abs = Float.abs x in
      let string float = Format.format_float format float in
      if x_abs = 0. then "0x"
      else if x_abs >= 1. then
        string (x *. 1.) ^ "x"
      else if x_abs >= 0.01 then
        string (x *. 100.) ^ "%"
      else
        string (x *. 10_000.) ^ "bp"

    module Stringable = struct

      type t = float

      (* WARNING - PLEASE READ BEFORE EDITING THESE FUNCTIONS:

         The string converters in Stable.V1 should never change.  If you are changing the
         semantics of anything that affects the sexp or bin-io representation of values of
         this type (this includes to_string and of_string) make a Stable.V2 and make your
         changes there.  Thanks! *)
      let to_string x =
        let x_abs = Float.abs x in
        let string float = sprintf "%.6G" float in
        if x_abs = 0. then "0x"
        else if x_abs >= 1. then
          string (x *. 1.) ^ "x"
        else if x_abs >= 0.01 then
          string (x *. 100.) ^ "%"
        else
          string (x *. 10_000.) ^ "bp"

      let really_of_string str float_of_string =
        match String.chop_suffix str ~suffix:"x" with
        | Some str -> float_of_string str
        | None ->
          match String.chop_suffix str ~suffix:"%" with
          | Some str -> float_of_string str *. 0.01
          | None ->
            match String.chop_suffix str ~suffix:"bp" with
            | Some str -> of_bp (float_of_string str)
            | None ->
              failwithf "Percent.of_string: must end in x, %%, or bp: %s" str ()

      let of_string str =
        let float str = Float_with_finite_only_serialization.t_of_sexp (Sexp.Atom str) in
        really_of_string str float

      let of_string_allow_nan_and_inf str =
        really_of_string str Float.of_string
    end

    include (Stringable : sig
               type t
               val of_string : string -> t
               val to_string : t -> string
             end with type t := t)

    include (Sexpable.Stable.Of_stringable.V1 (Stringable) : Sexpable.S with type t := t)
    include (Float : Binable with type t := t)
    include Comparable.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]

        (* Previous versions rendered comparable-based containers using float
           serialization rather than percent serialization, so when reading
           comparable-based containers in we accept either serialization. *)
        let t_of_sexp sexp =
          match Float.t_of_sexp sexp with
          | float       -> float
          | exception _ -> t_of_sexp sexp
      end)
  end

  let%test_module "Percent.V1" = (module Stable_unit_test.Make (struct
      include V1

      let _coerce t =
        ignore (t : t);
        ignore (t : float)

      let tests =
        [ 0.375, "37.5%", "\000\000\000\000\000\000\216?"
        ; 4.5, "4.5x", "\000\000\000\000\000\000\018@"
        ; 0.0002, "2bp", "-C\028\235\2266*?"
        ; 0.000075, "0.75bp", "a2U0*\169\019?"
        ]
    end))

  let%test_unit {|
    BUG: The sexp functions don't roundtrip.

    In this case problem is [of_percentage] divides by 100, and [of_string]
    scales by 0.01, which can yield different results.
  |} =
    let t = V1.of_percentage 3.638 in
    [%test_result: int] (compare t (V1.t_of_sexp (V1.sexp_of_t t))) ~expect:(-1);
  ;;

end

include Stable.V1

let is_zero t = t = 0.

let apply t f = t *. f
let scale t f = t *. f

include (struct
  include Float
  let sign = sign_exn
end : sig
           val zero : t
           val ( * ) : t -> t -> t
           val ( + ) : t -> t -> t
           val ( - ) : t -> t -> t
           val abs : t -> t
           val neg : t -> t
           val is_nan : t -> bool
           val is_inf : t -> bool
           val sign_exn : t -> Sign.t
           include Comparable.With_zero with type t := t
           include Robustly_comparable with type t := t
         end)

let validate = Float.validate_ordinary

let of_string_allow_nan_and_inf s =
  Stringable.of_string_allow_nan_and_inf s

let t_of_sexp_allow_nan_and_inf sexp =
  of_string_allow_nan_and_inf (Sexp.to_string sexp)

let%test_unit _ = (* [t_of_sexp] *)
  List.iter ~f:(fun (string, expected) ->
    assert (equal (t_of_sexp (Sexp.Atom string)) expected);
  )
    [ "30%"    , 0.3
    ; "3123bp" , 0.3123
    ; "3.17x"  , 3.17
    ; "0.0003x", 0.0003
    ; "0%"     , 0.
    ; "0bp"    , 0.
    ; "0x"     , 0.
    ; "0.000%" , 0.
    ; "0.00bp" , 0.
    ; "0.00x"  , 0.
    ; "3.1e5%" , 3100.
    ; "3.1e5bp", 31.
    ; "3.1e5x" , 310000.
    ; "10%"    , 0.1
    ; "110%"   , 1.1
    ; "0.1x"   , 0.1
    ; "1.1x"   , 1.1
    ; "0.001x" , 0.001
    ; "1bp"    , 0.0001
    ; "10bp"   , 0.001
    ; "100bp"  , 0.01
    ; "1000bp" , 0.1
    ; "11000bp", 1.1
    ; "1.1e4bp", 1.1
    ; "50%"    , 0.5
    ]
;;

let%test_unit _ = (* [sexp_of_t] and [t_of_sexp] *)
  List.iter ~f:(fun (t1, expected) ->
    let s = Sexp.to_string (sexp_of_t t1) in
    assert (String.equal s expected);
    let t2 = t_of_sexp (Sexp.of_string s) in
    assert (equal t1 t2);
  )
    [ 0.3    , "30%"
    ; 0.335  , "33.5%"
    ; 0.00335, "33.5bp"
    ; 33.46  , "33.46x"
    ; 0.1    , "10%"
    ; 0.99   , "99%"
    ; 1.     , "1x"
    ; 10.    , "10x"
    ; 0.001  , "10bp"
    ; 0.0001 , "1bp"
    ; 0.00001, "0.1bp"
    ; 0.5    , "50%"
    ]
;;

let%test_unit _ = (* [to_string'] and [to_string_f] *)
  List.iter ~f:(fun (t, precision, format', expect, expectf) ->
    let output' = format t (Format.compact_E ~precision) in
    assert (String.equal output' expect);
    let outputf = format t format' in
    assert (String.equal outputf expectf)
  )
    [ 0.3       , 3, (Format.decimal ~precision:3), "30%"    , "30.000%"
    ; 0.333     , 4, (Format.decimal ~precision:2), "33.3%"  , "33.30%"
    ; 0.333333  , 4, (Format.decimal ~precision:2), "33.33%" , "33.33%"
    ; 0.000333  , 2, (Format.decimal ~precision:2), "3.3bp"  , "3.33bp"
    ]
;;
