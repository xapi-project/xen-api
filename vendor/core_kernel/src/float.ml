external format_float : string -> float -> string = "caml_format_float"

(* Stolen from [pervasives.ml].  Adds a "." at the end if needed.  It is in
   [pervasives.mli], but it also says not to use it directly, so we copy and paste the
   code. It makes the assumption on the string passed in argument that it was returned by
   [format_float] *)
let valid_float_lexem s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s
  in
  loop 0
;;

open! Import

module List  = Base.List

module T = struct
  type t = float [@@deriving bin_io, typerep]

  include (Base.Float : module type of struct include Base.Float end
           with type t := t
           with module O := Base.Float.O
           with module Terse := Base.Float.Terse)
end
include T
include Hashable  .Make_binable                         (T)
include Comparable.Map_and_set_binable_using_comparator (T)
module Replace_polymorphic_compare = (T : Comparisons.S with type t := t)

module Robust_compare = struct
  module type S = sig
    (* intended to be a tolerance on human-entered floats *)
    val robust_comparison_tolerance : float
    include Robustly_comparable.S with type t := float
  end

  module Make(T : sig val robust_comparison_tolerance : float end) : S = struct
    (* We have test in the tree that rely on these functions not allocating, even without
       X_LIBRARY_INLING. The only way to ensure that these don't create temporary boxed
       floats without X_LIBRARY_INLING is for this code to see the float operations as
       externals, as defined in [Pervasives]. That's why we open [Caml.Pervasives]
       here. *)
    open Caml.Pervasives
    let robust_comparison_tolerance = T.robust_comparison_tolerance
    let ( >=. ) x y = x >= y -. robust_comparison_tolerance
    let ( <=. ) x y = y >=. x
    let ( =. ) x y = x >=. y && y >=. x
    let ( >. ) x y = x > y +. robust_comparison_tolerance
    let ( <. ) x y = y >. x
    let ( <>. ) x y = not (x =. y)
    let robustly_compare x y =
      let d = x -. y in
      if      d < ~-. robust_comparison_tolerance then -1
      else if d >     robust_comparison_tolerance then  1
      else 0
  end
end

module Robustly_comparable =
  Robust_compare.Make (struct let robust_comparison_tolerance = 1E-7 end)
include Robustly_comparable

module O = struct
  include Base.Float.O
  include Robustly_comparable
end

module Terse = struct
  type nonrec t = t [@@deriving bin_io]
  include (Base.Float.Terse
           : module type of struct include Base.Float.Terse end
           with type t := t)
end

let robust_sign t : Sign.t =
  if t >. 0.
  then Pos
  else if t <. 0.
  then Neg
  else Zero

(* There are two issues:
   - Float.sign used to use robust comparison, and users of [Core] might have come to
     depend on this.
   - Robustness aside, what we get from Comparable.With_zero would map nan to Neg.
*)
let sign = robust_sign

(* Standard 12 significant digits, exponential notation used as necessary, guaranteed to
   be a valid OCaml float lexem, not to look like an int. *)
let to_string_12 x = valid_float_lexem (format_float "%.12g" x);;

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Let_syntax

  let zero_exponent = ieee_exponent zero
  let zero_mantissa = ieee_mantissa zero

  let max_positive_subnormal_value = one_ulp `Down min_positive_normal_value

  let subnormal_exponent = ieee_exponent min_positive_subnormal_value

  let min_subnormal_mantissa = ieee_mantissa min_positive_subnormal_value
  let max_subnormal_mantissa = ieee_mantissa max_positive_subnormal_value

  let max_positive_normal_value = max_finite_value

  let min_normal_exponent = ieee_exponent min_positive_normal_value
  let max_normal_exponent = ieee_exponent max_positive_normal_value

  let _min_normal_mantissa = ieee_mantissa min_positive_normal_value
  let max_normal_mantissa = ieee_mantissa max_positive_normal_value

  let inf_exponent = ieee_exponent infinity
  let inf_mantissa = ieee_mantissa infinity

  let nan_exponent = ieee_exponent nan

  let min_nan_mantissa = Int63.succ inf_mantissa
  let max_nan_mantissa = max_normal_mantissa

  let test_class gen expect =
    Quickcheck.test gen ~f:(fun float ->
      let actual = classify float in
      if not (Int.equal (Class.compare actual expect) 0) then begin
        raise_s [%message
          "generator produced float in wrong class"
            (float  : t)
            (expect : Class.t)
            (actual : Class.t)]
      end)

  let num_mantissa_bits = 52

  (* We weight mantissas so that "integer-like" values, and values with only a few digits
     past the decimal, are reasonably common. *)
  let gen_normal_mantissa =
    let%bind num_bits = Int.gen_uniform_incl 0 num_mantissa_bits in
    let%map  bits =
      Int63.gen_incl
        Int63.zero
        (Int63.pred (Int63.shift_left Int63.one num_bits))
    in
    Int63.shift_left bits (Int.( - ) num_mantissa_bits num_bits)

  let gen_exponent_weighted_low lower_bound upper_bound =
    let%map offset = Int.gen_log_incl 0 (Int.( - ) upper_bound lower_bound) in
    Int.( + ) lower_bound offset

  let gen_exponent_weighted_high lower_bound upper_bound =
    let%map offset = Int.gen_log_incl 0 (Int.( - ) upper_bound lower_bound) in
    Int.( - ) upper_bound offset

  (* We weight exponents such that values near 1 are more likely. *)
  let gen_exponent =
    let midpoint = ieee_exponent 1. in
    Generator.weighted_union
      [ 0.5, gen_exponent_weighted_high min_normal_exponent midpoint
      ; 0.5, gen_exponent_weighted_low  midpoint max_normal_exponent
      ]

  let gen_zero =
    let%map negative = Bool.gen
    in
    create_ieee_exn ~negative ~exponent:zero_exponent ~mantissa:zero_mantissa

  let%test_unit _ = test_class gen_zero Zero

  let gen_subnormal =
    let%map negative = Bool.gen
    and     exponent = return subnormal_exponent
    and     mantissa = Int63.gen_log_incl min_subnormal_mantissa max_subnormal_mantissa
    in
    create_ieee_exn ~negative ~exponent ~mantissa

  let%test_unit _ = test_class gen_subnormal Subnormal

  let gen_normal =
    let%map negative = Bool.gen
    and     exponent = gen_exponent
    and     mantissa = gen_normal_mantissa
    in
    create_ieee_exn ~negative ~exponent ~mantissa

  let%test_unit _ = test_class gen_normal Normal

  let gen_infinite =
    let%map negative = Bool.gen
    in
    create_ieee_exn ~negative ~exponent:inf_exponent ~mantissa:inf_mantissa

  let%test_unit _ = test_class gen_infinite Infinite

  let gen_nan =
    let%map negative = Bool.gen
    and     exponent = return nan_exponent
    and     mantissa = Int63.gen_incl min_nan_mantissa max_nan_mantissa
    in
    create_ieee_exn ~negative ~exponent ~mantissa

  let%test_unit _ = test_class gen_nan Nan

  let gen_by_class c =
    match (c : Class.t) with
    | Zero      -> gen_zero
    | Subnormal -> gen_subnormal
    | Normal    -> gen_normal
    | Infinite  -> gen_infinite
    | Nan       -> gen_nan

  let weight_of_class c =
    match (c : Class.t) with
    | Zero      ->   1.
    | Subnormal ->  10.
    | Normal    -> 100.
    | Infinite  ->   1.
    | Nan       ->   1.

  let gen_matching_classes filter =
    List.filter_map Class.all ~f:(fun c ->
      if filter c
      then Some (weight_of_class c, gen_by_class c)
      else None)
    |> Generator.weighted_union

  let gen_finite =
    gen_matching_classes (function
      | Zero | Subnormal | Normal -> true
      | Infinite | Nan            -> false)

  let gen_without_nan =
    gen_matching_classes (function
      | Zero | Subnormal | Normal | Infinite -> true
      | Nan                                  -> false)

  let gen = gen_matching_classes (fun _ -> true)

  let gen_finite_non_zero =
    gen_matching_classes (function
      | Subnormal | Normal    -> true
      | Zero | Infinite | Nan -> false)

  let gen_positive =
    let%map t = gen_finite_non_zero in
    abs t

  let gen_negative =
    let%map t = gen_finite_non_zero in
    ~-. (abs t)

  let gen_uniform_excl lower_bound upper_bound =
    if not (is_finite lower_bound) || not (is_finite upper_bound) then begin
      raise_s [%message
        "Float.gen_uniform_excl: bounds are not finite"
          (lower_bound : t)
          (upper_bound : t)]
    end;
    let lower_incl = one_ulp `Up   lower_bound in
    let upper_incl = one_ulp `Down upper_bound in
    if lower_incl > upper_incl then begin
      raise_s [%message
        "Float.gen_uniform_excl: requested range is empty"
          (lower_bound : t)
          (upper_bound : t)]
    end;
    Generator.create (fun ~size:_ random ->
      Splittable_random.float random ~lo:lower_incl ~hi:upper_incl)

  let gen_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9,  gen_uniform_excl lower_bound upper_bound
      ]

  let obs = Observer.unmap Int64.obs ~f:Int64.bits_of_float

  let shrinker = Shrinker.empty ()

end

let gen              = For_quickcheck.gen
let gen_uniform_excl = For_quickcheck.gen_uniform_excl
let gen_incl         = For_quickcheck.gen_incl
let gen_without_nan  = For_quickcheck.gen_without_nan
let gen_finite       = For_quickcheck.gen_finite
let gen_positive     = For_quickcheck.gen_positive
let gen_negative     = For_quickcheck.gen_negative
let obs              = For_quickcheck.obs
let shrinker         = For_quickcheck.shrinker

(* Additional tests of Base.Float requiring the Gc module *)

let%test _ [@tags "64-bits-only"] =
  let before = Gc.minor_words () in
  assert (Int63.equal (int63_round_nearest_exn 0.8) (Int63.of_int_exn 1));
  let after = Gc.minor_words () in
  Int.equal before after

let%test_unit "Float.validate_positive doesn't allocate on success" =
  let initial_words = Gc.minor_words () in
  let _ : Validate.t = validate_positive 1. in
  let allocated = Int.(-) (Gc.minor_words ()) initial_words in
  [%test_result: int] allocated ~expect:0
;;

let to_string_round_trippable = to_string
