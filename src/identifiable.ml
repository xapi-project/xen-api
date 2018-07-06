open! Import

module Binable = Binable0

let failwiths = Error.failwiths

module type S_common = sig
  type t [@@deriving compare, hash, sexp_of]
  include Stringable.S     with type t := t
  include Pretty_printer.S with type t := t
end

module type S_plain = sig
  include S_common
  include Comparable.S_plain with type t := t
  include Hashable.S_plain   with type t := t
end

module type S_not_binable = sig
  type t [@@deriving hash, sexp]
  include S_common     with type t := t
  include Comparable.S with type t := t
  include Hashable.S   with type t := t
end

module type S = sig
  type t [@@deriving bin_io, hash, sexp]
  include S_common             with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
end

module Make_plain (T : sig
    type t [@@deriving compare, hash, sexp_of]
    include Stringable.S with type t := t
    val module_name : string
  end) = struct
  include T
  include Comparable.Make_plain   (T)
  include Hashable.Make_plain     (T)
  include Pretty_printer.Register (T)
end

module Make (T : sig
    type t [@@deriving bin_io, compare, hash, sexp]
    include Stringable.S with type t := t
    val module_name : string
  end) = struct
  include T
  include Comparable.Make_binable (T)
  include Hashable.Make_binable   (T)
  include Pretty_printer.Register (T)
end

module Make_and_derive_hash_fold_t (T : sig
    type t [@@deriving bin_io, compare, sexp]
    include Stringable.S with type t := t
    val hash : t -> int
    val module_name : string
  end) =
  Make (struct
    include T
    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Make_using_comparator (T : sig
    type t [@@deriving bin_io, compare, hash, sexp]
    include Comparator.S with type t := t
    include Stringable.S with type t := t
    val module_name : string
  end) = struct
  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable.Make_binable                    (T)
  include Pretty_printer.Register                  (T)
end

module Make_using_comparator_and_derive_hash_fold_t (T : sig
    type t [@@deriving bin_io, compare, sexp]
    include Comparator.S with type t := t
    include Stringable.S with type t := t
    val hash : t -> int
    val module_name : string
  end) =
  Make_using_comparator (struct
    include T
    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Extend(M : Base.Identifiable.S)(B : Binable0.S with type t = M.t) =
struct
  module T = struct
    include M
    include (B : Binable.S with type t := t)
  end
  include T
  include Comparable.Extend_binable (M) (T)
  include Hashable.Make_binable         (T)
end

(* The unit test below checks that for a call to [Identifiable.Make], the functions in the
   resulting module call the functions in the argument module the correct number of
   times. *)
let%test_module _ =
  (module struct

    module Counter = struct
      type t =
        | Compare
        | Hash
        | Of_string
        | Sexp_of_t
        | T_of_sexp
        | To_string
      [@@deriving compare, hash, sexp]
    end

    open Counter

    module Counts = struct
      module Map = Map.Make (Counter)

      type t = int Map.t ref [@@deriving sexp_of]

      let actual = ref Map.empty
      let expected = ref Map.empty

      let incr ?(by = 1) t counter =
        t := Map.update !t counter ~f:(function None -> by | Some i -> i + by)
      ;;

      let check location =
        if not (Map.equal (=) !actual !expected) then
          failwiths "mismatch" (location, `actual actual, `expected expected)
            [%sexp_of: Source_code_position.t * [ `actual of t ] * [ `expected of t ]]
      ;;
    end

    module T = struct

      let module_name = "Core_kernel.Identifiable.T"

      type t = A | B [@@deriving bin_io, compare, hash, sexp]

      include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)

      let incr ?by counter = Counts.incr Counts.actual counter ?by

      let compare t1 t2 = incr Compare; compare t1 t2
      let hash t = incr Hash; hash t
      let sexp_of_t t = incr Sexp_of_t; sexp_of_t t
      let t_of_sexp t = incr T_of_sexp; t_of_sexp t
      let of_string t = incr Of_string; of_string t
      let to_string t = incr To_string; to_string t
    end

    module Id = Make (T)

    let int_equal (i1 : int) i2 = Poly.equal i1 i2

    let%test_unit _ =
      let open T in
      let open Id in
      let check = Counts.check in
      let incr ?by counter = Counts.incr Counts.expected counter ?by in
      check [%here];
      ignore (to_string A : string);
      incr To_string;
      check [%here];
      ignore (of_string "A" : t);
      incr Of_string;
      check [%here];
      ignore (t_of_sexp (Sexplib.Sexp.of_string "A") : t);
      incr T_of_sexp;
      check [%here];
      ignore (sexp_of_t A : Base.Sexp.t);
      incr Sexp_of_t;
      check [%here];
      assert (int_equal (compare A A) 0);
      incr Compare;
      check [%here];
      assert (int_equal (compare A B) (-1));
      incr Compare;
      check [%here];
      assert (int_equal (compare B A) 1);
      incr Compare;
      check [%here];
      ignore (not (int_equal (hash A) (hash B)));
      incr Hash ~by:2;
      check [%here];
      let bigstring = Binable.to_bigstring (module T) A in
      check [%here];
      assert (Poly.equal A (Binable.of_bigstring (module T) bigstring));
      check [%here]
    ;;
  end)
