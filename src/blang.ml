open! Import
open Std_internal

(* the module [T] serves to enforce the invariant that all Blang.t values are in a
   normal form whereby boolean constants True and False only appear as the topmost
   constructor -- in any other position they are simplified away using laws of
   boolean algebra.

   Note: this file deviates from the usual pattern of modules with Stable interfaces in
   that the Stable sub-module is not the first thing to be defined in the module.  The
   reason for this deviation is so that one can convince oneself of the aforementioned
   invariant after reading only this small amount of code.  After defining T we then
   immediately define its Stable interface.
*)
module T : sig
  type 'a t = private
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  [@@deriving bin_io, compare, hash]

  val invariant : 'a t -> unit

  val true_   : 'a t
  val false_  : 'a t
  val not_    : 'a t -> 'a t
  val andalso : 'a t -> 'a t -> 'a t
  val orelse  : 'a t -> 'a t -> 'a t
  val if_     : 'a t -> 'a t -> 'a t -> 'a t
  val base    : 'a -> 'a t

end = struct

  type 'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  [@@deriving bin_io, compare, hash]

  let invariant =
    let subterms = function
      | True | False | Base _      -> []
      | Not t1                     -> [t1]
      | And (t1, t2) | Or (t1, t2) -> [t1; t2]
      | If (t1, t2, t3)            -> [t1; t2; t3]
    in
    let rec contains_no_constants = function
      | True | False -> assert false
      | t -> List.iter ~f:contains_no_constants (subterms t)
    in
    fun t ->
      List.iter ~f:contains_no_constants (subterms t)

  let true_ = True
  let false_ = False
  let base v = Base v

  let not_ = function
    | True -> False
    | False -> True
    | Not t -> t
    | t -> Not t

  let andalso t1 t2 =
    match (t1, t2) with
    | (_, False) | (False, _) -> False
    | (other, True) | (True, other) -> other
    | _ -> And (t1, t2)

  let orelse t1 t2 =
    match (t1, t2) with
    | (_, True) | (True, _) -> True
    | (other, False) | (False, other) -> other
    | _ -> Or (t1, t2)

  let if_ a b c =
    match a with
    | True -> b
    | False -> c
    | _ ->
      match (b, c) with
      | (True, _ ) -> orelse        a  c
      | (_, False) -> andalso       a  b
      | (_, True ) -> orelse  (not_ a) b
      | (False, _) -> andalso (not_ a) c
      | _ -> If (a, b, c)

end
include T

module Stable = struct
  module V1 : sig
    (* THIS TYPE AND ITS SERIALIZATIONS SHOULD NEVER BE CHANGED - PLEASE SPEAK WITH
       ANOTHER DEVELOPER IF YOU NEED MORE DETAIL *)
    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a
    [@@deriving bin_io, compare, hash, sexp]

    (* the remainder of this signature consists of functions used in the definitions
       of sexp conversions that are also useful more generally *)

    val and_ : 'a t list -> 'a t
    val or_  : 'a t list -> 'a t

    val gather_conjuncts : 'a t -> 'a t list
    val gather_disjuncts : 'a t -> 'a t list

  end = struct

    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a

    include (T : sig type 'a t [@@deriving bin_io, compare, hash] end with type 'a t := 'a t)

    type sexp = Sexp.t = Atom of string | List of sexp list (* cheap import *)

    (* flatten out nested and's *)
    let gather_conjuncts t =
      let rec loop acc = function
        | True         :: ts -> loop acc ts
        | And (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t            :: ts -> loop (t :: acc) ts
        | []                 -> List.rev acc
      in
      loop [] [t]

    (* flatten out nested or's *)
    let gather_disjuncts t =
      let rec loop acc = function
        | False       :: ts -> loop acc ts
        | Or (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t           :: ts -> loop (t :: acc) ts
        | []                -> List.rev acc
      in
      loop [] [t]

    let and_ ts =
      let rec loop acc = function
        | [] -> acc
        | False :: _ -> false_ (* short circuit evaluation *)
        | t :: ts -> loop (andalso acc t) ts
      in
      loop true_ ts

    let or_ ts =
      let rec loop acc = function
        | [] -> acc
        | True :: _ -> true_ (* short circuit evaluation *)
        | t :: ts -> loop (orelse acc t) ts
      in
      loop false_ ts

    let unary name args sexp =
      match args with
      | [x] -> x
      | _ ->
        let n = List.length args in
        of_sexp_error (sprintf "%s expects one argument, %d found" name n) sexp

    let ternary name args sexp =
      match args with
      | [x; y; z] -> (x, y, z)
      | _ ->
        let n = List.length args in
        of_sexp_error (sprintf "%s expects three arguments, %d found" name n) sexp

    let sexp_of_t sexp_of_value t =
      let rec aux t =
        match t with
        | Base x          -> sexp_of_value x
        | True            -> Atom "true"
        | False           -> Atom "false"
        | Not t           -> List [Atom "not"; aux t]
        | If (t1, t2, t3) -> List [Atom "if"; aux t1; aux t2; aux t3]
        | And _ as t ->
          let ts = gather_conjuncts t in List (Atom "and" :: List.map ~f:aux ts)
        | Or _ as t ->
          let ts = gather_disjuncts t in List (Atom "or" :: List.map ~f:aux ts)
      in
      aux t

    let t_of_sexp base_of_sexp sexp =
      let base sexp = base (base_of_sexp sexp) in
      let rec aux sexp =
        match sexp with
        | Atom kw ->
          begin
            match String.lowercase kw with
            | "true"  -> true_
            | "false" -> false_
            | _       -> base sexp
          end
        | List (Atom kw :: args) ->
          begin
            match String.lowercase kw with
            | "and" -> and_ (List.map ~f:aux args)
            | "or"  -> or_  (List.map ~f:aux args)
            | "not" -> not_ (aux (unary "not" args sexp))
            | "if"  ->
              let (x, y, z) = ternary "if" args sexp in
              if_ (aux x) (aux y) (aux z)
            | _ -> base sexp
          end
        | _ -> base sexp
      in
      aux sexp
  end

  let%test_module "Blang.V1" =
    (module Stable_unit_test.Make (struct
         type t = string V1.t [@@deriving sexp, bin_io]

         open V1

         let equal = Pervasives.(=)

         let test_blang =
           (if_ (base "foo")
              (not_ (or_  [(base "bara"); (base "barb")]))
              (not_ (and_ [(base "baza"); (base "bazb")])))

         let test_sexp = "(if foo (not (or bara barb)) (not (and baza bazb)))"
         let test_bin =
           "\005\006\003foo\
            \004\003\006\004bara\006\004barb\
            \004\002\006\004baza\006\004bazb"

         let tests =
           [ test_blang, test_sexp, test_bin
           ; true_, "true", "\000"
           ; false_, "false", "\001"
           ]
       end))
end

include (Stable.V1 : module type of Stable.V1 with type 'a t := 'a t)

let%test_module "auto-simplification" =
  (module struct

    let (a, b, c) = (base 1, base 2, base 3)

    let (=) a b = invariant a; invariant b; Pervasives.(=) a b

    let%test _ = not_ true_ = false_
    let%test _ = not_ false_ = true_
    let%test _ = not_ (not_ a) = a

    let%test _ = andalso true_ b = b
    let%test _ = andalso a true_ = a
    let%test _ = andalso false_ b = false_
    let%test _ = andalso a false_ = false_

    let%test _ = orelse false_ b = b
    let%test _ = orelse a false_ = a
    let%test _ = orelse true_ b  = true_
    let%test _ = orelse a true_  = true_

    let%test _ = if_ true_ b c = b
    let%test _ = if_ false_ b c = c
    let%test _ = if_ a true_ c = orelse a c
    let%test _ = if_ a b false_ = andalso a b
    let%test _ = if_ a b true_ = if_ (not_ a) true_ b  (* b/c (if a b c) = (if (not a) c b) *)
    let%test _ = if_ a b true_ = orelse (not_ a) b
    let%test _ = if_ a false_ c = if_ (not_ a) c false_  (* b/c (if a b c) = (if (not a) c b) *)
    let%test _ = if_ a false_ c = andalso (not_ a) c

    let%test_module "n-ary-and-or" =
      (module struct

        let%test _ = and_ [a; b; c] = andalso (andalso a b) c
        let%test _ = or_ [a; b; c]  = orelse (orelse a b) c

        let test_and ts = (and_ ts = List.fold ts ~init:true_ ~f:andalso)
        let test_or  ts = (or_  ts = List.fold ts ~init:false_ ~f:orelse)

        let%test _ = test_or []
        let%test _ = test_or [a]
        let%test _ = test_or [true_]
        let%test _ = test_or [false_]
        let%test _ = test_or [a; true_; b]
        let%test _ = test_or [a; false_; b]

        let%test _ = test_and []
        let%test _ = test_and [a]
        let%test _ = test_and [true_]
        let%test _ = test_and [false_]
        let%test _ = test_and [a; true_; b]
        let%test _ = test_and [a; false_; b]

      end)

  end)

let constant b = if b then true_ else false_

module type Constructors = sig
  val base     : 'a -> 'a t
  val true_    : _ t
  val false_   : _ t
  val constant : bool -> _ t
  val not_     : 'a t -> 'a t
  val and_     : 'a t list -> 'a t
  val or_      : 'a t list -> 'a t
  val if_      : 'a t -> 'a t -> 'a t -> 'a t
end

module O = struct
  include T
  let not = not_
  let and_ = and_
  let or_ = or_
  let constant = constant
  let (&&) = andalso
  let (||) = orelse
  let (==>) a b = not a || b
end

let constant_value = function
  | True -> Some true
  | False -> Some false
  | _ -> None

(* [values t] lists the base predicates in [t] from left to right *)
let values t =
  let rec loop acc = function
    | Base v          :: ts -> loop (v :: acc) ts
    | True            :: ts -> loop acc ts
    | False           :: ts -> loop acc ts
    | Not t1          :: ts -> loop acc (t1 :: ts)
    | And (t1, t2)    :: ts -> loop acc (t1 :: t2 :: ts)
    | Or (t1, t2)     :: ts -> loop acc (t1 :: t2 :: ts)
    | If (t1, t2, t3) :: ts -> loop acc (t1 :: t2 :: t3 :: ts)
    | []                    -> List.rev acc
  in
  loop [] [t]

let%test _ = [1; 2; 3; 4; 5; 6; 7] =
             values
               (and_ [
                  or_ [base 1; base 2];
                  base 3;
                  true_;
                  if_ (base 4) (base 5) (base 6);
                  not_ (base 7);
                ])

let%test _ = gather_conjuncts (base 1) = [base 1]
let%test _ = gather_conjuncts (and_ []) = []
let%test _ = gather_conjuncts (and_ [base 1]) = [base 1]
let%test _ = gather_conjuncts (and_ [base 1; base 2]) = [base 1; base 2]
let%test _ = gather_conjuncts (and_ [base 1; base 2; base 3]) = [base 1; base 2; base 3]
let%test _ =
  gather_conjuncts
    (and_ [
       and_ [and_ [base 1; base 2]; base 3];
       and_ [or_ [base 4; base 5]; and_ [base 6; base 7]];
     ])
  =
  [base 1; base 2; base 3; or_ [base 4; base 5]; base 6; base 7]

let%test _ = gather_disjuncts (base 1) = [base 1]
let%test _ = gather_disjuncts (or_ []) = []
let%test _ = gather_disjuncts (or_ [base 1]) = [base 1]
let%test _ = gather_disjuncts (or_ [base 1; base 2]) = [base 1; base 2]
let%test _ = gather_disjuncts (or_ [base 1; base 2; base 3]) = [base 1; base 2; base 3]
let%test _ =
  gather_disjuncts
    (or_ [
       or_ [or_ [base 1; base 2]; base 3];
       or_ [and_ [base 4; base 5]; or_ [base 6; base 7]];
     ])
  =
  [base 1; base 2; base 3; and_ [base 4; base 5]; base 6; base 7]

module C = Container.Make (struct
    type 'a t = 'a T.t
    let fold t ~init ~f =
      let rec loop acc t pending =
        match t with
        | Base a -> next (f acc a) pending
        | True | False -> next acc pending
        | Not t -> loop acc t pending
        | And (t1, t2) | Or (t1, t2) -> loop acc t1 (t2 :: pending)
        | If (t1, t2, t3) -> loop acc t1 (t2 :: t3 :: pending)
      and next acc = function
        | [] -> acc
        | t :: ts -> loop acc t ts
      in
      loop init t []

    let iter = `Define_using_fold
  end)

let count    = C.count
let sum      = C.sum
let exists   = C.exists
let find     = C.find
let find_map = C.find_map
let fold     = C.fold
let for_all  = C.for_all
let is_empty = C.is_empty
let iter     = C.iter
let length   = C.length
let mem      = C.mem
let to_array = C.to_array
let to_list  = C.to_list
let min_elt  = C.min_elt
let max_elt  = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until

include Monad.Make (struct

    type 'a t = 'a T.t

    let return = base

    let rec bind t ~f:k =
      match t with
      | Base v -> k v
      | True -> true_
      | False -> false_
      | Not t1 -> not_ (bind t1 ~f:k)
      (* Unfortunately we need to duplicate some of the short-circuiting from [andalso] and
         friends here. In principle we could do something involving [Lazy.t] but the
         overhead probably wouldn't be worth it. *)
      | And (t1, t2) ->
        begin match bind t1 ~f:k with
        | False -> false_
        | other -> andalso other (bind t2 ~f:k)
        end
      | Or (t1, t2) ->
        begin match bind t1 ~f:k with
        | True -> true_
        | other -> orelse other (bind t2 ~f:k)
        end
      | If (t1, t2, t3) ->
        begin match bind t1 ~f:k with
        | True -> bind t2 ~f:k
        | False -> bind t3 ~f:k
        | other -> if_ other (bind t2 ~f:k) (bind t3 ~f:k)
        end
    ;;

    let map = `Define_using_bind

  end)

let%test_module "bind short-circuiting" =
  (module struct
    let test expected_visits expr =
      let visited = ref [] in
      let f var =
        visited := var :: !visited;
        false_
      in
      match bind expr ~f with
      | True -> List.equal ~equal:Int.equal expected_visits (List.rev !visited)
      | _ -> false

    let%test _ = test [0] (or_ [not_ (base 0); base 1])
    let%test _ = test [0; 1] (not_ (and_ [not_ (base 0); base 1; base 2]))
    let%test _ = test [0; 2] (if_ (base 0) (base 1) (not_ (base 2)))
  end)

(* semantics *)

let eval t base_eval =
  let rec eval = function
    | True -> true
    | False -> false
    | And (t1, t2) -> eval t1 && eval t2
    | Or (t1, t2) -> eval t1 || eval t2
    | Not t -> not (eval t)
    | If (t1, t2, t3) -> if eval t1 then eval t2 else eval t3
    | Base x -> base_eval x
  in
  eval t

let specialize t f =
  bind t ~f:(fun v ->
    match f v with
    | `Known c -> constant c
    | `Unknown -> base v)

let eval_set ~universe:all set_of_base =
  let rec aux (b : _ t) =
    match b with
    | True       -> force all
    | False      -> Set.Using_comparator.empty ~comparator:(Set.comparator (force all))
    | And (a, b) -> Set.inter (aux a)     (aux b)
    | Or (a, b)  -> Set.union (aux a)     (aux b)
    | Not a      -> Set.diff  (force all) (aux a)
    | Base a     -> set_of_base a
    | If (cond, a, b) ->
      let cond = aux cond in
      Set.union
        (Set.inter cond (aux a))
        (Set.inter (Set.diff (force all) cond) (aux b))
  in
  aux
;;

let%test_module "laws" =
  (module struct

    type base = A | B | C [@@deriving sexp_of]

    type 'a base_fun = base -> 'a

    let sexp_of_base_fun sexp_of_a (f : 'a base_fun) =
      Sexp.List [
        Sexp.Atom "function";
        Sexp.List [Sexp.Atom "A"; Sexp.Atom "->"; sexp_of_a (f A)];
        Sexp.List [Sexp.Atom "B"; Sexp.Atom "->"; sexp_of_a (f B)];
        Sexp.List [Sexp.Atom "C"; Sexp.Atom "->"; sexp_of_a (f C)];
      ]

    module Gen = struct

      (* all random values are generated from a fixed PRNG seed so that
         unit tests are deterministic *)
      let prng =
        Random.State.make
          (String.to_list "31bb128c352e2569228fbacc590e937a29a8bb8f\
                           c4bfe7126504ce3dc400be7f401fa6f5be5dba38"
           |> Array.of_list
           |> Array.map ~f:Char.to_int)

      let bool () = Random.State.bool prng

      let element arr = arr.(Random.State.int prng (Array.length arr))

      let gen_blang gen_base =
        let atomic =
          [| (fun () -> constant (bool ()));
             (fun () -> base (gen_base ())); |]
        in
        let composite =
          [| (fun rand -> not_ (rand ()));
             (fun rand -> andalso (rand ()) (rand ()));
             (fun rand -> orelse (rand ()) (rand ()));
             (fun rand -> if_ (rand ()) (rand ()) (rand ())); |]
        in
        let rec aux ~depth =
          if depth <= 1 then
            element atomic ()
          else
            element composite (fun () -> aux ~depth:(depth - 1))
        in
        aux

      let gen_base =
        let bases = [| A; B; C |] in
        fun () -> element bases

      let gen_base_fun codomain =
        fun () ->
          let a_val = element codomain in
          let b_val = element codomain in
          let c_val = element codomain in
          function
          | A -> a_val
          | B -> b_val
          | C -> c_val

      let t () = gen_blang gen_base ~depth:5

      let f = gen_base_fun [| true; false |]
      let g = gen_base_fun [| `Unknown; `Known true; `Known false |]

      let tf () = (t (), f ())
      let tg () = (t (), g ())
    end

    let law gen sexp_of run =
      for _ = 0 to 100 do
        let arg = gen () in
        if not (run arg)
        then failwith (Sexp.to_string (sexp_of arg))
      done

    let forall_t  = law Gen.t  [%sexp_of: base t]
    let forall_tf = law Gen.tf [%sexp_of: base t * bool base_fun]
    let forall_tg = law Gen.tg [%sexp_of: base t * [`Known of bool | `Unknown] base_fun]

    let%test_unit _ =
      forall_t (fun t ->
        specialize t (fun _ -> `Unknown) = t)

    let%test_unit _ =
      forall_tf (fun (t, f) ->
        specialize t (fun x -> `Known (f x)) = constant (eval t f))

    let%test_unit _ =
      forall_tg (fun (t, g) ->
        List.for_all (values (specialize t g)) ~f:(fun x -> g x = `Unknown))

    let%test_unit _ =
      forall_tg (fun (t, g) ->
        (* an arbitrary [f] such that [f x = b] whenever [g x = `Known b] *)
        let f =
          let rand_fval x = match g x with `Known b -> b | `Unknown -> Gen.bool () in
          let a_val = rand_fval A in
          let b_val = rand_fval B in
          let c_val = rand_fval C in
          function
          | A -> a_val
          | B -> b_val
          | C -> c_val
        in
        eval t f = eval (specialize t g) f)

    let%test_module "eval_set" =
      (module struct

        type base_set =
          | Odd
          | Even
          | Greater_than of int
          | Smaller_than of int
        [@@deriving sexp_of]

        let size = 10
        let universe = lazy (List.init size ~f:Fn.id |> Int.Set.of_list)

        let gen_base =
          let bases =
            [| Odd; Even; Greater_than (size / 2); Smaller_than (size / 2) |]
          in
          fun () -> Gen.element bases
        ;;

        let t () = Gen.gen_blang gen_base ~depth:5

        let set_of_base = Memo.general (fun t ->
          Int.Set.filter (force universe) ~f:(fun e ->
            match t with
            | Odd            -> e mod 2 = 1
            | Even           -> e mod 2 = 0
            | Greater_than x -> e > x
            | Smaller_than x -> e < x))
        ;;

        let run expression =
          let expect =
            Set.filter (force universe) ~f:(fun e ->
              eval expression (fun base -> Int.Set.mem (set_of_base base) e))
          in
          try
            [%test_result: Int.Set.t] ~expect
              (eval_set ~universe set_of_base expression)
          with
          | exn -> failwiths "fail on expression" (expression, exn)
                     [%sexp_of: base_set t * Exn.t]
        ;;

        let%test_unit _ =
          for _ = 0 to 100 do
            run (t ())
          done;
        ;;
      end)

  end)
