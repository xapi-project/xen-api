open! Import

open Quickcheck_intf

module Array      = Base.Array
module Bool       = Base.Bool
module Char       = Base.Char
module Int        = Base.Int
module List       = Base.List
module Type_equal = Base.Type_equal

module Pre_float : Comparisons.S with type t = float = struct
  type t = float

  open Pervasives

  let compare (x : t) (y : t) = compare x y
  let equal   (x : t) (y : t) = compare x y = 0
  let min     (x : t) (y : t) = min     x y
  let max     (x : t) (y : t) = max     x y

  let ( >= ) (x : t) (y : t) = (x >= y)
  let ( <= ) (x : t) (y : t) = (x <= y)
  let ( =  ) (x : t) (y : t) = (x =  y)
  let ( >  ) (x : t) (y : t) = (x >  y)
  let ( <  ) (x : t) (y : t) = (x <  y)
  let ( <> ) (x : t) (y : t) = (x <> y)
end

module Pre_int : Pre_int with type t = int = struct
  include Base.Int
  let splittable_random = Splittable_random.int
end

let check_size str size =
  if size < 0 then Error.raise_s [%message str "size is negative" (size : int)]

let bounds_error name lower_bound upper_bound sexp_of_bound =
  raise_s [%message
    name
      "invalid bounds"
      (lower_bound : bound)
      (upper_bound : bound)]

module Make_int_random (M : Pre_int) : sig
  open M

  val uniform_incl     : Splittable_random.State.t -> lo:t -> hi:t -> t
  val log_uniform_incl : Splittable_random.State.t -> lo:t -> hi:t -> t

  module For_testing : sig
    val bits_to_represent         : t -> int
    val min_represented_by_n_bits : int -> t
    val max_represented_by_n_bits : int -> t
  end
end = struct
  open M

  module For_testing = struct
    let bits_to_represent t =
      assert (t >= zero);
      let t = ref t in
      let n = ref 0 in
      while !t > zero do
        t := shift_right !t 1;
        Int.incr n;
      done;
      !n

    let min_represented_by_n_bits n =
      if Int.equal n 0
      then zero
      else shift_left one (Int.pred n)

    let max_represented_by_n_bits n =
      pred (shift_left one n)
  end

  include For_testing

  let uniform_incl = splittable_random

  let log_uniform_incl state ~lo ~hi =
    let min_bits = bits_to_represent lo in
    let max_bits = bits_to_represent hi in
    let bits = Pre_int.splittable_random state ~lo:min_bits ~hi:max_bits in
    uniform_incl state
      ~lo:(min_represented_by_n_bits bits |> max lo)
      ~hi:(max_represented_by_n_bits bits |> min hi)
end

module Int_random = Make_int_random (Pre_int)

let%test_module "Make_int_random bitwise helpers" =
  (module struct
    open Int_random
    open For_testing

    let%test_unit "bits_to_represent" =
      let test n expect = [%test_result: int] (bits_to_represent n) ~expect in
      test 0 0;
      test 1 1;
      test 2 2;
      test 3 2;
      test 4 3;
      test 5 3;
      test 6 3;
      test 7 3;
      test 8 4;
      test 100 7;
      test Int.max_value (Int.pred Int.num_bits);
    ;;

    let%test_unit "min_represented_by_n_bits" =
      let test n expect = [%test_result: int] (min_represented_by_n_bits n) ~expect in
      test 0 0;
      test 1 1;
      test 2 2;
      test 3 4;
      test 4 8;
      test 7 64;
      test (Int.pred Int.num_bits) (Int.shift_right_logical Int.min_value 1);
    ;;

    let%test_unit "max_represented_by_n_bits" =
      let test n expect = [%test_result: int] (max_represented_by_n_bits n) ~expect in
      test 0 0;
      test 1 1;
      test 2 3;
      test 3 7;
      test 4 15;
      test 7 127;
      test (Int.pred Int.num_bits) Int.max_value;
    ;;

  end)

module Raw_generator : sig
  type +'a t

  val create   :         (size:int -> Splittable_random.State.t -> 'a) -> 'a t
  val generate : 'a t -> (size:int -> Splittable_random.State.t -> 'a)

  (* [sizes_for_elements] is a helper function for distributing [size] among an arbitrary
     number of elements.  Some of [size] is "spent" on extending the length of the
     collection (raising it from [min_len] to a maximum of [max_len]); the remainder is
     "spent" on the size of individual elements.  The implementation is designed not to
     bias the sizes toward any particular element.  The result is an array of the
     appropriate length, where each array element is the size of one corresponding element
     of the desired collection. *)
  val sizes_for_elements
    :  ?min_len : int
    -> ?max_len : int
    -> size     : int
    -> Splittable_random.State.t
    -> int array
end = struct
  type 'a t = size:int -> Splittable_random.State.t -> 'a

  let create = Fn.id

  let generate t ~size random =
    check_size "Quickcheck.Generator.generate" size;
    t ~size random

  let sizes_for_elements ?(min_len = 0) ?(max_len = Pre_int.max_value) ~size random =
    assert (min_len <= max_len);
    let upper_bound = min_len + size in
    let max_len =
      if upper_bound >= min_len (* guard against overflow *)
      then min max_len upper_bound
      else max_len
    in
    (* pick a length, weighted low so that most of the size is spent on elements *)
    let len = Int_random.log_uniform_incl random ~lo:min_len ~hi:max_len in
    (* if there are no elements return an empty array, otherwise return a non-empty array
       with the size distributed among the elements *)
    if len = 0 then [||] else begin
      let sizes = Array.init len ~f:(fun _ -> 0) in
      let remaining = size - (len - min_len) in
      let max_index = len - 1 in
      for _ = 1 to remaining do
        (* pick an index, weighted low so that we see unbalanced distributions often *)
        let index = Int_random.log_uniform_incl random ~lo:0 ~hi:max_index in
        sizes.(index) <- sizes.(index) + 1
      done;
      (* permute the array so that no index is favored over another *)
      for i = 0 to max_index - 1 do
        let j = Splittable_random.int random ~lo:i ~hi:max_index in
        Array.swap sizes i j
      done;
      assert (Array.sum (module Int) sizes ~f:Fn.id + (len - min_len) = size);
      sizes
    end
end

type 'a gen = 'a Raw_generator.t

module Raw_observer : sig
  type -'a t

  val create  :         ('a -> size:int -> Hash.state -> Hash.state) -> 'a t
  val observe : 'a t -> ('a -> size:int -> Hash.state -> Hash.state)
end = struct
  type 'a t = 'a -> size:int -> Hash.state -> Hash.state

  let create = Fn.id

  let observe t x ~size hash =
    check_size "Quickcheck.Observer.observe" size;
    t x ~size hash
end

type 'a obs = 'a Raw_observer.t

module Observer = struct

  include Raw_observer

  let of_hash (type a) (module M : Deriving_hash with type t = a) =
    create (fun x ~size:_ hash ->
      [%hash_fold: M.t] hash x)

  let bool = of_hash (module Bool)
  let char = of_hash (module Char)

  let variant2 t1 t2 =
    create (fun x ~size hash ->
      match x with
      | `A y -> observe t1 y ~size ([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ([%hash_fold: int] hash 2))

  let variant3 t1 t2 t3 =
    create (fun x ~size hash ->
      match x with
      | `A y -> observe t1 y ~size ([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ([%hash_fold: int] hash 3))

  let variant4 t1 t2 t3 t4 =
    create (fun x ~size hash ->
      match x with
      | `A y -> observe t1 y ~size ([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ([%hash_fold: int] hash 4))

  let variant5 t1 t2 t3 t4 t5 =
    create (fun x ~size hash ->
      match x with
      | `A y -> observe t1 y ~size ([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ([%hash_fold: int] hash 4)
      | `E y -> observe t5 y ~size ([%hash_fold: int] hash 5))

  let variant6 t1 t2 t3 t4 t5 t6 =
    create (fun x ~size hash ->
      match x with
      | `A y -> observe t1 y ~size ([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ([%hash_fold: int] hash 4)
      | `E y -> observe t5 y ~size ([%hash_fold: int] hash 5)
      | `F y -> observe t6 y ~size ([%hash_fold: int] hash 6))

  let tuple2 t1 t2 =
    create (fun (x1,x2) ~size hash ->
      hash
      |> observe t1 x1 ~size
      |> observe t2 x2 ~size)

  let tuple3 t1 t2 t3 =
    create (fun (x1,x2,x3) ~size hash ->
      hash
      |> observe t1 x1 ~size
      |> observe t2 x2 ~size
      |> observe t3 x3 ~size)

  let tuple4 t1 t2 t3 t4 =
    create (fun (x1,x2,x3,x4) ~size hash ->
      hash
      |> observe t1 x1 ~size
      |> observe t2 x2 ~size
      |> observe t3 x3 ~size
      |> observe t4 x4 ~size)

  let tuple5 t1 t2 t3 t4 t5 =
    create (fun (x1,x2,x3,x4,x5) ~size hash ->
      hash
      |> observe t1 x1 ~size
      |> observe t2 x2 ~size
      |> observe t3 x3 ~size
      |> observe t4 x4 ~size
      |> observe t5 x5 ~size)

  let tuple6 t1 t2 t3 t4 t5 t6 =
    create (fun (x1,x2,x3,x4,x5,x6) ~size hash ->
      hash
      |> observe t1 x1 ~size
      |> observe t2 x2 ~size
      |> observe t3 x3 ~size
      |> observe t4 x4 ~size
      |> observe t5 x5 ~size
      |> observe t6 x6 ~size)

  let unmap t ~f =
    create (fun x ~size hash ->
      observe t (f x) ~size hash)

  let of_predicate a b ~f =
    unmap (variant2 a b)
      ~f:(fun x -> if f x then `A x else `B x)

  let singleton () =
    create (fun _ ~size:_ hash -> hash)

  let doubleton f =
    of_predicate (singleton ()) (singleton ()) ~f

  module Make_int_observer (M : Pre_int) : sig
    val obs : M.t t
  end = struct
    let obs =
      create (fun x ~size:_ hash ->
        [%hash_fold: M.t] hash x)
  end

  module For_int = Make_int_observer (Pre_int)

  let enum _ ~f =
    unmap For_int.obs ~f

  let of_list list ~equal =
    let f x =
      match List.findi list ~f:(fun _ y -> equal x y) with
      | None        -> failwith "Quickcheck.Observer.of_list: value not found"
      | Some (i, _) -> i
    in
    enum (List.length list) ~f

  let of_fun f =
    create (fun x ~size hash ->
      observe (f ()) x ~size hash)

  let fixed_point f =
    let rec self () = f (of_fun self) in
    of_fun self

  let recursive = fixed_point

  let comparison ~compare ~eq ~lt ~gt =
    unmap (variant3 lt (singleton ()) gt) ~f:(fun x ->
      let c = compare x eq in
      if c < 0 then `A x else
      if c > 0 then `C x else
        `B x)

  let fn dom rng =
    create (fun f ~size hash ->
      let random = Splittable_random.State.of_int (Hash.get_hash_value hash) in
      let sizes = Raw_generator.sizes_for_elements ~size random in
      Array.fold sizes ~init:hash ~f:(fun hash size ->
        let x = Raw_generator.generate dom ~size random in
        observe rng (f x) ~size hash))
end

module Generator = struct

  include Raw_generator

  let return x =
    create (fun ~size:_ _ -> x)

  let custom_map t ~f =
    create (fun ~size random ->
      let x = generate t ~size random in
      f x)

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return = return

      let bind t1 ~f =
        create (fun ~size random ->
          let x = generate t1 ~size random in
          let t2 = f x in
          generate t2 ~size random)

      let map = `Custom custom_map
    end)

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return = return

      let apply t1 t2 =
        create (fun ~size random ->
          let f = generate t1 ~size random in
          let x = generate t2 ~size random in
          f x)

      let map = `Custom custom_map
    end)

  open Let_syntax

  let size = create (fun ~size _ -> size)

  let with_size t ~size =
    create (fun ~size:_ random ->
      generate t ~size random)

  let singleton = return

  let filter_map t ~f =
    let rec loop ~size random =
      let x = generate t ~size random in
      match f x with
      | Some y -> y
      | None   -> loop ~size:(size + 1) random
    in
    create loop

  let filter t ~f =
    filter_map t ~f:(fun x ->
      if f x
      then Some x
      else None)

  module Binary_search_array = Binary_searchable.Make1 (struct
      type 'a t = 'a array
      let get    = Array.get
      let length = Array.length
    end)

  let weighted_choice alist_by_weight =
    if List.is_empty alist_by_weight then
      Error.raise_s [%message "Quickcheck.Generator.weighted_union: empty list"];
    let weights, values = List.unzip alist_by_weight in
    let value_array = Array.of_list values in
    let total_weight, cumulative_weight_array =
      let array = Array.init (Array.length value_array) ~f:(fun _ -> 0.) in
      let sum =
        List.foldi weights ~init:0. ~f:(fun index acc weight ->
          let cumulative = acc +. weight in
          Array.set array index cumulative;
          cumulative)
      in
      sum, array
    in
    create (fun ~size:_ random ->
      let choice = Splittable_random.float random ~lo:0. ~hi:total_weight in
      match
        Binary_search_array.binary_search
          cumulative_weight_array
          ~compare:Pre_float.compare
          `First_greater_than_or_equal_to
          choice
      with
      | Some index -> Array.get value_array index
      | None       -> assert false)

  let weighted_union alist =
    weighted_choice alist
    |> join

  let doubleton x y =
    create (fun ~size:_ random ->
      if Splittable_random.bool random
      then x
      else y)

  let bool = doubleton true false

  let of_fun f =
    create (fun ~size random ->
      generate (f ()) ~size random)

  let of_sequence ~p seq =
    if Pervasives.( <= ) p 0. || Pervasives.( > ) p 1. then
      failwith (Printf.sprintf "Generator.of_sequence: probability [%f] out of bounds" p);
    Sequence.delayed_fold seq
      ~init:()
      ~finish:(fun () -> failwith "Generator.of_sequence: ran out of values")
      ~f:(fun () x ~k ->
        weighted_union
          [       p, singleton x
          ; 1. -. p, of_fun k
          ])

  let rec bounded_geometric ~p ~maximum init =
    if init = maximum
    then singleton maximum
    else
      weighted_union
        [       p, singleton init
        ; 1. -. p, of_fun (fun () -> bounded_geometric ~p ~maximum (init + 1))
        ]

  let geometric ~p init =
    bounded_geometric ~p ~maximum:Pre_int.max_value init

  let small_non_negative_int =
    create (fun ~size random ->
      Splittable_random.int random ~lo:0 ~hi:size)

  let small_positive_int =
    create (fun ~size random ->
      Splittable_random.int random ~lo:1 ~hi:(size + 1))

  module Make_int_generator (M : Pre_int) : sig
    val gen                  :               M.t t
    val gen_incl             : M.t -> M.t -> M.t t
    val gen_uniform_incl     : M.t -> M.t -> M.t t
    val gen_log_incl         : M.t -> M.t -> M.t t
    val gen_log_uniform_incl : M.t -> M.t -> M.t t
  end = struct
    open M

    module Random = Make_int_random (M)

    let gen_uniform_incl lo hi =
      if lo > hi then begin
        bounds_error "Quickcheck.Make_int().gen_uniform_incl" lo hi [%sexp_of: t]
      end;
      create (fun ~size:_ random ->
        Random.uniform_incl random ~lo ~hi)

    let gen_log_uniform_incl lo hi =
      if lo < zero || lo > hi then begin
        bounds_error "Quickcheck.Make_int().gen_log_uniform_incl" lo hi [%sexp_of: t]
      end;
      create (fun ~size:_ random ->
        Random.log_uniform_incl random ~lo ~hi)

    let gen_incl lower_bound upper_bound =
      weighted_union
        [ 0.05, return lower_bound
        ; 0.05, return upper_bound
        ; 0.9,  gen_uniform_incl lower_bound upper_bound
        ]

    let gen_log_incl lower_bound upper_bound =
      weighted_union
        [ 0.05, return lower_bound
        ; 0.05, return upper_bound
        ; 0.9,  gen_log_uniform_incl lower_bound upper_bound
        ]

    let gen =
      let%map sign = doubleton true false
      and     bits = gen_log_incl zero max_value
      in
      if sign then bit_not bits else bits

  end

  module For_int = Make_int_generator (Pre_int)

  let of_list list =
    let array = Array.of_list list in
    map (For_int.gen_uniform_incl 0 (Array.length array - 1)) ~f:(fun index ->
      array.(index))

  let union list = of_list list |> join

  let fixed_point f =
    let rec r ~size random =
      generate (force lazy_t) ~size random
    and lazy_t = lazy (f (create r))
    in
    force lazy_t

  let recursive = fixed_point

  (* The resulting generator will raise if used at size 0. *)
  let with_decremented_size_exn t =
    let%bind n = size in
    with_size t ~size:(n - 1)

  let weighted_recursive_union nonrec_list ~f =
    fixed_point (fun self ->
      let nonrec_gen = weighted_union nonrec_list in
      let rec_gen =
        let rec_list =
          List.map (f self) ~f:(fun (w, t) ->
            (* This generator is never used at size 0, so this use of
               [with_decremented_size_exn] is safe. *)
            (w, with_decremented_size_exn t))
        in
        weighted_union (nonrec_list @ rec_list)
      in
      match%bind size with
      | 0 -> nonrec_gen
      | _ -> rec_gen)

  let recursive_union nonrec_list ~f =
    let weighted list = List.map list ~f:(fun t -> (1., t)) in
    weighted_recursive_union
      (weighted nonrec_list)
      ~f:(fun self -> weighted (f self))

  let variant2 a b =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ]

  let variant3 a b c =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ]

  let variant4 a b c d =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ]

  let variant5 a b c d e =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ; map e ~f:(fun e -> `E e)
          ]

  let variant6 a b c d e f =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ; map e ~f:(fun e -> `E e)
          ; map f ~f:(fun f -> `F f)
          ]

  let tuple2 t1 t2 =
    t1 >>= fun x1 ->
    t2 >>| fun x2 ->
    (x1, x2)

  let tuple3 t1 t2 t3 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>| fun x3 ->
    (x1, x2, x3)

  let tuple4 t1 t2 t3 t4 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>| fun x4 ->
    (x1, x2, x3, x4)

  let tuple5 t1 t2 t3 t4 t5 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>= fun x4 ->
    t5 >>| fun x5 ->
    (x1, x2, x3, x4, x5)

  let tuple6 t1 t2 t3 t4 t5 t6 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>= fun x4 ->
    t5 >>= fun x5 ->
    t6 >>| fun x6 ->
    (x1, x2, x3, x4, x5, x6)

  let list_gen ?min_len ?max_len elem_gen =
    create (fun ~size random ->
      let sizes = sizes_for_elements ?min_len ?max_len ~size random in
      List.init (Array.length sizes) ~f:(fun i ->
        generate elem_gen ~size:sizes.(i) random))

  let list                 elem_gen = list_gen elem_gen
  let list_non_empty       elem_gen = list_gen elem_gen ~min_len:1
  let list_with_length len elem_gen = list_gen elem_gen ~min_len:len ~max_len:len

  (* [fn] generates random functions that produce randomly assigned results for each
     input, but which produce identical results for identical inputs.  Each random
     function is assigned a fresh, independent PRNG state using [Splittable_random.split].
     On each input, the PRNG state is copied so the original state is not modified and can
     be reused.  The input is hashed using the given domain observer, and that hash is
     used to seed the PRNG state using [Splittable_random.perturb] so that different
     inputs yield different outputs.  The PRNG state is then used to generate a random
     output. *)
  let fn dom rng =
    create (fun ~size random ->
      let random = Splittable_random.State.split random in
      (fun x ->
         let hash = Observer.observe dom x ~size (Hash.alloc ()) in
         let random = Splittable_random.State.copy random in
         Splittable_random.State.perturb random (Hash.get_hash_value hash);
         generate rng ~size random))

  let fn2 dom1 dom2 rng =
    fn (Observer.tuple2 dom1 dom2) rng
    >>| fun f ->
    (fun x1 x2 -> f (x1, x2))

  let fn3 dom1 dom2 dom3 rng =
    fn (Observer.tuple3 dom1 dom2 dom3) rng
    >>| fun f ->
    (fun x1 x2 x3 -> f (x1, x2, x3))

  let fn4 dom1 dom2 dom3 dom4 rng =
    fn (Observer.tuple4 dom1 dom2 dom3 dom4) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 -> f (x1, x2, x3, x4))

  let fn5 dom1 dom2 dom3 dom4 dom5 rng =
    fn (Observer.tuple5 dom1 dom2 dom3 dom4 dom5) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 x5 -> f (x1, x2, x3, x4, x5))

  let fn6 dom1 dom2 dom3 dom4 dom5 dom6 rng =
    fn (Observer.tuple6 dom1 dom2 dom3 dom4 dom5 dom6) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 x5 x6 -> f (x1, x2, x3, x4, x5, x6))

  let compare_fn dom =
    fn dom For_int.gen
    >>| fun get_index ->
    (fun x y -> [%compare: int] (get_index x) (get_index y))

  let equal_fn dom =
    compare_fn dom
    >>| fun cmp ->
    (fun x y -> Pervasives.( = ) (cmp x y) 0)

  let char_range lo hi =
    create (fun ~size:_ random ->
      Splittable_random.int random ~lo:(Char.to_int lo) ~hi:(Char.to_int hi)
      |> Char.unsafe_of_int)

  let char_uppercase     = char_range 'A' 'Z'
  let char_lowercase     = char_range 'a' 'z'
  let char_digit         = char_range '0' '9'
  let char_print_uniform = char_range ' ' '~'

  let char_uniform = char_range Char.min_value Char.max_value

  let char_alpha    = union [ char_lowercase ; char_uppercase ]
  let char_alphanum =
    weighted_union
      (* Most people probably expect this to be a uniform distribution, not weighted
         toward digits like we would get with [union] (since there are fewer digits than
         letters). *)
      [ 52., char_alpha
      ; 10., char_digit
      ]

  let char_whitespace = of_list (List.filter Char.all ~f:Char.is_whitespace)

  let char_print =
    weighted_union
      [ 10., char_alphanum
      ;  1., char_print_uniform
      ]

  let char =
    weighted_union
      [ 10., char_print
      ;  1., char_uniform
      ]
end

module Shrinker = struct

  type 'a t = ('a -> 'a Sequence.t) Staged.t

  let shrink t = Staged.unstage t

  let create t = Staged.stage t

  let empty () = create (fun _ -> Sequence.empty)

  let bool = empty ()
  let char = empty ()

  let map t ~f ~f_inverse =
    create (fun b ->
      shrink t (f_inverse b)
      |> Sequence.map ~f)

  module Test_data = struct
    let singleton equal min =
      create (fun v ->
        if equal min v
        then Sequence.empty
        else Sequence.singleton min)

    let%test_module "singleton" =
      (module struct
        let t = singleton Pervasives.(=) 42

        let%test_unit "singleton produces values" =
          let shrunk = shrink t 2 |> Sequence.to_list in
          let expect = [42] in
          [%test_result: int list ] ~expect shrunk

        let%test_unit "singleton doesn't produce the input" =
          let shrunk = shrink t 42 |> Sequence.to_list in
          let expect = [] in
          [%test_result: int list ] ~expect shrunk
      end)


    let t0 = singleton Pervasives.(=) 0
    let t1 = singleton Pervasives.(=) 1
    let t2 = singleton Pervasives.(=) 2
    let t3 = singleton Pervasives.(=) 3
    let t4 = singleton Pervasives.(=) 4
    let t5 = singleton Pervasives.(=) 5
  end

  let tuple2 t1 t2 =
    let shrinker (v1, v2) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x))  in
      Sequence.round_robin [v1_seq; v2_seq]
    in
    create shrinker

  let tuple3 t1 t2 t3 =
    let shrinker (v1, v2, v3) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq]
    in
    create shrinker

  let tuple4 t1 t2 t3 t4 =
    let shrinker (v1, v2, v3, v4) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq]
    in
    create shrinker

  let tuple5 t1 t2 t3 t4 t5 =
    let shrinker (v1, v2, v3, v4, v5) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4, v5)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4, v5)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4, v5)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x,  v5)) in
      let v5_seq = Sequence.map (shrink t5 v5) ~f:(fun x -> (v1, v2, v3, v4, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq]
    in
    create shrinker

  let tuple6 t1 t2 t3 t4 t5 t6 =
    let shrinker (v1, v2, v3, v4, v5, v6) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4, v5, v6)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4, v5, v6)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4, v5, v6)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x,  v5, v6)) in
      let v5_seq = Sequence.map (shrink t5 v5) ~f:(fun x -> (v1, v2, v3, v4, x,  v6)) in
      let v6_seq = Sequence.map (shrink t6 v6) ~f:(fun x -> (v1, v2, v3, v4, v5, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq; v6_seq]
    in
    create shrinker

  let%test_module "tuple shrinkers" =
    (module struct
      open Test_data

      let%test_unit "tuple2 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int ] in
        let expect =
          [(0,5); (5,1)]
          |> sort
        in
        let results =
          shrink (tuple2 t0 t1) (5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int * int) list ] ~expect results

      let%test_unit "tuple3 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int ] in
        let expect = [(0,5,5); (5,1,5); (5,5,2)] |> sort in
        let results =
          shrink (tuple3 t0 t1 t2) (5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int) list ] results ~expect

      let%test_unit "tuple4 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int ] in
        let expect =
          [(0,5,5,5); (5,1,5,5); (5,5,2,5); (5,5,5,3)]
          |> sort
        in
        let results =
          shrink (tuple4 t0 t1 t2 t3) (5,5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int) list ] results ~expect

      let%test_unit "tuple5 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int * int ] in
        let expect =
          [(0,5,5,5,5); (5,1,5,5,5); (5,5,2,5,5); (5,5,5,3,5); (5,5,5,5,4)]
          |> sort
        in
        let results =
          shrink (tuple5 t0 t1 t2 t3 t4) (5,5,5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int*int) list ] results ~expect

      let%test_unit "tuple6 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int * int * int ] in
        let expect =
          [ (0,9,9,9,9,9)
          ; (9,1,9,9,9,9)
          ; (9,9,2,9,9,9)
          ; (9,9,9,3,9,9)
          ; (9,9,9,9,4,9)
          ; (9,9,9,9,9,5)
          ]
          |> sort
        in
        let results =
          shrink (tuple6 t0 t1 t2 t3 t4 t5) (9,9,9,9,9,9)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int*int*int) list ] results ~expect

    end)

  let variant2 t_a t_b : [ `A of 'a | `B of 'b ] t =
    let shrinker var =
      match var with
      | `A a -> Sequence.map (shrink t_a a) ~f:(fun a -> (`A a))
      | `B b -> Sequence.map (shrink t_b b) ~f:(fun b -> (`B b))
    in
    create shrinker

  let variant3 t_a t_b t_c =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
    in
    create shrinker

  let variant4 t_a t_b t_c t_d =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
    in
    create shrinker

  let variant5 t_a t_b t_c t_d t_e =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
      | `E v -> Sequence.map (shrink t_e v) ~f:(fun v -> (`E v))
    in
    create shrinker

  let variant6 t_a t_b t_c t_d t_e t_f =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
      | `E v -> Sequence.map (shrink t_e v) ~f:(fun v -> (`E v))
      | `F v -> Sequence.map (shrink t_f v) ~f:(fun v -> (`F v))
    in
    create shrinker

  let%test_module "variant shrinkers" =
    (module struct
      open Test_data

      type var2 = [ `A of int | `B of int ] [@@deriving sexp, compare]
      type var3 = [ `A of int | `B of int | `C of int ] [@@deriving sexp, compare]
      type var4 = [ `A of int | `B of int | `C of int | `D of int ] [@@deriving sexp, compare]
      type var5 = [ `A of int | `B of int | `C of int | `D of int | `E of int ]
      [@@deriving sexp, compare]
      type var6 = [ `A of int | `B of int | `C of int | `D of int | `E of int | `F of int ]
      [@@deriving sexp, compare]

      let%test_unit "variant2 shrinker" =
        let t = variant2 t0 t1 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var2 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var2 list ] ~expect:[`B 1] shrunk_b

      let%test_unit "variant3 shrinker" =
        let t = variant3 t0 t1 t2 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`C 2] shrunk_c

      let%test_unit "variant4 shrinker" =
        let t = variant4 t0 t1 t2 t3 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`D 3] shrunk_d

      let%test_unit "variant5 shrinker" =
        let t = variant5 t0 t1 t2 t3 t4 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`D 3] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`E 4] shrunk_e

      let%test_unit "variant6 shrinker" =
        let t = variant6 t0 t1 t2 t3 t4 t5 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`D 3] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`E 4] shrunk_e;
        let shrunk_f = shrink t (`F 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`F 5] shrunk_f
    end)

  let fixed_point f =
    let rec shrinker v =
      Sequence.of_lazy (lazy (shrink (f (create shrinker)) v))
    in
    create shrinker

  let recursive = fixed_point

  module Make_int_shrinker (M : Pre_int) : sig
    val shrinker  : M.t t
  end = struct
    (* Having smaller numbers generally doesn't make a bug easier to
       understand, it is usually much more useful to let other shrinkers have
       more attempts to reduce the size of the datastructure. *)
    let shrinker = empty ()
  end

  module For_int = Make_int_shrinker (Pre_int)

end

module Make_int (M : Pre_int)
  : S_int
    with type    t   :=    M.t
    with type 'a gen := 'a gen
    with type 'a obs := 'a obs
    with type 'a shr := 'a Shrinker.t
= struct

  include Shrinker.Make_int_shrinker   (M)
  include Generator.Make_int_generator (M)
  include Observer.Make_int_observer   (M)

end

module For_int = struct
  include Shrinker.For_int
  include Generator.For_int
  include Observer.For_int
end

module Let_syntax = struct
  module Let_syntax = struct
    include Generator
    module Open_on_rhs = Generator
  end
  include Generator.Monad_infix
  let return = Generator.return
end

module Configure (Config : Quickcheck_config) = struct

  include Config

  type 'a shr = 'a Shrinker.t

  let nondeterministic_state = lazy (Random.State.make_self_init ())

  let random_state_of_seed seed =
    match seed with
    | `Nondeterministic  -> Splittable_random.State.create (force nondeterministic_state)
    | `Deterministic str -> Splittable_random.State.of_int ([%hash: string] str)

  let ensure_infinite sequence ~if_finite_then_raise_s =
    Sequence.unfold_with_and_finish sequence
      (* These two arguments correspond to the ordinary [~init] and [~f] like in
         [Sequence.unfold_step], and we just return values from [sequence] as usual. *)
      ~init:()
      ~running_step:(fun () x -> Yield (x, ()))
      (* These two arguments correspond to a follow-up [~init] and [~f] once [sequence]
         terminates. We raise instead of calculating the new state, and treat the new
         state as having an impossible type equivalent to [Nothing.t], although we cannot
         use [Nothing] directly due to cyclic dependencies. *)
      ~inner_finished:(fun () -> raise_s (force if_finite_then_raise_s))
      ~finishing_step:(function (_ : (unit, int) Type_equal.t) -> .)

  let random_sequence ?(seed = default_seed) ?(sizes = default_sizes) gen =
    let random = random_state_of_seed seed in
    let sizes =
      ensure_infinite sizes
        ~if_finite_then_raise_s:
          (lazy [%message "Quickcheck: [~sizes] argument ran out of values"])
    in
    Sequence.map sizes ~f:(fun size ->
      Generator.generate gen ~size random)

  let iter_or_error ?seed ?sizes ?(trials = default_trial_count) gen ~f =
    let seq = Sequence.take (random_sequence ?seed ?sizes gen) trials in
    Sequence.fold_result seq ~init:() ~f:(fun () value -> f value)

  let iter ?seed ?sizes ?trials gen ~f =
    iter_or_error ?seed ?sizes ?trials gen ~f:(fun elt -> Ok (f elt))
    |> Or_error.ok_exn

  let random_value ?(seed = default_seed) ?(size = 30) gen =
    let random = random_state_of_seed seed in
    Generator.generate gen ~size random

  let shrink_iter
        ?sexp_of
        ~value
        ~error
        ~shrinker
        ?(shrink_attempts = default_shrink_attempts)
        ~f =
    let within_bounds attempts=
      match shrink_attempts with
      | `Exhaustive -> true
      | `Limit n    -> attempts < n
    in
    let rec shrink_loop seq attempts result =
      if within_bounds attempts then
        match Sequence.next seq with
        | None                     -> result
        | Some (shr_value, seq_tl) ->
          match f shr_value with
          | Ok ()           -> shrink_loop seq_tl (attempts+1) result
          | Error shr_error ->
            let seq = Shrinker.shrink shrinker shr_value in
            shrink_loop seq (attempts+1) (Some (shr_value, shr_error))
      else
        result
    in
    match shrink_loop (Shrinker.shrink shrinker value) 0 None with
    | Some (shr_value, shr_error) ->
      let sexp_of_value =
        match sexp_of with
        | Some f -> f
        | None   -> [%sexp_of: _]
      in
      Error.create_s [%message
        "shrunk random input"
          ~shrunk_value:   (shr_value : value)
          ~shrunk_error:   (shr_error : Error.t)
          ~original_value: (value     : value)
          ~original_error: (error     : Error.t)]
    | None ->
      match sexp_of with
      | None               -> error
      | Some sexp_of_value ->
        Error.create_s [%message
          "random input"
            ~value: (value : value)
            ~error: (error : Error.t)]

  let test_or_error
        ?seed
        ?sizes
        ?trials
        ?shrinker
        ?shrink_attempts
        ?sexp_of
        ?(examples = [])
        gen
        ~f
    =
    let f =
      match shrinker with
      | Some shrinker ->
        (fun x ->
           match f x with
           | Ok ()       -> Ok ()
           | Error error ->
             Error (shrink_iter ~value:x ~error ?sexp_of ~shrinker ?shrink_attempts ~f))
      | None ->
        match sexp_of with
        | Some sexp_of_value ->
          (fun value ->
             match f value with
             | Ok ()       -> Ok ()
             | Error error ->
               Or_error.error_s [%message
                 "random input"
                   ~value: (value : value)
                   ~error: (error : Error.t)])
        | None -> f
    in
    let open Or_error.Monad_infix in
    List.fold_result examples ~init:() ~f:(fun () x -> f x)
    >>= fun () ->
    iter_or_error ?seed ?sizes ?trials gen ~f

  let test
        ?seed
        ?sizes
        ?trials
        ?shrinker
        ?shrink_attempts
        ?sexp_of
        ?examples
        gen
        ~f
    =
    test_or_error
      ?seed
      ?sizes
      ?trials
      ?shrinker
      ?shrink_attempts
      ?sexp_of
      ?examples
      gen
      (* Why do we use [Or_error.try_with ~backtrace:true] here but not in [iter]? The
         semantics we're mapping between are different. [iter] doesn't catch errors by
         design, so it's okay to just bubble them up. [test] has _lots_ of logic about
         what it does when it gets an error (it stops, it annotates with random value if
         it has [sexp_of], it finds a minimal example if it has [shrinker]), so we have to
         map errors into something that [test_or_error] will handle properly *)
      ~f:(fun elt -> Or_error.try_with ~backtrace:true (fun () -> f elt))
    |> Or_error.ok_exn

  let test_distinct_values
        (type key)
        ?seed
        ?sizes
        ?sexp_of
        gen
        ~trials
        ~distinct_values
        ~compare
    =
    let module S = Caml.Set.Make (struct type t = key let compare = compare end) in
    let fail set =
      let expect_count = distinct_values in
      let actual_count = S.cardinal set in
      let values =
        match sexp_of with
        | None             -> None
        | Some sexp_of_elt -> Some [%sexp (S.elements set : elt list)]
      in
      raise_s [%message
        "insufficient distinct values"
          (trials       : int)
          (expect_count : int)
          (actual_count : int)
          (values       : Base.Sexp.t sexp_option)]
    in
    with_return (fun r ->
      let set = ref S.empty in
      iter ?seed ?sizes ~trials gen ~f:(fun elt ->
        set := S.add elt !set;
        if S.cardinal !set >= distinct_values then r.return ());
      fail !set)

  let test_can_generate
        ?seed
        ?sizes
        ?(trials = default_can_generate_trial_count)
        ?sexp_of
        gen ~f =
    let r = ref [] in
    let f_and_enqueue return x =
      if f x
      then return `Can_generate
      else r := x :: !r
    in
    match
      With_return.with_return (fun return ->
        iter
          ?seed
          ?sizes
          ~trials
          gen
          ~f:(f_and_enqueue return.return);
        `Cannot_generate)
    with
    | `Can_generate    -> ()
    | `Cannot_generate ->
      match sexp_of with
      | None -> failwith "cannot generate"
      | Some sexp_of_value ->
        Error.raise_s
          [%message
            "cannot generate"
              ~attempts:(!r : value list)]

end

include Configure (struct
    let default_seed = `Deterministic "an arbitrary but deterministic string"
    let default_trial_count =
      match Word_size.word_size with
      | W64 -> 10_000
      | W32 ->  1_000
    let default_can_generate_trial_count = 10_000
    let default_shrink_attempts = `Limit 1000
    let default_sizes =
      Sequence.cycle_list_exn (List.range 0 30 ~stop:`inclusive)
  end)

module type S = S
  with type 'a gen := 'a Generator.t
  with type 'a obs := 'a Observer.t
  with type 'a shr := 'a Shrinker.t

module type S1 = S1
  with type 'a gen := 'a Generator.t
  with type 'a obs := 'a Observer.t
  with type 'a shr := 'a Shrinker.t

module type S2 = S2
  with type 'a gen := 'a Generator.t
  with type 'a obs := 'a Observer.t
  with type 'a shr := 'a Shrinker.t

module type S_int = S_int
  with type 'a gen := 'a Generator.t
  with type 'a obs := 'a Observer.t
  with type 'a shr := 'a Shrinker.t

type nonrec seed            = seed
type nonrec shrink_attempts = shrink_attempts

module type Quickcheck_config = Quickcheck_config

module type Quickcheck_configured = Quickcheck_configured
  with type 'a gen := 'a gen
   and type 'a shr := 'a shr
