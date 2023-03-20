open Xapi_metastore

(* primitive OCaml types, including corner cases.
   We always want to test corner cases: the randomly generated values
   may not hit all corner cases most of the time.
   So split generation in two: fully random, vs corner cases with 50/50 chance
   to pick one
*)

let corner_cases gen lst =
  Crowbar.(choose [gen; choose @@ List.rev_map const lst])

let int = corner_cases Crowbar.int [min_int; max_int]

let char = corner_cases Crowbar.char ['\x00'; '\xff']

let all_str = String.init 255 Char.chr

let string = corner_cases Crowbar.bytes [""; all_str]

let float =
  corner_cases Crowbar.float
    [
      -0.0
    ; Float.succ 1.0
    ; Float.succ 0.
    ; Float.infinity
    ; Float.neg_infinity
    ; Float.nan
    ; Float.max_float
    ; Float.min_float
    ]

let bool = Crowbar.bool

let unit = Crowbar.const ()

let int32 = corner_cases Crowbar.int32 [Int32.min_int; Int32.max_int]

let int64 = corner_cases Crowbar.int64 [Int64.min_int; Int64.max_int]

(*let large_list elt =
  Crowbar.choose
  [Crowbar.list elt
  ;Crowbar.map [elt] (fun x -> List.init 1_000_000 (fun _ -> x))
  ]*)

let array elt = Crowbar.map [Crowbar.list elt] Array.of_list

type t = {
    i: int
  ; c: char
  ; s: string
  ; f: float
  ; bo: bool
  ; u: unit
  ; a: int array
  ; l: char list
  ; o: bool option
  ; i32: int32
  ; i64: int64
  ; p: int * int
}
[@@deriving rpcty]

let equal x y =
  Int.equal x.i y.i
  && Char.equal x.c y.c
  && String.equal x.s y.s
  (* must use Float.equal here so that [nan] is equal to itself,
     which by default it wouldn't be *)
  && Float.equal x.f y.f
  && Bool.equal x.bo y.bo
  && Unit.equal x.u y.u
  && Array.for_all2 Int.equal x.a y.a
  && List.equal Char.equal x.l y.l
  && Option.equal Bool.equal x.o y.o
  && Int32.equal x.i32 y.i32
  && Int64.equal x.i64 y.i64
  && x.p = y.p

let pack ((i, c, s, f), (bo, u, a, l), (o, i32, i64, p)) =
  {i; c; s; f; bo; u; a; l; o; i32; i64; p}

let unpack t =
  ((t.i, t.c, t.s, t.f), (t.bo, t.u, t.a, t.l), (t.o, t.i32, t.i64, t.p))

let t =
  Crowbar.map
    [
      int
    ; char
    ; string
    ; float
    ; bool
    ; unit
    ; array int
    ; Crowbar.list char
    ; Crowbar.option bool
    ; int32
    ; int64
    ; Crowbar.pair int int
    ]
  @@ fun i c s f bo u a l o i32 i64 p ->
  {i; c; s; f; bo; u; a; l; o; i32; i64; p}

(* ppx_deriving_rpc only supports tuples up to arity 4 *)
type t2 =
  (int * char * string * float)
  * (bool * unit * int array * char list)
  * (bool option * int32 * int64 * (int * int))
[@@deriving rpcty]

let test_dump_rpc () =
  Format.printf "%a@."
    (Serialization.dump typ_of)
    {
      i= 0
    ; c= 'a'
    ; s= "str"
    ; f= 4.5
    ; bo= true
    ; u= ()
    ; a= [|9; -7|]
    ; l= ['c'; 'd']
    ; o= Some false
    ; i32= 7l
    ; i64= 8L
    ; p= (5, -1)
    }

let () = test_dump_rpc ()

let typ_of_t2' = Serialization.using ~aname:"t" unpack pack typ_of_t2

let test_serialize_deserialize eq typ_of t =
  let actual =
    t |> Serialization.serialize typ_of |> Serialization.deserialize typ_of
  in
  let eq = Result.equal ~ok:eq ~error:( = ) in
  Crowbar.check_eq ~eq
    ~pp:(Fmt.result ~ok:(Serialization.dump typ_of) ~error:Rresult.R.pp_msg)
    (Ok t) actual

let test_using t = test_serialize_deserialize equal typ_of_t2' t

let test_serialize t = test_serialize_deserialize equal typ_of t

module StringMap = Map.Make (String)

(* don't use [t] as value because both might consist of very large lists,
   and then we'd get 1_000_000*1_000_000 elements
*)
let typ_of_map = Serialization.typ_of_stringmap Rpc.Types.int.ty

let map =
  Crowbar.map [Crowbar.list (Crowbar.pair string int)] @@ fun lst ->
  lst |> List.to_seq |> StringMap.of_seq

let test_stringmap map =
  test_serialize_deserialize (StringMap.equal Int.equal) typ_of_map map

let () =
  Crowbar.add_test ~name:"serialize/deserialize" [t] test_serialize ;
  Crowbar.add_test ~name:"using" [t] test_using ;
  Crowbar.add_test ~name:"stringmap" [map] test_stringmap
