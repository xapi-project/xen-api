
let string_of_err = function | `Msg x -> x
let rec canonicalise r =
  match r with
  | Rpc.Dict x -> Rpc.Dict (List.sort (fun (s1,_) (s2,_) -> compare s1 s2) x |> List.map (fun (x,y) -> (x,canonicalise y)))
  | Rpc.Enum ys -> Rpc.Enum (List.map canonicalise ys)
  | x -> x

let check_marshal_unmarshal : 'a * Rpc.t * 'a Rpc.Types.typ -> unit = fun (x,r,typ) ->
  let r' = Rpcmarshal.marshal typ x in
  let x' = Rpcmarshal.unmarshal typ r in
  Alcotest.check (Testable.unmarshal_res typ)
    "same after marshal->unmarshal"
    (Result.Ok x) x';
  Alcotest.check Testable.rpc
    "marshalled stuff issame as specified rpc after canonicalisation"
    (canonicalise r) (canonicalise r')

let check_unmarshal_error : 'a Rpc.Types.typ -> Rpc.t -> unit = fun typ t ->
  match Rpcmarshal.unmarshal typ t with
  | Result.Ok _ -> Alcotest.fail "unmarshal error expected"
  | Result.Error e -> Printf.printf "%s\n" (string_of_err e)

let check_unmarshal_ok : 'a -> 'a Rpc.Types.typ -> Rpc.t -> unit = fun x typ r ->
  Alcotest.check (Testable.unmarshal_res typ)
    "correctly unmarshalled"
    (Result.Ok x)
    (Rpcmarshal.unmarshal typ r)

type test_int = int [@@deriving rpcty]
let test_int () = check_marshal_unmarshal (1, Rpc.Int 1L, typ_of_test_int)
let test_int_from_string () = check_unmarshal_ok 1 typ_of_test_int (Rpc.String "1")
let test_bad_int () = check_unmarshal_error typ_of_test_int Rpc.Null
let test_bad_int_string () = check_unmarshal_error typ_of_test_int (Rpc.String "tree")

type test_int32 = int32 [@@deriving rpcty]
let test_int32 () = check_marshal_unmarshal (1l, Rpc.Int 1L, typ_of_test_int32)
let test_int32_from_string () = check_unmarshal_ok 1l typ_of_test_int32 (Rpc.String "1")
let test_bad_int32 () = check_unmarshal_error typ_of_test_int32 Rpc.Null
let test_bad_int32_string () = check_unmarshal_error typ_of_test_int32 (Rpc.String "tree")

type test_int64 = int64 [@@deriving rpcty]
let test_int64 () = check_marshal_unmarshal (1L, Rpc.Int 1L, typ_of_test_int64)
let test_int64_from_string () = check_unmarshal_ok 1L typ_of_test_int64 (Rpc.String "1")
let test_bad_int64 () = check_unmarshal_error typ_of_test_int64 Rpc.Null
let test_bad_int64_string () = check_unmarshal_error typ_of_test_int64 (Rpc.String "tree")

type test_unit = unit [@@deriving rpcty]
let test_unit () = check_marshal_unmarshal ((), Rpc.Null, typ_of_test_unit)
let test_bad_unit () = check_unmarshal_error typ_of_test_unit (Rpc.Int 1L)

type test_string = string [@@deriving rpcty]
let test_string () = check_marshal_unmarshal ("test string", Rpc.String "test string", typ_of_test_string)
let test_bad_string () = check_unmarshal_error typ_of_test_string (Rpc.Int 1L)

type test_float = float [@@deriving rpcty]
let test_float () = check_marshal_unmarshal (2.0, Rpc.Float 2.0, typ_of_test_float)
let test_float_from_string () = check_unmarshal_ok 1.0 typ_of_test_float (Rpc.String "1.0")
let test_bad_float () = check_unmarshal_error typ_of_test_float (Rpc.Enum [])
let test_bad_float_string () = check_unmarshal_error typ_of_test_float (Rpc.String "xxy")

type test_bool = bool [@@deriving rpcty]
let test_bool () = check_marshal_unmarshal (true, Rpc.Bool true, typ_of_test_bool)
let test_bad_bool () = check_unmarshal_error typ_of_test_bool (Rpc.String "true")

type test_char = char [@@deriving rpcty]
let test_char () = check_marshal_unmarshal ('x', Rpc.Int (Char.code 'x' |> Int64.of_int), typ_of_test_char)
let test_bad_char () = check_unmarshal_error typ_of_test_char (Rpc.String "x")

type test_int_list = int list [@@deriving rpcty]
let test_int_list () = check_marshal_unmarshal
    ([1;2;3;4], Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L;], typ_of_test_int_list)

type test_int_array = int array [@@deriving rpcty]
let test_int_array () =
  check_marshal_unmarshal
    ([|1;2;3;4|], Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L;], typ_of_test_int_array)

type test_tuple2 = (int * string) [@@deriving rpcty]
let test_tuple2 () =
  check_marshal_unmarshal ((3, "hello"), Rpc.Enum [Rpc.Int 3L; Rpc.String "hello"], typ_of_test_tuple2)

(*type test_tuple3 = (int * string * char) [@@deriving rpcty]
let test_tuple3 () =
  check_marshal_unmarshal ((3, "hi", 'c'), Rpc.Enum [Rpc.Int 3L; Rpc.String "hi"; Rpc.Int (Char.code 'c' |> Int64.of_int)], typ_of_test_tuple3)*)

type test_option = int option [@@deriving rpcty]
let test_option () =
  check_marshal_unmarshal (Some 1, Rpc.Enum [Rpc.Int 1L], typ_of_test_option)
let test_option_none () =
  check_marshal_unmarshal (None, Rpc.Enum [], typ_of_test_option)
let test_bad_option () =
  check_unmarshal_error typ_of_test_option (Rpc.Int 5L)

type test_constr = test_int [@@deriving rpcty]
let test_constr () =
  check_marshal_unmarshal (1, Rpc.Int 1L, typ_of_test_constr)

type test_variant = VNone | VOne of int | VTwo of (int * int) [@@deriving rpcty]
let test_variant () =
  check_marshal_unmarshal (VNone, Rpc.String "VNone", typ_of_test_variant)
let test_variant1 () =
  check_marshal_unmarshal (VOne 1, Rpc.Enum [Rpc.String "VOne"; Rpc.Int 1L], typ_of_test_variant)
let test_variant2 () =
  check_marshal_unmarshal (VTwo (1,2), Rpc.Enum [Rpc.String "VTwo"; Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L]], typ_of_test_variant)
let test_variant_case () =
  check_unmarshal_ok VNone typ_of_test_variant (Rpc.String "vnone")
let test_bad_variant_case () =
  check_unmarshal_error typ_of_test_variant (Rpc.Enum [Rpc.String "vtwo"; Rpc.Int 5L])

type test_variant_name = VThree of int [@name "bob"] | VFour [@name "lofty"] [@@deriving rpcty]
let test_variant_name () =
  check_marshal_unmarshal (VThree 5, Rpc.Enum [Rpc.String "bob"; Rpc.Int 5L], typ_of_test_variant_name)
let test_variant_name2 () =
  check_marshal_unmarshal (VFour, Rpc.String "lofty", typ_of_test_variant_name)

type test_record = {
  fiEld1 : int;
  fiEld2 : string;
  fiEld3 : bool;
} [@@deriving rpcty]
let test_record () =
  check_marshal_unmarshal ({fiEld1=7; fiEld2="banana"; fiEld3=false}, Rpc.Dict ["fiEld1",Rpc.Int 7L; "fiEld2",Rpc.String "banana"; "fiEld3",Rpc.Bool false], typ_of_test_record)
let test_record_case () =
  check_unmarshal_ok {fiEld1=7; fiEld2="banana"; fiEld3=false} typ_of_test_record (Rpc.Dict ["field1",Rpc.Int 7L; "FIELD2",Rpc.String "banana"; "Field3",Rpc.Bool false])
let test_bad_record () =
  check_unmarshal_error typ_of_test_record (Rpc.Dict ["field1",Rpc.Int 7L;])

type test_record_opt = {
  field3 : int option;
  field4 : string option;
} [@@deriving rpcty]
let test_record_opt1 () =
  check_marshal_unmarshal ({field3=Some 7; field4=Some "banana"}, Rpc.Dict ["field3",Rpc.Int 7L; "field4",Rpc.String "banana"], typ_of_test_record_opt)
let test_record_opt2 () =
  check_marshal_unmarshal ({field3=Some 7; field4=None}, Rpc.Dict ["field3",Rpc.Int 7L;], typ_of_test_record_opt)
let test_record_opt3 () =
  check_marshal_unmarshal ({field3=None; field4=Some "hamster"}, Rpc.Dict ["field4",Rpc.String "hamster"], typ_of_test_record_opt)
let test_record_opt4 () =
  check_marshal_unmarshal ({field3=None; field4=None}, Rpc.Dict [], typ_of_test_record_opt)

type test_record_attrs = {
  field5 : int [@key "foo"]
} [@@deriving rpcty]
let test_record_attrs () =
  check_marshal_unmarshal ({field5=6}, Rpc.Dict ["foo", Rpc.Int 6L], typ_of_test_record_attrs)

[@@@warning "+27"]
type test_record_one_field = {
  field : bool
} [@@deriving rpcty]
[@@@warning "-27"]
let test_record_one_field () =
  check_marshal_unmarshal ({field=true}, Rpc.Dict ["field", Rpc.Bool true], typ_of_test_record_one_field)

type key = string [@@deriving rpcty]
type test_dict_key = (key * int) list [@@deriving rpcty]
let test_dict_key () =
 check_marshal_unmarshal (["foo",1; "bar",2; "baz",3], Rpc.Dict ["foo", Rpc.Int 1L; "bar", Rpc.Int 2L; "baz", Rpc.Int 3L], typ_of_test_dict_key)

type 'a test_poly = 'a list [@@deriving rpcty]
let test_poly () =
  let (x : int test_poly) = [1;2;3] in
  check_marshal_unmarshal (x, Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L], (typ_of_test_poly (Rpc.Types.Basic Rpc.Types.Int)))

type nested = {
  variant : test_variant_name;
  var2 : test_variant;
  record : test_record_opt;
  rec2 : test_record;
} [@@deriving rpcty]

let fakegen () =
  let fake ty =
    let fake = Rpc_genfake.genall 10 "string" ty in
    let ss = List.map (fun f -> Rpcmarshal.marshal ty f |> Jsonrpc.to_string) fake in
    let test2 = List.map (fun json -> Rpcmarshal.unmarshal ty (Jsonrpc.of_string json) ) ss in
    List.iter (fun s -> Printf.printf "%s\n" s) ss;
    List.iter2 (function a -> function
      | (Result.Ok b) -> assert(a=b); ()
      | (Result.Error (`Msg err)) -> print_endline err; assert false) fake test2
  in
  fake typ_of_test_record_opt;
  fake typ_of_test_variant_name;
  fake typ_of_nested

type test_defaults = {
  test_with_default : int [@default 5];
} [@@deriving rpcty]

let test_defaults () =
  assert (Result.Ok {test_with_default=5} = Rpcmarshal.unmarshal typ_of_test_defaults (Rpc.Dict []))

type test_defaults_var =
   | X1
   | X2
  [@@deriving rpcty] [@@default X1]

let test_defaults_var () =
  Alcotest.check (Testable.unmarshal_res typ_of_test_defaults_var)
    "Unknown enum correctly unmarshalled into default"
    (Result.Ok X1) (Rpcmarshal.unmarshal typ_of_test_defaults_var (Rpc.String "X3"))

let test_defaults_bad () =
  match Rpcmarshal.unmarshal typ_of_test_defaults_var (Rpc.Int 3L) with
  | Ok _ -> Alcotest.fail "Should have had an error"
  | Error _ -> ()

let tests =
  [ "int", `Quick, test_int
  ; "int_from_string", `Quick, test_int_from_string
  ; "bad_int", `Quick, test_bad_int
  ; "bad_int_string", `Quick, test_bad_int_string
  ; "int32", `Quick, test_int32
  ; "int32_from_string", `Quick, test_int32_from_string
  ; "bad_int32", `Quick, test_bad_int32
  ; "bad_int32_string", `Quick, test_bad_int32_string
  ; "int64", `Quick, test_int64
  ; "int64_from_string", `Quick, test_int64_from_string
  ; "bad_int64", `Quick, test_bad_int64
  ; "bad_int64_string", `Quick, test_bad_int64_string
  ; "unit", `Quick, test_unit
  ; "bad_unit", `Quick, test_bad_unit
  ; "string", `Quick, test_string
  ; "bad_string", `Quick, test_bad_string
  ; "float", `Quick, test_float
  ; "float_from_string", `Quick, test_float_from_string
  ; "bad_float", `Quick, test_bad_float
  ; "bad_float_string", `Quick, test_bad_float_string
  ; "bool", `Quick, test_bool
  ; "bad_bool", `Quick, test_bad_bool
  ; "char", `Quick, test_char
  ; "bad_char", `Quick, test_bad_char
  ; "int list", `Quick, test_int_list
  ; "int array", `Quick, test_int_array
  ; "tuple2", `Quick, test_tuple2
    (*    "tuple3", `Quick, test_tuple3;*)
  ; "option", `Quick, test_option
  ; "option (none)", `Quick, test_option_none
  ; "bad_option", `Quick, test_bad_option
  ; "constr", `Quick, test_constr
  ; "variant", `Quick, test_variant
  ; "variant1", `Quick, test_variant1
  ; "variant2", `Quick, test_variant2
  ; "variant_case", `Quick, test_variant_case
  ; "variant_name", `Quick, test_variant_name
  ; "variant_name2", `Quick, test_variant_name2
  ; "bad_variant_case", `Quick, test_bad_variant_case
  ; "record", `Quick, test_record
  ; "record_case", `Quick, test_record_case
  ; "bad_record", `Quick, test_bad_record
  ; "record_opt1", `Quick, test_record_opt1
  ; "record_opt2", `Quick, test_record_opt2
  ; "record_opt3", `Quick, test_record_opt3
  ; "record_opt4", `Quick, test_record_opt4
  ; "record_attrs", `Quick, test_record_attrs
  ; "record_one_field", `Quick, test_record_one_field
  ; "poly", `Quick, test_poly
  ; "fakegen", `Quick, fakegen
  ; "defaults", `Quick, test_defaults
  ; "defaults_var", `Quick, test_defaults_var
  ; "defaults_bad", `Quick, test_defaults_bad
  ; "dict", `Quick, test_dict_key
  ]
