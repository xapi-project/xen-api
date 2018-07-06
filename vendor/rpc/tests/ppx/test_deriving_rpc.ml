(* All the ppx tests *)

(* Check that t -/-> of_rpc but t -> t_of_rpc *)
type t = int [@@deriving rpc]
let _ = t_of_rpc

let string_of_err = function `Msg x -> x

let check_marshal_unmarshal : 'a * Rpc.t * ('a -> Rpc.t) * (Rpc.t -> 'a) -> unit = fun (x, r, marshal, unmarshal) ->
  let r' = marshal x in
  let x' = unmarshal r in
  Alcotest.check (Testable.from_rpc_of_t marshal) "same after marshal->unmarshal" x x';
  Alcotest.check Testable.rpc "marshalled stuff is same as specified rpc" r r'


let check_unmarshal_error : (Rpc.t -> 'a) -> Rpc.t -> unit = fun unmarshal t ->
  let u =
    try
      Some (unmarshal t)
    with e ->
      None
  in
  match u with
  | Some _ -> Alcotest.fail "Expecting an error when unmarshalling"
  | None -> ()

let check_unmarshal_ok : 'a Alcotest.testable -> (Rpc.t -> 'a) -> 'a -> Rpc.t -> unit = fun testable unmarshal x r ->
  let x' = unmarshal r in
  Alcotest.check testable "unmarshaller returned expected value" x x'

type test_int = int [@@deriving rpc]
let test_int () =
  check_marshal_unmarshal (1, Rpc.Int 1L, rpc_of_test_int, test_int_of_rpc)
let test_int_from_string () =
  check_unmarshal_ok Alcotest.int test_int_of_rpc 1 (Rpc.String "1")
let test_bad_int () =
  check_unmarshal_error test_int_of_rpc Rpc.Null
let test_bad_int_string () =
  check_unmarshal_error test_int_of_rpc (Rpc.String "tree")

type test_int32 = int32 [@@deriving rpc]
let test_int32 () =
  check_marshal_unmarshal (1l, Rpc.Int 1L, rpc_of_test_int32, test_int32_of_rpc)
let test_int32_from_string () =
  check_unmarshal_ok Alcotest.int32 test_int32_of_rpc 1l (Rpc.String "1")
let test_bad_int32 () =
  check_unmarshal_error test_int32_of_rpc (Rpc.Float 1.0)
let test_bad_int32_string () =
  check_unmarshal_error test_int32_of_rpc (Rpc.String "moo")

type test_int64 = int64 [@@deriving rpc]
let test_int64 () =
  check_marshal_unmarshal (1L, Rpc.Int 1L, rpc_of_test_int64, test_int64_of_rpc)
let test_int64_from_string () =
  check_unmarshal_ok Alcotest.int64 test_int64_of_rpc 1L (Rpc.String "1")
let test_bad_int64 () =
  check_unmarshal_error test_int64_of_rpc (Rpc.Float 1.0)
let test_bad_int64_string () =
  check_unmarshal_error test_int64_of_rpc (Rpc.String "hello")

type test_unit = unit [@@deriving rpc]
let test_unit () =
  check_marshal_unmarshal ((), Rpc.Null,  rpc_of_test_unit, test_unit_of_rpc)
let test_bad_unit () =
  check_unmarshal_error test_unit_of_rpc (Rpc.Int 1L)

type test_string = string [@@deriving rpc]
let test_string () =
  check_marshal_unmarshal ("test string", Rpc.String "test string", rpc_of_test_string, test_string_of_rpc)
let test_bad_string () =
  check_unmarshal_error test_string_of_rpc (Rpc.Int 1L)

type test_float = float [@@deriving rpc]
let check_unmarshal_float_ok =
  check_unmarshal_ok Testable.float test_float_of_rpc
let test_float () =
  check_marshal_unmarshal (2.0, Rpc.Float 2.0, rpc_of_test_float, test_float_of_rpc)
let test_float_from_int () =
  check_unmarshal_float_ok 1.0 (Rpc.Int 1L)
let test_float_from_int32 () =
  check_unmarshal_float_ok 1.0 (Rpc.Int32 1l)
let test_float_from_string () =
  check_unmarshal_float_ok 1.0 (Rpc.String "1.0")
let test_bad_float () =
  check_unmarshal_error test_float_of_rpc (Rpc.Enum [])
let test_bad_float_string () =
  check_unmarshal_error test_float_of_rpc (Rpc.String "xxy")

type test_bool = bool [@@deriving rpc]
let test_bool () =
  check_marshal_unmarshal (true, Rpc.Bool true, rpc_of_test_bool, test_bool_of_rpc)
let test_bad_bool () =
  check_unmarshal_error test_bool_of_rpc (Rpc.String "true")

type test_char = char [@@deriving rpc]
let test_char () =
  check_marshal_unmarshal ('x', Rpc.Int (Char.code 'x' |> Int64.of_int), rpc_of_test_char, test_char_of_rpc)
let test_bad_char () =
  check_unmarshal_error test_char_of_rpc (Rpc.String "x")

type test_int_list = int list [@@deriving rpc]
let test_int_list () =
  check_marshal_unmarshal ([1;2;3;4], Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L;], rpc_of_test_int_list, test_int_list_of_rpc)

type test_dict = (string * int) list [@@deriving rpc]
let test_dict () =
  check_marshal_unmarshal (["foo",1; "bar",2; "baz",3], Rpc.Dict ["foo", Rpc.Int 1L; "bar", Rpc.Int 2L; "baz", Rpc.Int 3L], rpc_of_test_dict, test_dict_of_rpc)

type key = string [@@deriving rpc]
type test_dict_key = (key * int) list [@@deriving rpc]
let test_dict_key () =
  check_marshal_unmarshal (["foo",1; "bar",2; "baz",3], Rpc.Dict ["foo", Rpc.Int 1L; "bar", Rpc.Int 2L; "baz", Rpc.Int 3L], rpc_of_test_dict_key, test_dict_key_of_rpc)

type test_int_array = int array [@@deriving rpc]
let test_int_array () =
  check_marshal_unmarshal ([|1;2;3;4|], Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L;], rpc_of_test_int_array, test_int_array_of_rpc)

type test_tuple2 = (int * string) [@@deriving rpc]
let test_tuple2 () =
  check_marshal_unmarshal ((3, "hello"), Rpc.Enum [Rpc.Int 3L; Rpc.String "hello"], rpc_of_test_tuple2, test_tuple2_of_rpc)

type test_tuple3 = (int * string * char) [@@deriving rpc]
let test_tuple3 () =
  check_marshal_unmarshal ((3, "hi", 'c'), Rpc.Enum [Rpc.Int 3L; Rpc.String "hi"; Rpc.Int (Char.code 'c' |> Int64.of_int)], rpc_of_test_tuple3, test_tuple3_of_rpc)

type test_option = int option [@@deriving rpc]
let test_option () =
  check_marshal_unmarshal (Some 1, Rpc.Enum [Rpc.Int 1L], rpc_of_test_option, test_option_of_rpc)
let test_option_none () =
  check_marshal_unmarshal (None, Rpc.Enum [], rpc_of_test_option, test_option_of_rpc)
let test_bad_option () =
  check_unmarshal_error test_option_of_rpc (Rpc.Int 5L)

type test_constr = test_int [@@deriving rpc]
let test_constr () =
  check_marshal_unmarshal (1, Rpc.Int 1L, rpc_of_test_constr, test_constr_of_rpc)

type test_variant = VNone | VOne of int | VTwo of (int * int) [@@deriving rpc]
let test_variant () =
  check_marshal_unmarshal (VNone, Rpc.String "VNone", rpc_of_test_variant, test_variant_of_rpc)
let test_variant1 () =
  check_marshal_unmarshal (VOne 1, Rpc.Enum [Rpc.String "VOne"; Rpc.Int 1L], rpc_of_test_variant, test_variant_of_rpc)
let test_variant2 () =
  check_marshal_unmarshal (VTwo (1,2), Rpc.Enum [Rpc.String "VTwo"; Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L]], rpc_of_test_variant, test_variant_of_rpc)
let test_variant_case () =
  check_unmarshal_ok (Testable.from_rpc_of_t rpc_of_test_variant)
    test_variant_of_rpc VNone (Rpc.String "vnone")
let test_bad_variant_case () =
  check_unmarshal_error test_variant_of_rpc (Rpc.Enum [Rpc.String "vtwo"; Rpc.Int 5L])

type test_variant_name = VThree of int [@name "bob"] | VFour [@name "lofty"] [@@deriving rpc]
let test_variant_name () =
  check_marshal_unmarshal (VThree 5, Rpc.Enum [Rpc.String "bob"; Rpc.Int 5L], rpc_of_test_variant_name, test_variant_name_of_rpc)
let test_variant_name2 () =
  check_marshal_unmarshal (VFour, Rpc.String "lofty", rpc_of_test_variant_name, test_variant_name_of_rpc)

type test_record = {
  fiEld1 : int;
  fiEld2 : string;
} [@@deriving rpc]
let test_record () =
  check_marshal_unmarshal ({fiEld1=7; fiEld2="banana"}, Rpc.Dict ["fiEld1",Rpc.Int 7L; "fiEld2",Rpc.String "banana"], rpc_of_test_record, test_record_of_rpc)
let test_record_case () =
  check_unmarshal_ok (Testable.from_rpc_of_t rpc_of_test_record) test_record_of_rpc {fiEld1=7; fiEld2="banana"} (Rpc.Dict ["field1",Rpc.Int 7L; "FIELD2",Rpc.String "banana"])
let test_bad_record () =
  check_unmarshal_error test_record_of_rpc (Rpc.Dict ["field1",Rpc.Int 7L;])

type test_record_opt = {
  field3 : int option;
  field4 : string option;
} [@@deriving rpc]
let test_record_opt1 () =
  check_marshal_unmarshal ({field3=Some 7; field4=Some "banana"}, Rpc.Dict ["field3",Rpc.Int 7L; "field4",Rpc.String "banana"], rpc_of_test_record_opt, test_record_opt_of_rpc)
let test_record_opt2 () =
  check_marshal_unmarshal ({field3=Some 7; field4=None}, Rpc.Dict ["field3",Rpc.Int 7L;], rpc_of_test_record_opt, test_record_opt_of_rpc)
let test_record_opt3 () =
  check_marshal_unmarshal ({field3=None; field4=Some "hamster"}, Rpc.Dict ["field4",Rpc.String "hamster"], rpc_of_test_record_opt, test_record_opt_of_rpc)
let test_record_opt4 () =
  check_marshal_unmarshal ({field3=None; field4=None}, Rpc.Dict [], rpc_of_test_record_opt, test_record_opt_of_rpc)

type test_record_attrs = {
  field5 : int [@key "foo"]
} [@@deriving rpc]
let test_record_attrs () =
  check_marshal_unmarshal ({field5=6}, Rpc.Dict ["foo", Rpc.Int 6L], rpc_of_test_record_attrs, test_record_attrs_of_rpc)

type 'a test_poly = 'a list [@@deriving rpc]
let test_poly () =
  let (x : int test_poly) = [1;2;3] in
  check_marshal_unmarshal (x, Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L], (rpc_of_test_poly Rpc.rpc_of_int), (test_poly_of_rpc Rpc.int_of_rpc))

type 'a myref = string [@@deriving rpc]
type vdi_ref = [`VDI] myref [@@deriving rpc]

type test_polyvar = [ `one | `two of int | `thRee of int * int ] [@@deriving rpc]
let test_polyvar () =
  check_marshal_unmarshal (`one, Rpc.String "one", rpc_of_test_polyvar, test_polyvar_of_rpc)
let test_polyvar2 () =
  check_marshal_unmarshal (`two 2, Rpc.Enum [Rpc.String "two"; Rpc.Int 2L], rpc_of_test_polyvar, test_polyvar_of_rpc)
let test_polyvar3 () =
  check_marshal_unmarshal (`thRee (4,5), Rpc.Enum [Rpc.String "thRee"; Rpc.Enum [ Rpc.Int 4L; Rpc.Int 5L]], rpc_of_test_polyvar, test_polyvar_of_rpc)
let test_polyvar_case () =
  check_unmarshal_ok (Testable.from_rpc_of_t rpc_of_test_polyvar)
    test_polyvar_of_rpc (`thRee (4,5)) (Rpc.Enum [Rpc.String "THREE"; Rpc.Enum [ Rpc.Int 4L; Rpc.Int 5L]])

type test_pvar_inherit = [ `four of string | test_polyvar ] [@@deriving rpc]
let test_pvar_inherit () =
  check_marshal_unmarshal (`one, Rpc.String "one", rpc_of_test_pvar_inherit, test_pvar_inherit_of_rpc)
let test_pvar_inherit2 () =
  check_marshal_unmarshal (`four "apple", Rpc.Enum [Rpc.String "four"; Rpc.String "apple"], rpc_of_test_pvar_inherit, test_pvar_inherit_of_rpc)

type enum = [ `x | `y | `z | `default ] [@default `default] [@@deriving rpc]
let test_default_enum () =
  check_unmarshal_ok (Testable.from_rpc_of_t rpc_of_enum) enum_of_rpc `default (Rpc.String "unknown_enum");
  check_unmarshal_ok (Testable.from_rpc_of_t rpc_of_enum) enum_of_rpc `default (Rpc.Enum [Rpc.String "thRee"; Rpc.Enum [ Rpc.Int 4L; Rpc.Int 5L]]);
  check_unmarshal_error enum_of_rpc (Rpc.Enum [Rpc.Int 6L]);
  check_unmarshal_error enum_of_rpc (Rpc.Dict ["foo",Rpc.String "bar"]);
  check_unmarshal_error enum_of_rpc (Rpc.Int 1L);
  check_unmarshal_error enum_of_rpc (Rpc.Float 1.0)

type enum_string_map = (enum * string) list [@@deriving rpc]
let test_enum_string_map () =
  check_marshal_unmarshal ([`x, "x"; `y, "y"; `z, "z"], Rpc.Dict ["x", Rpc.String "x"; "y", Rpc.String "y"; "z", Rpc.String "z"], rpc_of_enum_string_map, enum_string_map_of_rpc)

type enum2 = [`a | `b | `c] [@@deriving rpc]
type enum_string_map2 = (enum2 * string) list [@dict] [@@deriving rpc]
let test_enum_string_map2 () =
    check_marshal_unmarshal ([`a, "x"; `b, "y"; `c, "z"], Rpc.Dict ["a", Rpc.String "x"; "b", Rpc.String "y"; "c", Rpc.String "z"], rpc_of_enum_string_map2, enum_string_map2_of_rpc)



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
  ; "float_from_int", `Quick, test_float_from_int
  ; "float_from_int32", `Quick, test_float_from_int32
  ; "float_from_string", `Quick, test_float_from_string
  ; "bad_float", `Quick, test_bad_float
  ; "bad_float_string", `Quick, test_bad_float_string
  ; "bool", `Quick, test_bool
  ; "bad_bool", `Quick, test_bad_bool
  ; "char", `Quick, test_char
  ; "bad_char", `Quick, test_bad_char
  ; "int list", `Quick, test_int_list
  ; "int array", `Quick, test_int_array
  ; "dict", `Quick, test_dict
  ; "dict_key", `Quick, test_dict_key
  ; "tuple2", `Quick, test_tuple2
  ; "tuple3", `Quick, test_tuple3
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
  ; "poly", `Quick, test_poly
  ; "polyvar", `Quick, test_polyvar
  ; "polyvar2", `Quick, test_polyvar2
  ; "polyvar3", `Quick, test_polyvar3
  ; "polyvar_case", `Quick, test_polyvar_case
  ; "pvar_inherit", `Quick, test_pvar_inherit
  ; "pvar_inherit2", `Quick, test_pvar_inherit2
  ; "default_enum", `Quick, test_default_enum
  ; "enum_string_map", `Quick, test_enum_string_map
  ; "enum_string_map2", `Quick, test_enum_string_map2
  ]
