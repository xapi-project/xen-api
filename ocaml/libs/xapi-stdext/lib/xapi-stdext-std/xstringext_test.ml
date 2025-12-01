(* Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module XString = Xapi_stdext_std.Xstringext.String

let test_boolean tested_f (name, case, expected) =
  let check () = Alcotest.(check bool) name expected (tested_f case) in
  (name, `Quick, check)

let test_string tested_f (name, case, expected) =
  let check () = Alcotest.(check string) name expected (tested_f case) in
  (name, `Quick, check)

let test_list tested_f (name, case, expected) =
  let check () =
    Alcotest.(check @@ list string) name expected (tested_f case)
  in
  (name, `Quick, check)

let test_rev_map =
  let spec_rev = [("", ""); ("foo bar", "rab oof")] in
  let spec_func = [("id", Fun.id); ("uppercase_ascii", Char.uppercase_ascii)] in
  let test (f_name, f) (case, expected) =
    let expected = String.map f expected in
    let name =
      Printf.sprintf {|"%s" produces "%s" (%s)|} case expected f_name
    in
    test_string (XString.rev_map f) (name, case, expected)
  in
  let tests =
    (* Generate the product of the two lists to generate the tests *)
    List.concat_map (fun func -> List.map (test func) spec_rev) spec_func
  in
  ("rev_map", tests)

let test_split =
  let test ?limit (splitter, splitted, expected) =
    let split, name =
      match limit with
      | None ->
          let name = Printf.sprintf {|'%c' splits "%s"|} splitter splitted in
          (* limit being set to -1 is the same as not using the parameter *)
          let split = XString.split ~limit:(-1) in
          (split, name)
      | Some limit ->
          let name =
            Printf.sprintf {|'%c' splits "%s" with limit %i|} splitter splitted
              limit
          in
          let split = XString.split ~limit in
          (split, name)
    in
    test_list (split splitter) (name, splitted, expected)
  in
  let specs_no_limit =
    [
      ('.', "...", [""; ""; ""; ""]); ('.', "foo.bar.baz", ["foo"; "bar"; "baz"])
    ]
  in
  let tests_no_limit = List.map test specs_no_limit in
  let specs_limit =
    [
      (0, [('.', "...", ["..."]); ('.', "foo.bar.baz", ["foo.bar.baz"])])
    ; (1, [('.', "...", ["..."]); ('.', "foo.bar.baz", ["foo.bar.baz"])])
    ; (2, [('.', "...", [""; ".."]); ('.', "foo.bar.baz", ["foo"; "bar.baz"])])
    ; ( 3
      , [
          ('.', "...", [""; ""; "."])
        ; ('.', "foo.bar.baz", ["foo"; "bar"; "baz"])
        ]
      )
    ; (4, [('.', "...", [""; ""; ""; ""])])
    ]
  in
  let tests_limit =
    List.concat_map
      (fun (limit, spec) -> List.map (test ~limit) spec)
      specs_limit
  in
  ("split", List.concat [tests_no_limit; tests_limit])

let test_split_f =
  let specs =
    [
      (XString.isspace, "foo bar", ["foo"; "bar"])
    ; (XString.isspace, "foo  bar", ["foo"; "bar"])
    ; (XString.isspace, "foo \n\t\r bar", ["foo"; "bar"])
    ; (XString.isspace, " foo bar ", ["foo"; "bar"])
    ; (XString.isspace, "", [])
    ; (XString.isspace, " ", [])
    ]
  in
  let test (splitter, splitted, expected) =
    let name = Printf.sprintf {|"%s"|} (String.escaped splitted) in
    test_list (XString.split_f splitter) (name, splitted, expected)
  in
  let tests = List.map test specs in
  ("split_f", tests)

let test_has_substr =
  let spec =
    [
      ("", "", true)
    ; ("", "foo bar", true)
    ; ("f", "foof", true)
    ; ("foofo", "foof", false)
    ; ("foof", "foof", true)
    ; ("f", "foof", true)
    ; ("fo", "foof", true)
    ; ("of", "foof", true)
    ; ("ff", "foof", false)
    ]
  in
  let test (contained, container, expected) =
    let name = Printf.sprintf {|"%s" in "%s"|} contained container in
    test_boolean (XString.has_substr container) (name, contained, expected)
  in
  ("has_substr", List.map test spec)

let test_rtrim =
  let spec =
    [
      ("", "")
    ; ("\n", "")
    ; ("\n\n", "\n")
    ; ("\n ", "\n ")
    ; ("foo\n", "foo")
    ; ("fo\no", "fo\no")
    ]
  in
  let test (case, expected) =
    let name =
      Printf.sprintf {|"%s" gets trimmed to "%s"|} (String.escaped case)
        (String.escaped expected)
    in
    test_string XString.rtrim (name, case, expected)
  in
  ("rtrim", List.map test spec)

(** Simple implementation of escaped for testing against *)
let escaped_spec ?rules string =
  match rules with
  | None ->
      String.escaped string
  | Some rules ->
      let apply_rules char =
        match List.assoc_opt char rules with
        | None ->
            Seq.return char
        | Some replacement ->
            String.to_seq replacement
      in
      string |> String.to_seq |> Seq.concat_map apply_rules |> String.of_seq

let test_escaped =
  let open QCheck2 in
  (* Generator for escape rules: list of (char, string) mappings *)
  let gen_rules =
    let open Gen in
    let gen_rule = pair char (string_size (int_range 0 5) ~gen:char) in
    list gen_rule
  in
  (* Generator for test input: string and optional rules *)
  let gen_input = Gen.pair Gen.string (Gen.opt gen_rules) in
  let property (s, rules) =
    let expected = escaped_spec ?rules s in
    let actual = XString.escaped ?rules s in
    String.equal expected actual
  in
  let test =
    Test.make ~name:"escaped matches reference implementation" ~count:1000
      gen_input property
  in
  ("escaped", [QCheck_alcotest.to_alcotest test])

let () =
  Alcotest.run "Xstringext"
    [
      test_rev_map
    ; test_split
    ; test_split_f
    ; test_has_substr
    ; test_rtrim
    ; test_escaped
    ]
