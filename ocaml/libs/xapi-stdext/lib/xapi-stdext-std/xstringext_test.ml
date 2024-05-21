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

let test_string tested_f (name, case, expected) =
  let check () = Alcotest.(check string) name expected (tested_f case) in
  (name, `Quick, check)

let test_list tested_f (name, case, expected) =
  let check () =
    Alcotest.(check @@ list string) name expected (tested_f case)
  in
  (name, `Quick, check)

let test_split =
  let test limit (splitter, splitted, expected) =
    let split = XString.split ~limit in
    let name =
      Printf.sprintf {|'%c' splits "%s" with limit %i|} splitter splitted limit
    in
    test_list (split splitter) (name, splitted, expected)
  in
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
  let tests =
    List.concat_map
      (fun (limit, spec) -> List.map (test limit) spec)
      specs_limit
  in
  ("split", tests)

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

let () = Alcotest.run "Xstringext" [test_split; test_rtrim]
