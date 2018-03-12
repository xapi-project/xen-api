(*
 * Copyright (C) 2010-2014 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Db_cache_types

let create_test_db () =
  let schema = Test_schemas.many_to_many in
  Database.make schema
  |> Db_upgrade.generic_database_upgrade
  |> Db_backend.blow_away_non_persistent_fields schema

let check_many_to_many () =
  let db = create_test_db () in
  (* make a foo with bars = [] *)
  (* make a bar with foos = [] *)
  (* add 'bar' to foo.bars *)
  let db = db
           |> add_row "bar" "bar:1" (Row.add 0L Db_names.ref (Schema.Value.String "bar:1") (Row.add 0L "foos" (Schema.Value.Set []) Row.empty))
           |> add_row "foo" "foo:1" (Row.add 0L Db_names.ref (Schema.Value.String "foo:1") (Row.add 0L "bars" (Schema.Value.Set []) Row.empty))
           |> set_field "foo" "foo:1" "bars" (add_to_set "bar:1" (Schema.Value.Set []))
  in
  (* check that 'bar.foos' includes 'foo' *)
  let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
  let bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [ "foo:1" ])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));

  (* set foo.bars to [] *)
  (*		let foo_1 = Table.find "foo:1" (TableSet.find "foo" (Database.tableset db)) in*)
  let db = set_field "foo" "foo:1" "bars" (Schema.Value.Set []) db in
  (* check that 'bar.foos' is empty *)
  let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
  let bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected () got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));
  (* add 'bar' to foo.bars *)
  let db = set_field "foo" "foo:1" "bars" (Schema.Value.Set [ "bar:1" ]) db in
  (* check that 'bar.foos' includes 'foo' *)
  let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
  let bar_foos = Row.find "foos" bar_1 in
  if bar_foos <> (Schema.Value.Set [ "foo:1" ])
  then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s - 2" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t bar_foos)));
  (* delete 'bar' *)
  let db = remove_row "bar" "bar:1" db in
  (* check that 'foo.bars' is empty *)
  let foo_1 = Table.find "foo:1" (TableSet.find "foo" (Database.tableset db)) in
  let foo_bars = Row.find "bars" foo_1 in
  if foo_bars <> (Schema.Value.Set [])
  then failwith (Printf.sprintf "check_many_to_many: foo(foo:1).foos expected () got %s" (Sexplib.Sexp.to_string (Schema.Value.sexp_of_t foo_bars)));
  ()

open OUnit

let _ =
  let suite = "db_cache" >:::
              [
                "many to many" >:: check_many_to_many;
              ] in
  OUnit2.run_test_tt_main (OUnit.ounit2_of_ounit1 suite)
