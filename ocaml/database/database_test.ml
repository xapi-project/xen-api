(*
 * Copyright (C) 2010 Citrix Systems Inc.
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
open Xapi_stdext_monadic
open Xapi_stdext_unix

let name_label = "name__label"
let name_description = "name__description"

module Tests = functor(Client: Db_interface.DB_ACCESS) -> struct

  let name = "thevmname"
  let invalid_name = "notavmname"

  let make_vm r uuid =
    [
      "uuid", uuid;
      name_description, "";
      (*      "protection_policy", "";*)
      "other_config", "()";
      "tags", "()";
      name_label, name;
    ]

  let make_vbd vm r uuid = [
    (*    "ref", r; *)
    "uuid", uuid;
    "VM", vm;
    "type", "user";
  ]

  let expect_missing_row tbl r f =
    try
      f ()
    with Db_exn.DBCache_NotFound("missing row", tbl', r') when tbl' = tbl && r = r' -> ()

  let expect_missing_tbl tbl f =
    try
      f ()
    with Db_exn.DBCache_NotFound("missing table", tbl', "") when tbl' = tbl -> ()

  let expect_uniqueness_violation tbl fld v f =
    try
      f ()
    with Db_exn.Uniqueness_constraint_violation(tbl', fld', v') when tbl' = tbl && fld' = fld && v' = v -> ()

  let expect_missing_uuid tbl uuid f =
    try
      f ()
    with Db_exn.Read_missing_uuid(tbl', "", uuid') when tbl' = tbl && uuid' = uuid -> ()

  let expect_missing_column name f =
    try
      f ()
    with Db_exn.DBCache_NotFound("missing column", _, name') when name' = name -> ()

  let expect_missing_field name f =
    try
      f ()
    with Db_exn.DBCache_NotFound("missing field", name', _) when name' = name -> ()
  let test_invalid_where_record fn_name fn =
    Printf.printf "%s <invalid table> ...\n" fn_name;
    expect_missing_tbl "Vm"
      (fun () ->
         let (_: string list) = fn { Db_cache_types.table = "Vm"; return = ""; where_field = ""; where_value = "" } in
         failwith (Printf.sprintf "%s <invalid table>" fn_name)
      );
    Printf.printf "%s <valid table> <invalid return> <valid field> <valid value>\n" fn_name;
    expect_missing_field "wibble"
      (fun () ->
         let (_: string list) = fn { Db_cache_types.table = "VM"; return = "wibble"; where_field = name_label; where_value = name } in
         failwith (Printf.sprintf "%s <valid table> <invalid return> <valid field> <valid value>" fn_name)
      );
    Printf.printf "%s <valid table> <valid return> <invalid field> <valid value>\n" fn_name;
    expect_missing_field "wibble"
      (fun () ->
         let (_: string list) = fn { Db_cache_types.table = "VM"; return = name_label; where_field = "wibble"; where_value = "" } in
         failwith (Printf.sprintf "%s <valid table> <valid return> <invalid field> <valid value>" fn_name)
      )

  (* Verify the ref_index contents are correct for a given [tblname] and [key] (uuid/ref) *)
  let check_ref_index t tblname key = match Ref_index.lookup key with
    | None ->
      (* We should fail to find the row *)
      expect_missing_row tblname key
        (fun () -> let (_: string) = Client.read_field t tblname "uuid" key in ());
      expect_missing_uuid tblname key
        (fun () -> let (_: string) = Client.db_get_by_uuid t tblname key in ())
    | Some { Ref_index.name_label = name_label'; uuid = uuid; _ref = _ref } ->
      (* key should be either uuid or _ref *)
      if key <> uuid && (key <> _ref)
      then failwith (Printf.sprintf "check_ref_index %s key %s: got ref %s uuid %s" tblname key _ref uuid);
      let real_ref = if Client.is_valid_ref t key then key else Client.db_get_by_uuid t tblname key in
      let real_name_label =
        try Some (Client.read_field t tblname name_label real_ref)
        with _ -> None in
      if name_label' <> real_name_label
      then failwith (Printf.sprintf "check_ref_index %s key %s: ref_index name_label = %s; db has %s" tblname key (Opt.default "None" name_label') (Opt.default "None" real_name_label))


  open Xapi_stdext_pervasives
  open Db_cache_types

  let create_test_db () =
    let schema = Test_schemas.many_to_many in
    let db =
      (Database.make schema) |>
      (Db_upgrade.generic_database_upgrade) |>
      (Db_backend.blow_away_non_persistent_fields schema)
    in
    db

  let check_many_to_many () =
    let db = create_test_db () in
    (* make a foo with bars = [] *)
    (* make a bar with foos = [] *)
    (* add 'bar' to foo.bars *)
    let db = db |>
             add_row "bar" "bar:1" (Row.add 0L Db_names.ref (String "bar:1") (Row.add 0L "foos" (Set []) Row.empty)) |>
             add_row "foo" "foo:1" (Row.add 0L Db_names.ref (String "foo:1") (Row.add 0L "bars" (Set []) Row.empty)) |>
             set_field "foo" "foo:1" "bars" (add_to_set "bar:1" (Set []))
    in
    (* check that 'bar.foos' includes 'foo' *)
    let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
    let bar_foos = Row.find "foos" bar_1 in
    if bar_foos <> Set ["foo:1"]
    then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s" (Schema.Value.marshal bar_foos));

    (* set foo.bars to [] *)
    (*		let foo_1 = Table.find "foo:1" (TableSet.find "foo" (Database.tableset db)) in*)
    let db = set_field "foo" "foo:1" "bars" (Set []) db in
    (* check that 'bar.foos' is empty *)
    let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
    let bar_foos = Row.find "foos" bar_1 in
    if bar_foos <> Set []
    then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected () got %s" (Schema.Value.marshal bar_foos));
    (* add 'bar' to foo.bars *)
    let db = set_field "foo" "foo:1" "bars" (Set["bar:1"]) db in
    (* check that 'bar.foos' includes 'foo' *)
    let bar_1 = Table.find "bar:1" (TableSet.find "bar" (Database.tableset db)) in
    let bar_foos = Row.find "foos" bar_1 in
    if bar_foos <> (Set["foo:1"])
    then failwith (Printf.sprintf "check_many_to_many: bar(bar:1).foos expected ('foo:1') got %s - 2" (Schema.Value.marshal bar_foos));
    (* delete 'bar' *)
    let db = remove_row "bar" "bar:1" db in
    (* check that 'foo.bars' is empty *)
    let foo_1 = Table.find "foo:1" (TableSet.find "foo" (Database.tableset db)) in
    let foo_bars = Row.find "bars" foo_1 in
    if foo_bars <> (Set [])
    then failwith (Printf.sprintf "check_many_to_many: foo(foo:1).foos expected () got %s" (Schema.Value.marshal foo_bars));
    ()

  let check_events t =
    let dump db g =
      let tables = Db_cache_types.Database.tableset db in
      Db_cache_types.TableSet.fold_over_recent g
        (fun name _ table acc ->
           Db_cache_types.Table.fold_over_recent g
             (fun r { Db_cache_types.Stat.created; modified; deleted } _ acc ->
                let s =
                  try
                    let row = Db_cache_types.Table.find r table in
                    let s = Db_cache_types.Row.fold_over_recent g
                        (fun k _ v acc ->
                           Printf.sprintf "%s %s=%s" acc k (Schema.Value.marshal v)) row "" in
                    s
                  with _ -> "(deleted)"
                in
                Printf.printf "%s(%s): (%Ld %Ld %Ld) %s\n" name r created modified deleted s;
                ()
             ) table ())  tables ()
    in

    let get_created db g =
      let tables = Db_cache_types.Database.tableset db in
      Db_cache_types.TableSet.fold_over_recent g
        (fun name _ table acc ->
           Db_cache_types.Table.fold_over_recent g
             (fun r { Db_cache_types.Stat.created } _ acc ->
                if created>=g then (name,r)::acc else acc) table acc
        ) tables []
    in

    let get_updated db g =
      let tables = Db_cache_types.Database.tableset db in
      Db_cache_types.TableSet.fold_over_recent g
        (fun name _ table acc ->
           Db_cache_types.Table.fold_over_recent g
             (fun r _ _ acc ->
                let row = Db_cache_types.Table.find r table in
                Db_cache_types.Row.fold_over_recent g
                  (fun k _ v acc ->
                     (r,(k,v))::acc) row acc)
             table acc) tables []
    in

    let get_deleted db g =
      let tables = Db_cache_types.Database.tableset db in
      Db_cache_types.TableSet.fold_over_recent g
        (fun name _ table acc ->
           Db_cache_types.Table.fold_over_deleted g
             (fun r { Db_cache_types.Stat.deleted } acc ->
                if deleted > g then r::acc else acc)
             table acc) tables []
    in

    let get_max db =
      let tables = Db_cache_types.Database.tableset db in
      Db_cache_types.TableSet.fold_over_recent (-1L)
        (fun _ { Db_cache_types.Stat.created; modified; deleted } _ largest ->
           max created (max modified (max deleted largest))) tables (-1L)
    in

    let db = Db_ref.get_database t in
    let g = get_max db in
    Printf.printf "check_events: current generation is: %Ld\n" g;

    let vm = "vmref" in
    let vm_uuid = "vmuuid" in
    let vbd = "vbdref" in
    let vbd_uuid = "vbduuid" in
    let vbd2 = "vbdref2" in
    let vbd_uuid2 = "vbduuid2" in

    Client.create_row t "VM" (make_vm vm vm_uuid) vm;
    let db = Db_ref.get_database t in
    let g2 = get_max db in
    Printf.printf "generation after create_row is: %Ld\n" g2;
    dump db g;
    let created = get_created db g in
    Printf.printf "===TEST=== Checking that the VM creation event is reported: ";
    if (List.exists (fun (table,r) -> table="VM" && r=vm) created)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    let (_: unit) = Client.write_field t "VM" vm name_label "moo" in
    let db = Db_ref.get_database t in
    let g3 = get_max db in
    Printf.printf "generation after write_field is: %Ld\n" g3;
    dump db g2;
    let updated = get_updated db g2 in
    let vm_updated = List.filter (fun (r,_) -> r=vm) updated in
    let vm_updated = List.map snd vm_updated in
    Printf.printf "===TEST=== Checking that the VM field update is reported: ";
    if (List.mem_assoc name_label vm_updated)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    Client.create_row t "VBD" (make_vbd vm vbd vbd_uuid) vbd;
    let db = Db_ref.get_database t in
    let g4 = get_max db in
    Printf.printf "generation after create VBD is: %Ld\n" g4;
    dump db g3;
    let updated = get_updated db g3 in
    Printf.printf "===TEST=== Checking one-to-many after one-create: ";
    let vm_updated = List.filter (fun (r,_) -> r=vm) updated in
    let vm_updated = List.map snd vm_updated in
    if (List.mem_assoc "VBDs" vm_updated)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    let (_: unit) = Client.write_field t "VBD" vbd "VM" "moo" in
    let db = Db_ref.get_database t in
    let g5 = get_max db in
    Printf.printf "generation after write_field is: %Ld\n" g5;
    dump db g4;
    let updated = get_updated db g4 in
    Printf.printf "===TEST=== Checking one-to-many after one-update: ";
    let vm_updated = List.filter (fun (r,_) -> r=vm) updated in
    let vm_updated = List.map snd vm_updated in
    if (List.mem_assoc "VBDs" vm_updated)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    let (_: unit) = Client.write_field t "VBD" vbd "type" "Banana" in
    let db = Db_ref.get_database t in
    let g6 = get_max db in
    Printf.printf "generation after write_field is: %Ld\n" g6;
    dump db g5;
    let updated = get_updated db g5 in
    Printf.printf "===TEST=== Checking one-to-many after one-update of non-reference field: ";
    let vm_updated = List.filter (fun (r,_) -> r=vm) updated in
    let vm_updated = List.map snd vm_updated in
    if not (List.mem_assoc "VBDs" vm_updated)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    let (_ : unit) = Client.delete_row t "VBD" vbd in
    let db = Db_ref.get_database t in
    let g7 = get_max db in
    Printf.printf "generation after delete VBD is: %Ld\n" g7;
    Printf.printf "===TEST=== Checking deleted event: ";
    let deleted = get_deleted db g6 in
    if (List.mem vbd deleted)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");

    Client.create_row t "VBD" (make_vbd vm vbd vbd_uuid) vbd;
    let (_ : unit) = Client.delete_row t "VBD" vbd in
    let db = Db_ref.get_database t in
    let g8 = get_max db in
    Printf.printf "generation after create/delete VBD is: %Ld\n" g8;
    Printf.printf "===TEST=== Checking the VBD doesn't appear in the deleted list: ";
    let deleted = get_deleted db g7 in
    if not (List.mem vbd deleted)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");
    dump db g7;

    Client.create_row t "VBD" (make_vbd vm vbd vbd_uuid) vbd;
    let db = Db_ref.get_database t in
    let g9 = get_max db in
    let (_ : unit) = Client.delete_row t "VBD" vbd in
    Client.create_row t "VBD" (make_vbd vm vbd2 vbd_uuid2) vbd2;
    let (_ : unit) = Client.delete_row t "VBD" vbd2 in
    let db = Db_ref.get_database t in
    let g10 = get_max db in

    Printf.printf "===TEST=== Checking for masking of delete events: ";


    let deleted = get_deleted db g9 in
    if (List.mem vbd deleted)
    then (Printf.printf "Pass\n")
    else (Printf.printf "Fail\n"; failwith "Event problem");
    dump db g9;
    ignore(g10);



    ()

  let main in_process =
    (* reference which we create *)
    let valid_ref = "ref1" in
    let valid_uuid = "uuid1" in
    let invalid_ref = "foo" in
    let invalid_uuid = "bar" in

    let t =
      if in_process
      then begin
        Db_backend.make ()
      end else begin
        Db_ref.Remote
      end
    in

    let vbd_ref = "waz" in
    let vbd_uuid = "whatever" in

    check_many_to_many ();

    (* Before we begin, clear out any old state: *)
    expect_missing_row "VM" valid_ref
      (fun () ->
         Client.delete_row t "VM" valid_ref;
      );
    if in_process then check_ref_index t "VM" valid_ref;

    expect_missing_row "VBD" vbd_ref
      (fun () ->
         Client.delete_row t "VBD" vbd_ref;
      );
    if in_process then check_ref_index t "VBD" vbd_ref;

    Printf.printf "Deleted stale state from previous test\n";

    Printf.printf "get_table_from_ref <invalid ref>\n";
    begin
      match Client.get_table_from_ref t invalid_ref with
      | None -> Printf.printf "Reference '%s' has no associated table\n" invalid_ref
      | Some t -> failwith (Printf.sprintf "Reference '%s' exists in table '%s'" invalid_ref t)
    end;
    Printf.printf "is_valid_ref <invalid_ref>\n";
    if Client.is_valid_ref t invalid_ref then failwith "is_valid_ref <invalid_ref> = true";

    Printf.printf "read_refs <valid tbl>\n";
    let existing_refs = Client.read_refs t "VM" in
    Printf.printf "VM refs: [ %s ]\n" (String.concat "; " existing_refs);
    Printf.printf "read_refs <invalid tbl>\n";
    expect_missing_tbl "Vm"
      (fun () ->
         let (_: string list) = Client.read_refs t "Vm" in
         ()
      );
    Printf.printf "delete_row <invalid ref>\n";
    expect_missing_row "VM" invalid_ref
      (fun () ->
         Client.delete_row t "VM" invalid_ref;
         failwith "delete_row of a non-existent row silently succeeded"
      );
    Printf.printf "create_row <unique ref> <unique uuid> <missing required field>\n";
    expect_missing_field name_label
      (fun () ->
         let broken_vm = List.filter (fun (k, _) -> k <> name_label) (make_vm valid_ref valid_uuid) in
         Client.create_row t "VM" broken_vm valid_ref;
         failwith "create_row <unique ref> <unique uuid> <missing required field>"
      );
    Printf.printf "create_row <unique ref> <unique uuid>\n";
    Client.create_row t "VM" (make_vm valid_ref valid_uuid) valid_ref;
    if in_process then check_ref_index t "VM" valid_ref;
    Printf.printf "is_valid_ref <valid ref>\n";
    if not (Client.is_valid_ref t valid_ref)
    then failwith "is_valid_ref <valid_ref> = false, after create_row";
    Printf.printf "get_table_from_ref <valid ref>\n";
    begin match Client.get_table_from_ref t valid_ref with
      | Some "VM" -> ()
      | Some t -> failwith "get_table_from_ref <valid ref> : invalid table"
      | None -> failwith "get_table_from_ref <valid ref> : None"
    end;
    Printf.printf "read_refs includes <valid ref>\n";
    if not (List.mem valid_ref (Client.read_refs t "VM"))
    then failwith "read_refs did not include <valid ref>";

    Printf.printf "create_row <duplicate ref> <unique uuid>\n";
    expect_uniqueness_violation "VM" "_ref" valid_ref
      (fun () ->
         Client.create_row t "VM" (make_vm valid_ref (valid_uuid ^ "unique")) valid_ref;
         failwith "create_row <duplicate ref> <unique uuid>"
      );
    Printf.printf "create_row <unique ref> <duplicate uuid>\n";
    expect_uniqueness_violation "VM" "uuid" valid_uuid
      (fun () ->
         Client.create_row t "VM" (make_vm (valid_ref ^ "unique") valid_uuid) (valid_ref ^ "unique");
         failwith "create_row <unique ref> <duplicate uuid>"
      );
    Printf.printf "db_get_by_uuid <valid uuid>\n";
    let r = Client.db_get_by_uuid t "VM" valid_uuid in
    if r <> valid_ref
    then failwith (Printf.sprintf "db_get_by_uuid <valid uuid>: got %s; expected %s" r valid_ref);
    Printf.printf "db_get_by_uuid <invalid uuid>\n";
    expect_missing_uuid "VM" invalid_uuid
      (fun () ->
         let (_: string) = Client.db_get_by_uuid t "VM" invalid_uuid in
         failwith "db_get_by_uuid <invalid uuid>"
      );
    Printf.printf "get_by_name_label <invalid name label>\n";
    if Client.db_get_by_name_label t "VM" invalid_name <> []
    then failwith "db_get_by_name_label <invalid name label>";

    Printf.printf "get_by_name_label <valid name label>\n";
    if Client.db_get_by_name_label t "VM" name <> [ valid_ref ]
    then failwith "db_get_by_name_label <valid name label>";

    Printf.printf "read_field <valid field> <valid objref>\n";
    if Client.read_field t "VM" name_label valid_ref <> name
    then failwith "read_field <valid field> <valid objref> : invalid name";

    Printf.printf "read_field <valid defaulted field> <valid objref>\n";
    if Client.read_field t "VM" "protection_policy" valid_ref <> "OpaqueRef:NULL"
    then failwith "read_field <valid defaulted field> <valid objref> : invalid protection_policy";

    Printf.printf "read_field <valid field> <invalid objref>\n";
    expect_missing_row "VM" invalid_ref
      (fun () ->
         let (_: string) = Client.read_field t "VM" name_label invalid_ref in
         failwith "read_field <valid field> <invalid objref>"
      );
    Printf.printf "read_field <invalid field> <valid objref>\n";
    expect_missing_field "name_label"
      (fun () ->
         let (_: string) = Client.read_field t "VM" "name_label" valid_ref in
         failwith "read_field <invalid field> <valid objref>"
      );
    Printf.printf "read_field <invalid field> <invalid objref>\n";
    expect_missing_row "VM" invalid_ref
      (fun () ->
         let (_: string) = Client.read_field t "VM" "name_label" invalid_ref in
         failwith "read_field <invalid field> <invalid objref>"
      );
    Printf.printf "read_field_where <valid table> <valid return> <valid field> <valid value>\n";
    let where_name_label =
      { Db_cache_types.table = "VM"; return = name_label; where_field="uuid"; where_value = valid_uuid } in
    let xs = Client.read_field_where t where_name_label in
    if not (List.mem name xs)
    then failwith "read_field_where <valid table> <valid return> <valid field> <valid value>";
    test_invalid_where_record "read_field_where" (Client.read_field_where t);

    Printf.printf "write_field <invalid table>\n";
    expect_missing_tbl "Vm"
      (fun () ->
         let (_: unit) = Client.write_field t "Vm" "" "" "" in
         failwith "write_field <invalid table>"
      );
    Printf.printf "write_field <valid table> <invalid ref>\n";
    expect_missing_row "VM" invalid_ref
      (fun () ->
         let (_: unit) = Client.write_field t "VM" invalid_ref name_label "" in
         failwith "write_field <valid table> <invalid ref>"
      );
    Printf.printf "write_field <valid table> <valid ref> <invalid field>\n";
    expect_missing_column "wibble"
      (fun () ->
         let (_: unit) = Client.write_field t "VM" valid_ref "wibble" "" in
         failwith "write_field <valid table> <valid ref> <invalid field>"
      );
    Printf.printf "write_field <valid table> <valid ref> <valid field>\n";
    let (_: unit) = Client.write_field t "VM" valid_ref name_description "description" in
    if in_process then check_ref_index t "VM" valid_ref;
    Printf.printf "write_field <valid table> <valid ref> <valid field> - invalidating ref_index\n";
    let (_: unit) = Client.write_field t "VM" valid_ref name_label "newlabel" in
    if in_process then check_ref_index t "VM" valid_ref;

    Printf.printf "read_record <invalid table> <invalid ref>\n";
    expect_missing_tbl "Vm"
      (fun () ->
         let _ = Client.read_record t "Vm" invalid_ref in
         failwith "read_record <invalid table> <invalid ref>"
      );
    Printf.printf "read_record <valid table> <valid ref>\n";
    expect_missing_row "VM" invalid_ref
      (fun () ->
         let _ = Client.read_record t "VM" invalid_ref in
         failwith "read_record <valid table> <invalid ref>"
      );
    Printf.printf "read_record <valid table> <valid ref>\n";
    let fv_list, fvs_list = Client.read_record t "VM" valid_ref in
    if not(List.mem_assoc name_label fv_list)
    then failwith "read_record <valid table> <valid ref> 1";
    if List.assoc "VBDs" fvs_list <> []
    then failwith "read_record <valid table> <valid ref> 2";
    Printf.printf "read_record <valid table> <valid ref> foreign key\n";
    Client.create_row t "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref;
    let fv_list, fvs_list = Client.read_record t "VM" valid_ref in
    if List.assoc "VBDs" fvs_list <> [ vbd_ref ] then begin
      Printf.printf "fv_list = [ %s ] fvs_list = [ %s ]\n%!" (String.concat "; " (List.map (fun (k, v) -> k ^":" ^ v) fv_list))  (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ (String.concat ", " v)) fvs_list));
      failwith "read_record <valid table> <valid ref> 3"
    end;
    Printf.printf "read_record <valid table> <valid ref> deleted foreign key\n";
    Client.delete_row t "VBD" vbd_ref;
    let fv_list, fvs_list = Client.read_record t "VM" valid_ref in
    if List.assoc "VBDs" fvs_list <> []
    then failwith "read_record <valid table> <valid ref> 4";
    Printf.printf "read_record <valid table> <valid ref> overwritten foreign key\n";
    Client.create_row t "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref;
    let fv_list, fvs_list = Client.read_record t "VM" valid_ref in
    if List.assoc "VBDs" fvs_list = []
    then failwith "read_record <valid table> <valid ref> 5";
    Client.write_field t "VBD" vbd_ref "VM" "overwritten";
    let fv_list, fvs_list = Client.read_record t "VM" valid_ref in
    if List.assoc "VBDs" fvs_list <> []
    then failwith "read_record <valid table> <valid ref> 6";

    expect_missing_tbl "Vm"
      (fun () ->
         let _ = Client.read_records_where t "Vm" Db_filter_types.True in
         ()
      );
    let xs = Client.read_records_where t "VM" Db_filter_types.True in
    if List.length xs <> 1
    then failwith "read_records_where <valid table> 2";
    let xs = Client.read_records_where t "VM" Db_filter_types.False in
    if xs <> []
    then failwith "read_records_where <valid table> 3";

    expect_missing_tbl "Vm"
      (fun () ->
         let _ = Client.find_refs_with_filter t "Vm" Db_filter_types.True in
         failwith "find_refs_with_filter <invalid table>";
      );
    let xs = Client.find_refs_with_filter t "VM" Db_filter_types.True in
    if List.length xs <> 1
    then failwith "find_refs_with_filter <valid table> 1";
    let xs = Client.find_refs_with_filter t "VM" Db_filter_types.False in
    if xs <> []
    then failwith "find_refs_with_filter <valid table> 2";

    expect_missing_tbl "Vm"
      (fun () ->
         Client.process_structured_field t ("","") "Vm" "wibble" invalid_ref Db_cache_types.AddSet;
         failwith "process_structure_field <invalid table> <invalid fld> <invalid ref>"
      );
    expect_missing_field "wibble"
      (fun () ->
         Client.process_structured_field t ("","") "VM" "wibble" valid_ref Db_cache_types.AddSet;
         failwith "process_structure_field <valid table> <invalid fld> <valid ref>"
      );
    expect_missing_row "VM" invalid_ref
      (fun () ->
         Client.process_structured_field t ("","") "VM" name_label invalid_ref Db_cache_types.AddSet;
         failwith "process_structure_field <valid table> <valid fld> <invalid ref>"
      );
    Client.process_structured_field t ("foo", "") "VM" "tags" valid_ref Db_cache_types.AddSet;
    if Client.read_field t "VM" "tags" valid_ref <> "('foo')"
    then failwith "process_structure_field expected ('foo')";
    Client.process_structured_field t ("foo", "") "VM" "tags" valid_ref Db_cache_types.AddSet;
    if Client.read_field t "VM" "tags" valid_ref <> "('foo')"
    then failwith "process_structure_field expected ('foo') 2";
    Client.process_structured_field t ("foo", "bar") "VM" "other_config" valid_ref Db_cache_types.AddMap;

    if Client.read_field t "VM" "other_config" valid_ref <> "(('foo' 'bar'))"
    then failwith "process_structure_field expected (('foo' 'bar')) 3";

    begin
      try
        Client.process_structured_field t ("foo", "bar") "VM" "other_config" valid_ref Db_cache_types.AddMap;
      with Db_exn.Duplicate_key("VM", "other_config", r', "foo") when r' = valid_ref -> ()
    end;
    if Client.read_field t "VM" "other_config" valid_ref <> "(('foo' 'bar'))"
    then failwith "process_structure_field expected (('foo' 'bar')) 4";

    (* Check that non-persistent fields are filled with an empty value *)

    (* Event tests *)

    check_events t;

    (* Performance test *)
    if in_process then begin
      let time n f =
        let start = Unix.gettimeofday () in
        for i = 0 to n do
          f i
        done;
        let total = Unix.gettimeofday () -. start in
        float_of_int n /. total in

      let n = 5000 in

      let rpc_time = time n (fun _ ->
          let (_: bool) = Client.is_valid_ref t valid_ref in ()) in

      Printf.printf "%.2f primitive RPC calls/sec\n" rpc_time;

      (* Delete stuff left-over from the previous run *)
      let delete_time = time n
          (fun i ->
             let rf = Printf.sprintf "%s:%d" vbd_ref i in
             try
               Client.delete_row t "VBD" rf
             with _ -> ()
          ) in
      Printf.printf "Deleted %d VBD records, %.2f calls/sec\n%!" n delete_time;

      expect_missing_row "VBD" vbd_ref
        (fun () ->
           Client.delete_row t "VBD" vbd_ref;
        );

      (* Create lots of VBDs referening no VM *)
      let create_time = time n
          (fun i ->
             let rf = Printf.sprintf "%s:%d" vbd_ref i in
             let uuid = Printf.sprintf "%s:%d" vbd_uuid i in
             Client.create_row t "VBD" (make_vbd invalid_ref rf uuid) rf;
          ) in
      Printf.printf "Created %d VBD records, %.2f calls/sec\n%!" n create_time;

      let m = 300000 in (* multiple of 3 *)

      (* Time a benign VM create_row, delete_row, read_record sequence *)
      let benign_time = time m
          (fun i ->
             if i < (m / 3 * 2) then begin
               if i mod 2 = 0
               then Client.create_row t "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref
               else Client.delete_row t "VBD" vbd_ref
             end else
               let _ = Client.read_record t "VM" valid_ref in
               ()
          ) in
      Printf.printf "good sequence: %.2f calls/sec\n%!" benign_time;

      let malign_time = time m
          (fun i ->
             match i mod 3 with
             | 0 -> Client.create_row t "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref
             | 1 -> Client.delete_row t "VBD" vbd_ref
             | 2 -> let _ = Client.read_record t "VM" valid_ref in ()
             | _ -> ()
          ) in
      Printf.printf "bad sequence: %.2f calls/sec\n%!" malign_time;
    end
end

