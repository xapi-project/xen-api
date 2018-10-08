(*
 * Copyright (C) Citrix Systems Inc.
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

open Test_common
open Event_types

let get_gen db =
  db
  |> Db_cache_types.Database.manifest
  |> Db_cache_types.Manifest.generation

let check_db_equals dba dbb =
  let _gen = get_gen dba = get_gen dbb in
  let _man = Db_cache_types.Database.manifest dba = Db_cache_types.Database.manifest dbb in
  let _ts = Db_cache_types.Database.tableset dba = Db_cache_types.Database.tableset dbb in
  _gen && _man && _ts

let get_tables db =
  Db_cache_types.TableSet.fold_over_recent (-2L) (fun table stat value acc -> table::acc) (Db_cache_types.Database.tableset db) []

let sort_and_flatten l =
  let sorted = List.sort compare (List.map (List.sort compare) l) in
  let flattened = List.sort compare (List.flatten sorted) in
  String.concat ", " flattened

let dump db =
  Db_cache_types.TableSet.iter (fun tblname table ->
      Printf.printf "\n# TABLE: %s\n\n" tblname;
      Db_cache_types.Table.iter (fun objref row ->
          Printf.printf "## Object: %s\n" objref;
          Db_cache_types.Row.iter (fun fldname v ->
              Printf.printf "  %s: %s\n" fldname (Schema.Value.marshal v)) row) table) (Db_cache_types.Database.tableset db)

let stat_to_string (stat:Db_cache_types.Stat.t) =
  "stat : {created: " ^ (Int64.to_string stat.created) ^
  "; modified: " ^ (Int64.to_string stat.modified) ^
  "; deleted: " ^ (Int64.to_string stat.deleted) ^ ";}; "


let tbl_to_string (tbl:Db_cache_types.Table.t) =
  let row_func = Db_cache_types.Row.fold (fun str stat value acc -> (str ^ (stat_to_string stat) ^ (Schema.Value.marshal value)) :: acc) in
  let lofl = Db_cache_types.Table.fold (fun str stat value acc -> (row_func value []) :: acc) tbl [] in
  sort_and_flatten lofl

let tbl_eq dba dbb tbl =
  let tbl_str_a = tbl_to_string (Db_cache_types.TableSet.find tbl (Db_cache_types.Database.tableset dba)) in
  let tbl_str_b = tbl_to_string (Db_cache_types.TableSet.find tbl (Db_cache_types.Database.tableset dbb)) in
  if tbl_str_a <> tbl_str_b then
    begin
      Printf.printf "\nThe %s tables are not equal" tbl;
      Printf.printf "\nTable a: '%s'" tbl_str_a;
      Printf.printf "\nTable b: '%s'\n" tbl_str_b;
      false
    end
  else
    true

let print_field (f:Xapi_database_backup.field) =
  Printf.printf "\n    Field: %s, {%Li;%Li;%Li}" f.fldname f.stat.created f.stat.modified f.stat.deleted

let print_row (r:Xapi_database_backup.row) =
  Printf.printf "\nRow: %s, {%Li;%Li;%Li}" r.objref r.stat.created r.stat.modified r.stat.deleted;
  List.iter print_field r.fields

let t_eq (t1:Xapi_database_backup.table) (t2:Xapi_database_backup.table) =
  if t1.tblname = t2.tblname && t1.stat = t2.stat && t1.rows = t2.rows then
    true
  else
    begin
      Printf.printf "%s tables not equal:\n" t1.tblname;
      Printf.printf "table a: %s\n" (stat_to_string {created=t1.stat.created; modified=t1.stat.modified; deleted=t1.stat.deleted});
      Printf.printf "table b: %s\n\n" (stat_to_string {created=t2.stat.created; modified=t2.stat.modified; deleted=t2.stat.deleted});
      List.iter print_row t1.rows;
      Printf.printf "\n";
      List.iter print_row t2.rows;
      Printf.printf "\n";
      false
    end


let table_list_eq  tl1 tl2 =
  List.for_all2 t_eq tl1 tl2

let db_to_str db =
  let tbl_list = (get_tables db) in
  let tbl_t_list = List.map (fun tblname -> (Db_cache_types.TableSet.find tblname (Db_cache_types.Database.tableset db))) tbl_list in
  let tbl_as_str_list = List.map tbl_to_string tbl_t_list in
  String.concat ", " tbl_as_str_list

let print_db db =
  Printf.printf "DB: %s" (db_to_str db)

let dbs_are_equal dba dbb =
  List.for_all (fun x -> x) (List.map (tbl_eq dba dbb) (get_tables dba))

let test_db_backup () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let init_db = Db_ref.get_database (Context.database_of __context) in

  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  let gen_init = get_gen init_db in
  let gen_changes = get_gen changes_db in

  Alcotest.(check bool) "Test the equality check" true (dbs_are_equal init_db init_db);
  Alcotest.(check bool) "Database generations are equal" true (gen_init = gen_changes);
  Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal init_db changes_db);

  Xapi_slave_db.clear_db ()

let test_db_with_vm () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in

  let (_vm_ref: API.ref_VM) = make_vm __context () in

  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "VM generation is the same" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "VM table updates are equal" true (dbs_are_equal init_db changes_db);

  Xapi_slave_db.clear_db ()

let test_db_with_name_change () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vm = make_vm __context () in
  Db.VM.set_name_description __context vm "NewName";
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "changes are reflected in both tables' generations" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "All info in VM tables is correct" true (dbs_are_equal init_db changes_db);

  Xapi_slave_db.clear_db ()

let test_db_with_multiple_changes () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vm = make_vm __context () in
  Db.VM.set_name_description __context vm "NewName1";
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "First vm created - generations equal" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "First vm created - tables correct" true (dbs_are_equal init_db changes_db);

  let vm2 = make_vm __context () in
  Db.VM.set_name_description __context vm2 "NewName2";
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "Second vm created - generations equal" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "Second vm created - tables correct" true (dbs_are_equal init_db changes_db);

  let vm3 = make_vm __context () in
  Db.VM.set_name_description __context vm3 "NewName3";
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "Third vm created - generations equal" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "Third vm created - tables correct" true (dbs_are_equal init_db changes_db);

  Xapi_slave_db.clear_db ()

let test_db_apply () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vm = make_vm __context () in
  Db.VM.set_name_description __context vm "NewName1";
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  let tl = Xapi_database_backup.apply_changes changes in
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "First vm created - generations equal" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "First vm created - tables correct" true (dbs_are_equal init_db changes_db);

  (* Construct delta from slave_db *)
  let local_delta = ref changes in
  local_delta := {fresh_token= (get_gen changes_db);
                  last_event_token=(Int64.pred (get_gen changes_db));
                  tables=(Xapi_database_backup.check_for_updates (-2L) changes_db);
                  deletes=(Xapi_database_backup.get_deleted (-2L) changes_db tl);
                  counts=(Xapi_database_backup.object_count changes_db tl);
                 };
  let _remote_delta_str = (Xapi_database_backup.delta_to_string changes) in
  let _local_delta_str = (Xapi_database_backup.delta_to_string !local_delta) in

  Alcotest.(check bool) "Fresh_tokens are equal" true (changes.fresh_token=(!local_delta.fresh_token));
  Alcotest.(check bool) "Last event tokens are equal" true (changes.last_event_token=(!local_delta.last_event_token));
  Alcotest.(check bool) "Delta tables are equal" true (table_list_eq changes.tables (!local_delta.tables));
  Alcotest.(check bool) "Delta deletes are equal" true (changes.deletes=(!local_delta.deletes));
  Alcotest.(check bool) "Deltas are equal" true ((Xapi_database_backup.delta_to_string changes)=(Xapi_database_backup.delta_to_string !local_delta));

  Xapi_slave_db.clear_db ()

let test_db_events () =
  (* Test that events on one object create the correct number of events on other objects *)
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vma = make_vm __context () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let _changes_db = !(Xapi_slave_db.slave_db) in

  let evs = Xapi_event.from __context ["vm"] "" 30.0 |> parse_event_from in
  let tok = evs.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  Alcotest.(check int) "Creation of: dom0 vm and test vma" 2 (List.length vm_ev);

  Db.VM.set_name_description __context vma "NewName1";

  let vmb = make_vm __context () in
  let evs2 = Xapi_event.from __context ["vm"] tok 30.0 |> parse_event_from in
  let tok2 = evs2.token in
  let vm_ev2 = List.filter (fun ev -> ev.ty="vm") evs2.events in
  Alcotest.(check int) "Rename vma and creation of test vmb" 2 (List.length vm_ev2);

  let vbd = make_vbd ~__context ~vM:vma () in
  let evs3 = Xapi_event.from __context ["vm"] tok2 30.0 |> parse_event_from in
  let tok3 = evs3.token in
  let vm_ev3 = List.filter (fun ev -> ev.ty="vm") evs3.events in
  Alcotest.(check int) "Creation of vbd for vma" 1 (List.length vm_ev3);

  Db.VBD.set_VM __context vbd vmb;
  let evs4 = Xapi_event.from __context ["vm"] tok3 30.0 |> parse_event_from in
  let vm_ev4 = List.filter (fun ev -> ev.ty="vm") evs4.events in
  Alcotest.(check int) "Change vbd from vma to vmb" 2 (List.length vm_ev4);
  Xapi_slave_db.clear_db ()

let test_db_events_through_slave_db () =
  (* Test that events on one object create the correct number of events on other objects *)
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vma = make_vm __context () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let _changes_db = !(Xapi_slave_db.slave_db) in

  let classes = ["vm"] in
  let timeout = 30.0 in
  let token = "" in

  let evs = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let tok = evs.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  Alcotest.(check int) "Creation of: dom0 vm and test vma" 2 (List.length vm_ev);

  Db.VM.set_name_description __context vma "NewName1";

  let _vmb = make_vm __context () in
  let token = tok in
  let evs2 = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let vm_ev2 = List.filter (fun ev -> ev.ty="vm") evs2.events in
  Alcotest.(check int) "Rename vma and creation of test vmb" 2 (List.length vm_ev2);

  let _vif = make_vif ~__context ~vM:vma () in
  let token = evs2.token in
  let evs3 = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes:["*"] ~token ~timeout:5.) |> parse_event_from in
  let vm_ev3 = List.filter (fun ev -> ev.ty="vm") evs3.events in
  Alcotest.(check int) "Create vif" 1 (List.length vm_ev3);
  Xapi_slave_db.clear_db ()

let test_db_events_without_session () =
  (* Test that events on one object create the correct number of events on other objects *)
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let __context = Context.update_session_id None __context in
  let vma = make_vm __context () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let _changes_db = !(Xapi_slave_db.slave_db) in
  Alcotest.(check bool) "Check that we don't have a session" false (Context.has_session_id __context);

  let classes = ["vm"] in
  let timeout = 30.0 in
  let token = "" in

  let evs = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let tok = evs.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  Alcotest.(check int) "Creation of 2 vms without session" 2 (List.length vm_ev);

  Db.VM.set_name_description __context vma "NewName1";

  let _vmb = make_vm __context () in
  let token = tok in
  let evs2 = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let vm_ev2 = List.filter (fun ev -> ev.ty="vm") evs2.events in
  Alcotest.(check int) "Rename vm and make new vm" 2 (List.length vm_ev2);
  Xapi_slave_db.clear_db ()

let test_db_events_with_session () =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let rpc, session_id = Test_common.make_client_params ~__context in
  let __context = Context.update_session_id (Some session_id) __context in
  Alcotest.(check bool) "Check that we have a session" true (Context.has_session_id __context);

  let vma = make_vm __context () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let _changes_db = !(Xapi_slave_db.slave_db) in

  let classes = ["vm"] in
  let timeout = 30.0 in
  let token = "" in

  let evs = Xapi_slave_db.call_with_updated_context __context ~session_id:(Some session_id)
      (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let tok = evs.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  Alcotest.(check int) "Creation of 2 vms with session" 2 (List.length vm_ev);

  Db.VM.set_name_description __context vma "NewName1";

  let _vmb = make_vm __context () in
  let token = tok in
  let evs2 = Xapi_slave_db.call_with_updated_context __context ~session_id:(Some session_id)
      (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let vm_ev2 = List.filter (fun ev -> ev.ty="vm") evs2.events in
  Alcotest.(check int) "Rename vm and make new vm" 2 (List.length vm_ev2);
  Xapi_slave_db.clear_db ()

let test_db_counts () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let _vma = make_vm ~__context ~name_label:"vma" () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in

  let init_tl = Db_cache_types.TableSet.fold (fun table stat value acc -> table::acc) (Db_cache_types.Database.tableset _init_db) [] in
  let tl = Xapi_database_backup.apply_changes changes in
  let _changes_db = !(Xapi_slave_db.slave_db) in
  let token = changes.last_event_token in

  Alcotest.(check bool) "Created vms - object count equal" true
    (Xapi_database_backup.count_check (Xapi_database_backup.object_count _init_db init_tl) (Xapi_database_backup.object_count _changes_db tl));
  Alcotest.(check bool) "Vms created - generations equal" true (get_gen _init_db = get_gen _changes_db);
  Alcotest.(check bool) "Vms created - tables correct" true (dbs_are_equal _init_db _changes_db);

  Db.VM.destroy ~__context ~self:_vma;

  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context token in
  let init_tl = Db_cache_types.TableSet.fold (fun table stat value acc -> table::acc) (Db_cache_types.Database.tableset _init_db) [] in
  let tl = Xapi_database_backup.apply_changes changes in
  let _changes_db = !(Xapi_slave_db.slave_db) in
  let _init_db = Db_ref.get_database (Context.database_of __context) in

  Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal _init_db _changes_db);
  Alcotest.(check bool) "Deleted vms - object count equal" true
    (Xapi_database_backup.count_check (Xapi_database_backup.object_count _init_db init_tl) (Xapi_database_backup.object_count _changes_db tl));

  Xapi_slave_db.clear_db ()

let do_test_with_x_vms x =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let vm_create_count = 1 in

  let finished = ref false in

  let _vm_list = List.init vm_create_count (fun _ -> make_vm ~__context ~name_label:"Auto_created" ()) in

  let _nasty_thread = Thread.create (fun () ->
      let rec inner n =
        match n with
        | 0 -> ()
        | n ->
          let _new_vm = make_vm ~__context ~name_label:"badone_dontdelete" () in
          let new_vm = make_vm ~__context ~name_label:"badone" () in
          Db.VM.destroy ~__context ~self:new_vm;
          inner (n-1)
      in inner x;
      finished := true;
      (* One final one to wake the event thread below *)
      let _new_vm = make_vm ~__context ~name_label:"badone_dontdelete" () in
      ()
    ) () in

  let tl = ref [] in
  let rec loop token =
    let update () =
      Printf.printf "Looiping (token=%Ld)\n" token;
      let changes = Xapi_database_backup.get_delta __context token in
      tl := Xapi_database_backup.apply_changes changes;
      let token = changes.last_event_token in
      token
    in
    if !finished then update () else begin
      loop (update ())
    end
  in
  let _final_token = loop (-2L) in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let _init_tl = Db_cache_types.TableSet.fold (fun table stat value acc -> table::acc) (Db_cache_types.Database.tableset _init_db) [] in
  let _changes_db = !(Xapi_slave_db.slave_db) in
  let counts1 = Xapi_database_backup.object_count _init_db _init_tl in
  let counts2 = Xapi_database_backup.object_count _changes_db !tl in
  List.iter (fun (t,v) -> Printf.printf "ty: %s  count1: %d count2: %d\n" t v (List.assoc t counts2)) counts1;
  Alcotest.(check bool) "Created vms - object count equal" true
    (Xapi_database_backup.count_check (Xapi_database_backup.object_count _init_db _init_tl) (Xapi_database_backup.object_count _changes_db !tl));
  Alcotest.(check bool) "Vms created - generations equal" true (get_gen _init_db = get_gen _changes_db);
  Alcotest.(check bool) "Vms created - tables correct" true (dbs_are_equal _init_db _changes_db)

let test_db_vms_100 () =
  do_test_with_x_vms 100;
  Xapi_slave_db.clear_db ()

let test_vif_plug () =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let vm_create_count = 10 in
  let vif_create_count = 50 in
  let token = (-2L) in

  let rec loop token n =
    let _vm_list = List.init vm_create_count (fun _ -> make_vm ~__context ~name_label:"Auto_created" ()) in
    let _vif_list = List.init vif_create_count (fun _ -> make_vif ~__context ~vM:(List.nth _vm_list (Random.int vm_create_count)) ()) in
    let changes = Xapi_database_backup.get_delta __context (-2L) in
    ignore(Xapi_database_backup.apply_changes changes);
    let _init_db = Db_ref.get_database (Context.database_of __context) in
    let _changes_db = !(Xapi_slave_db.slave_db) in
    let gen_init = get_gen _init_db in
    let gen_changes = get_gen _changes_db in
    Alcotest.(check bool) "Database generations are equal" true (gen_init = gen_changes);
    Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal _init_db _changes_db);
    if n > 0 then loop changes.fresh_token (n-1) else ()
  in
  loop token 10;
  Xapi_slave_db.clear_db ()

let test_vdi_plug () =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let vm_create_count = 10 in
  let vdi_create_count = 50 in
  let token = (-2L) in

  let rec loop token n =
    let _vm_list = List.init vm_create_count (fun _ -> make_vm ~__context ~name_label:"Auto_created" ()) in
    let _vdi_list = List.init vdi_create_count (fun _ -> make_vdi ~__context) in
    let changes = Xapi_database_backup.get_delta __context (-2L) in
    ignore(Xapi_database_backup.apply_changes changes);
    let _init_db = Db_ref.get_database (Context.database_of __context) in
    let _changes_db = !(Xapi_slave_db.slave_db) in
    let gen_init = get_gen _init_db in
    let gen_changes = get_gen _changes_db in
    Alcotest.(check bool) "Database generations are equal" true (gen_init = gen_changes);
    Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal _init_db _changes_db);
    if n > 0 then loop changes.fresh_token (n-1) else ()
  in
  loop token 10;
  Xapi_slave_db.clear_db ()

let test_vbd_plug () =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let vm_create_count = 10 in
  let vbd_create_count = 50 in
  let token = (-2L) in

  let rec loop token n =
    let _vm_list = List.init vm_create_count (fun _ -> make_vm ~__context ~name_label:"Auto_created" ()) in
    let _vbd_list = List.init vbd_create_count (fun _ -> make_vbd ~__context ~vM:(List.nth _vm_list (Random.int vm_create_count)) ()) in
    let changes = Xapi_database_backup.get_delta __context (-2L) in
    ignore(Xapi_database_backup.apply_changes changes);
    let _init_db = Db_ref.get_database (Context.database_of __context) in
    let _changes_db = !(Xapi_slave_db.slave_db) in
    let gen_init = get_gen _init_db in
    let gen_changes = get_gen _changes_db in
    Alcotest.(check bool) "Database generations are equal" true (gen_init = gen_changes);
    Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal _init_db _changes_db);
    if n > 0 then loop changes.fresh_token (n-1) else ()
  in
  loop token 10;
  Xapi_slave_db.clear_db ()

let test_db_vm_plus_vif () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vm = make_vm __context () in
  let _vif =  make_vif ~__context ~vM:vm () in
  let init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let changes_db = !(Xapi_slave_db.slave_db) in

  Alcotest.(check bool) "changes are reflected in both tables' generations" true (get_gen init_db = get_gen changes_db);
  Alcotest.(check bool) "All info in VM tables is correct" true (dbs_are_equal init_db changes_db);

  Xapi_slave_db.clear_db ()

let test_lots_of_events () =
  Xapi_slave_db.clear_db ();

  let __context = make_test_database () in
  let vma = make_vm __context () in
  let _init_db = Db_ref.get_database (Context.database_of __context) in
  let changes = Xapi_database_backup.get_delta __context (-2L) in
  ignore(Xapi_database_backup.apply_changes changes);
  let _changes_db = !(Xapi_slave_db.slave_db) in

  let classes = ["vm"] in
  let timeout = 30.0 in
  let token = "" in

  let evs = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let tok = evs.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  Alcotest.(check int) "Creation of: dom0 vm and test vma" 2 (List.length vm_ev);

  Db.VM.set_name_description __context vma "NewName1";

  let _vmb = make_vm __context () in
  let token = tok in
  let evs2 = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes ~token ~timeout) |> parse_event_from in
  let vm_ev2 = List.filter (fun ev -> ev.ty="vm") evs2.events in
  Alcotest.(check int) "Rename vma and creation of test vmb" 2 (List.length vm_ev2);

  let rec loop token n =
    let event_count = ref 1 in
    Printf.printf "Starting loop\n";
    if Random.int 100 mod 2 = 0 then () else
      begin
        let _new_vm = make_vm __context () in
        Printf.printf "Making vm"; event_count := 2
      end;
    let _vif = make_vif ~__context ~vM:vma () in
    let events = Xapi_slave_db.call_with_updated_context __context (Xapi_event.from ~classes:["*"] ~token ~timeout:5.) |> parse_event_from in
    let vm_events = List.filter (fun ev -> ev.ty="vm") events.events in
    Alcotest.(check int) "Create vif" !event_count (List.length vm_events);
    Printf.printf "N IS: %i" n;
    let new_n = n -1 in
    Printf.printf "New n is: %i" new_n;
    if new_n > 0 then begin Printf.printf "loop"; loop (events.token) new_n end else ()
  in
  loop evs2.token 1000;
  Xapi_slave_db.clear_db ()

let test_creating_random () =
  Xapi_slave_db.clear_db ();
  let __context = make_test_database () in
  let base_vm = make_vm __context () in
  let token = (-2L) in

  let make_random () =
    match (Random.int 4) with
    | 0 ->let _ = make_vm ~__context ~name_label:"Auto_created" () in ();
    | 1 ->let _ = make_vbd ~__context ~vM:base_vm () in ();
    | 2 ->let _vif = make_vif ~__context ~vM:base_vm () in ();
    | 3 ->let _ = make_vdi ~__context in ();
    | _ -> ();
  in

  let rec loop token n =
    make_random ();
    let _init_db = Db_ref.get_database (Context.database_of __context) in
    let changes = Xapi_database_backup.get_delta __context token in
    let init_tl = Db_cache_types.TableSet.fold (fun table stat value acc -> table::acc) (Db_cache_types.Database.tableset _init_db) [] in
    let tl = Xapi_database_backup.apply_changes changes in
    let _changes_db = !(Xapi_slave_db.slave_db) in

    let gen_init = get_gen _init_db in
    let gen_changes = get_gen _changes_db in
    Printf.printf "Currently on loop %i\n" n;
    Printf.printf "Token is currently: %Li\n" token;
    Printf.printf "Gen 1: %Li" gen_init;
    Printf.printf "Gen 2: %Li" gen_changes;

    Alcotest.(check bool) "Database generations are equal" true (gen_init = gen_changes);
    Alcotest.(check bool) "Database tables are equal" true (dbs_are_equal _init_db _changes_db);
    Alcotest.(check bool) "Created vms - object count equal" true
      (Xapi_database_backup.count_check (Xapi_database_backup.object_count _init_db init_tl) (Xapi_database_backup.object_count _changes_db tl));
    if n > 0 then loop changes.last_event_token (n-1) else ()
  in
  loop token 100;
  Xapi_slave_db.clear_db ()


let test =
  [
    "test_db_backup", `Quick, test_db_backup;
    "test_db_with_vm", `Quick, test_db_with_vm;
    "test_db_with_name_change", `Quick,  test_db_with_name_change;
    "test_db_with_multiple_changes", `Quick,  test_db_with_multiple_changes;
    "test_db_apply", `Quick, test_db_apply;
    "test_db_events", `Quick, test_db_events;
    "test_db_events_through_slave_db", `Quick, test_db_events_through_slave_db;
    "test_db_events_without_session", `Quick, test_db_events_without_session;
    "test_db_vm_plus_vif", `Quick, test_db_vm_plus_vif;
    "test_vif_plug", `Quick, test_vif_plug;
    "test_vbd_plug", `Quick, test_vbd_plug;
    "test_vdi_plug", `Quick, test_vdi_plug;
    "test_creating_random", `Quick, test_creating_random;
    "test_db_events_with_session", `Quick, test_db_events_with_session;
    "test_db_counts", `Quick, test_db_counts;
    "test_lots_of_events", `Quick, test_lots_of_events;
    "test_db_vms_100", `Slow, test_db_vms_100;
  ]

