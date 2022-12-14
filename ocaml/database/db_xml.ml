(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

(** Functions to marshall/unmarshall the database as XML *)

exception Unmarshall_error of string

let persist_generation_counts = true

let name x = ("", x) (* no namespace *)

let make_tag n attrs : Xmlm.tag =
  (name n, List.map (fun (k, v) -> (name k, v)) attrs)

let _schema_major_vsn = "schema_major_vsn"

let _schema_minor_vsn = "schema_minor_vsn"

let _generation_count = "generation_count"

module To = struct
  (* Write out a key/value pair *)
  let pair (output : Xmlm.output) (key : string) (v : string) =
    Xmlm.output output (`El_start (make_tag "pair" [("key", key); ("value", v)])) ;
    Xmlm.output output `El_end

  (* Write out a string *)
  let string (output : Xmlm.output) (key : string) (x : string) =
    pair output key x

  (* Write out an int *)
  let int (output : Xmlm.output) (key : string) (x : int) =
    pair output key (string_of_int x)

  (* Write out an int64 *)
  let int64 (output : Xmlm.output) (key : string) (x : Int64.t) =
    pair output key (Int64.to_string x)

  (* Marshal a whole database table to an Xmlm output abstraction *)
  let table schema (output : Xmlm.output) name (tbl : Table.t) =
    let record rf {Stat.created; modified; _} (row : Row.t) _ =
      let preamble =
        if persist_generation_counts then
          [
            ("__mtime", Generation.to_string modified)
          ; ("__ctime", Generation.to_string created)
          ; ("ref", rf)
          ]
        else
          [("ref", rf)]
      in
      let (tag : Xmlm.tag) =
        make_tag "row"
          (List.rev
             (Row.fold
                (fun k _ v acc ->
                  (k, Xml_spaces.protect (Schema.Value.marshal v)) :: acc
                )
                row preamble
             )
          )
      in
      Xmlm.output output (`El_start tag) ;
      Xmlm.output output `El_end
    in
    let tag = make_tag "table" [("name", name)] in
    Xmlm.output output (`El_start tag) ;
    (* we write a table entry whether or not the table persists, because populate happens to assume
       that all tables will be present. However, if the table is marked as "don't persist" then we
       don't write any row entries: *)
    if Schema.is_table_persistent schema name then
      Table.fold record tbl () ;
    Xmlm.output output `El_end

  (* Write out a manifest *)
  let manifest (output : Xmlm.output) (manifest : Manifest.t) : unit =
    Xmlm.output output (`El_start (make_tag "manifest" [])) ;
    let major, minor = Manifest.schema manifest in
    int output _schema_major_vsn major ;
    int output _schema_minor_vsn minor ;
    int64 output _generation_count (Manifest.generation manifest) ;
    Xmlm.output output `El_end

  (* Write out a full database *)
  let database (output : Xmlm.output) db : unit =
    Xmlm.output output (`Dtd None) ;
    Xmlm.output output (`El_start (make_tag "database" [])) ;
    manifest output (Database.manifest db) ;
    TableSet.iter (table (Database.schema db) output) (Database.tableset db) ;
    Xmlm.output output `El_end

  let fd (fd : Unix.file_descr) db : unit =
    let oc = Unix.out_channel_of_descr fd in
    database (Xmlm.make_output (`Channel oc)) db ;
    flush oc

  let file (filename : string) db : unit =
    let fdescr =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
    in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> fd fdescr db)
      (fun () -> Unix.close fdescr)
end

module From = struct
  let schema_vsn_of_manifest manifest =
    let get_vsn schema = int_of_string (List.assoc schema manifest) in
    (get_vsn _schema_major_vsn, get_vsn _schema_minor_vsn)

  let deser_row tags table tblname schema =
    let get_tags key tags = List.partition (fun ((_, k), _) -> k = key) tags in
    let get_int64 tag =
      match tag with [(_, s)] -> Int64.of_string s | _ -> 0L
    in

    let ref_l, rest = get_tags "ref" tags in
    let ctime_l, rest = get_tags "__ctime" rest in
    let mtime_l, rest = get_tags "__mtime" rest in

    let rf =
      match ref_l with
      | (_, ref) :: _ ->
          ref (* Pick up the first ref and ignore others *)
      | [] ->
          raise (Unmarshall_error "Row does not have ref attribute")
    in
    let ctime = get_int64 ctime_l in
    let mtime = get_int64 mtime_l in

    let lifecycle_state_of ~obj fld =
      let open Datamodel in
      let {fld_states; _} = StringMap.find obj all_lifecycles in
      StringMap.find fld fld_states
    in

    let row =
      List.fold_left
        (fun row ((_, k), v) ->
          let table_schema =
            Schema.Database.find tblname schema.Schema.database
          in
          try
            let do_not_load =
              try lifecycle_state_of ~obj:tblname k = Removed_s
              with Not_found ->
                D.warn "no lifetime information about %s.%s, ignoring" tblname k ;
                false
            in
            if do_not_load then (
              D.info
                {|dropping column "%s.%s": it has been removed from the datamodel|}
                tblname k ;
              row
            ) else
              let column_schema = Schema.Table.find k table_schema in
              let value =
                Schema.Value.unmarshal column_schema.Schema.Column.ty
                  (Xml_spaces.unprotect v)
              in
              let empty = column_schema.Schema.Column.empty in
              Row.update mtime k empty
                (fun _ -> value)
                (Row.add ctime k value row)
          with Not_found ->
            (* This means there's an unexpected field, fail since no field
               should ever be deleted, instead they should change their
               lifecycle state to Removed *)
            let exc =
              Unmarshall_error
                (Printf.sprintf "Unexpected column in table %s: %s" tblname k)
            in
            raise exc
        )
        Row.empty rest
    in
    Table.update mtime rf Row.empty (fun _ -> row) (Table.add ctime rf row table)

  let database schema (input : Xmlm.input) =
    let tags = Stack.create () in
    let maybe_return f accu =
      if Xmlm.eoi input then
        if Stack.is_empty tags then
          accu
        else
          raise (Unmarshall_error "Unexpected end of file")
      else
        f accu
    in
    let rec deser ((tableset, table, tblname, manifest) as acc) =
      match Xmlm.input input with
      (* On reading a start tag... *)
      | `El_start (tag : Xmlm.tag) -> (
          Stack.push tag tags ;
          match tag with
          | (_, ("database" | "manifest")), _ ->
              deser acc
          | (_, "table"), [((_, "name"), tblname)] ->
              deser (tableset, Table.empty, tblname, manifest)
          | (_, "row"), rest ->
              let table = deser_row rest table tblname schema in
              deser (tableset, table, tblname, manifest)
          | (_, "pair"), [((_, "key"), k); ((_, "value"), v)] ->
              deser (tableset, table, tblname, (k, v) :: manifest)
          | (_, name), _ ->
              raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
        )
      (* On reading an end tag... *)
      | `El_end -> (
          let tag = Stack.pop tags in
          match tag with
          | (_, ("database" | "manifest" | "row" | "pair")), _ ->
              maybe_return deser acc
          | (_, "table"), [((_, "name"), name)] ->
              maybe_return deser
                (TableSet.add 0L name table tableset, Table.empty, "", manifest)
          | (_, name), _ ->
              raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
        )
      | _ ->
          deser acc
    in
    let ts, _, _, manifest = deser (TableSet.empty, Table.empty, "", []) in
    let g = Int64.of_string (List.assoc _generation_count manifest) in
    let major_vsn, minor_vsn = schema_vsn_of_manifest manifest in
    let manifest = Manifest.make major_vsn minor_vsn g in
    Database.make schema
    |> Database.update_tableset (fun _ -> ts)
    |> Database.update_manifest (fun _ -> manifest)

  let file schema xml_filename =
    let input = open_in xml_filename in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> database schema (Xmlm.make_input (`Channel input)))
      (fun () -> close_in input)

  let channel schema inchan = database schema (Xmlm.make_input (`Channel inchan))
end
