let schema =
  let _ref = {
    Schema.Column.name = Db_names.ref;
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in
  let uuid = {
    Schema.Column.name = Db_names.uuid;
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in
  let name_label = {
    Schema.Column.name = Db_names.name_label;
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in
  let name_description = {
    Schema.Column.name = "name__description";
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in
  let type' = {
    Schema.Column.name = "type";
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in
  let vbds = {
    Schema.Column.name = "VBDs";
    persistent = false;
    empty = Schema.Value.Set [];
    default = Some(Schema.Value.Set []);
    ty = Schema.Type.Set;
    issetref = true;
  } in
  let other_config = {
    Schema.Column.name = "other_config";
    persistent = false;
    empty = Schema.Value.Pairs [];
    default = Some(Schema.Value.Pairs []);
    ty = Schema.Type.Pairs;
    issetref = false;
  } in
  let pp = {
    Schema.Column.name = "protection_policy";
    persistent = true;
    empty = Schema.Value.String "";
    default = Some(Schema.Value.String "OpaqueRef:NULL");
    ty = Schema.Type.String;
    issetref = false;
  } in
  let tags = {
    Schema.Column.name = "tags";
    persistent = true;
    empty = Schema.Value.Set [];
    default = Some(Schema.Value.Set []);
    ty = Schema.Type.Set;
    issetref = false;
  } in
  let vm = {
    Schema.Column.name = "VM";
    persistent = true;
    empty = Schema.Value.String "";
    default = None;
    ty = Schema.Type.String;
    issetref = false;
  } in

  let vm_table = {
    Schema.Table.name = "VM";
    columns = [ _ref; uuid; name_label; vbds; pp; name_description; tags; other_config ];
    persistent = true;
  } in
  let vbd_table = {
    Schema.Table.name = "VBD";
    columns = [ _ref; uuid; vm; type' ];
    persistent = true;
  } in
  let database = {
    Schema.Database.tables = [ vm_table; vbd_table ];
  } in
  let one_to_many = Schema.ForeignMap.add "VBD" [ "VM", "VM", "VBDs" ] (Schema.ForeignMap.empty) in
  {

    Schema.major_vsn = 1;
    minor_vsn = 1;
    database = database;
    (** indexed by table name, a list of (this field, foreign table, foreign field) *)
    one_to_many = one_to_many;
    many_to_many = Schema.ForeignMap.empty;
  }


let many_to_many =
  let bar_column = { Schema.Column.name = "bars";
                     persistent = false;
                     empty = Schema.Value.Pairs [];
                     default = None;
                     ty = Schema.Type.Pairs;
                     issetref = false;
                   } in
  let foo_column = { bar_column with Schema.Column.name = "foos" } in
  let foo_table = { Schema.Table.name = "foo"; columns = [ bar_column ]; persistent = true } in
  let bar_table = { Schema.Table.name = "bar"; columns = [ foo_column ]; persistent = true } in

  let database = { Schema.Database.tables = [ foo_table; bar_table ] } in
  let many_to_many =
    Schema.ForeignMap.add "foo" [ "bars", "bar", "foos" ]
      (Schema.ForeignMap.add "bar" [ "foos", "foo", "bars" ]
         Schema.ForeignMap.empty) in
  let schema = { Schema.empty with
                 Schema.database = database;
                 many_to_many = many_to_many
               } in
  schema
