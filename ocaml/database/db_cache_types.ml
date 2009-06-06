type row = (string, string) Hashtbl.t
type table = (string, row) Hashtbl.t
type cache = (string, table) Hashtbl.t

type where_record = {table:string; return:string; where_field:string; where_value:string}
type structured_op_t = AddSet | RemoveSet | AddMap | RemoveMap

let string_of_structured_op op = match op with
  | AddSet -> "add_set"
  | RemoveSet -> "remove_set"
  | AddMap -> "add_map"
  | RemoveMap -> "remove_map"

type db_dump_manifest =
    {
      installation_uuid : string;
      control_domain_uuid : string;
      pool_conf : string;
      pool_token : string;
      schema_major_vsn : int;
      schema_minor_vsn : int;
      product_version : string;
      product_brand : string;
      build_number : string;
      xapi_major_vsn : int;
      xapi_minor_vsn : int;
      generation_count : Generation.t
    }

let gen_manifest gen_count =
  {
    installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid;
    control_domain_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid;
    pool_conf = Unixext.read_whole_file_to_string Xapi_globs.pool_config_file;
    pool_token = Unixext.read_whole_file_to_string Xapi_globs.pool_secret_path;
    schema_major_vsn = Datamodel.schema_major_vsn;
    schema_minor_vsn = Datamodel.schema_minor_vsn;
    product_version = Version.product_version;
    product_brand = Version.product_brand;
    build_number = Version.build_number;
    xapi_major_vsn = Xapi_globs.version_major;
    xapi_minor_vsn = Xapi_globs.version_minor;
    generation_count = gen_count
  }
