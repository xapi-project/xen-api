open OUnit

let test_payload = Rrd_protocol.({
    timestamp = 1387867223L;
    datasources = [
      Rrd.Host,
      Ds.ds_make ~name:"test_int1"
        ~description:"A test integer"
        ~value:(Rrd.VT_Int64 1234L)
        ~ty:Rrd.Gauge
        ~default:false
        ~units:"things" ();
      Rrd.SR "test_sr",
      Ds.ds_make ~name:"test_int2"
        ~description:"A test integer"
        ~value:(Rrd.VT_Int64 5678L)
        ~ty:Rrd.Gauge
        ~default:true
        ~units:"things" ();
      Rrd.VM "test_vm",
      Ds.ds_make ~name:"test_float1"
        ~description:"A test float"
        ~value:(Rrd.VT_Float 987.654)
        ~ty:Rrd.Gauge
        ~default:false
        ~units:"things" ();
    ];
  })

let make_list make_one count =
  let rec make_list make_one acc = function
    | count when count <= 0 -> acc
    | count ->
      let thing = make_one () in
      make_list make_one (thing :: acc) (count - 1)
  in
  make_list make_one [] count

let make_random_datasource () =
  let owner =
    if Random.bool ()
    then Rrd.Host
    else begin
      if Random.bool ()
      then Rrd.SR "test_sr"
      else Rrd.VM "test_vm"
    end
  in
  let value =
    if Random.bool ()
    then Rrd.VT_Int64 (Random.int64 1048576L)
    else Rrd.VT_Float (Random.float 1048576.0)
  in
  let ty =
    if Random.bool ()
    then Rrd.Absolute
    else begin
      if Random.bool ()
      then Rrd.Gauge
      else Rrd.Derive
    end
  in
  let default = Random.bool () in
  owner,
  Ds.ds_make ~name:"test_ds"
    ~description:"A datasource"
    ~value
    ~ty
    ~default
    ~units:"things" ()

let make_random_payload timestamp datasource_count =
  let datasources = make_list make_random_datasource datasource_count in
  Rrd_protocol.({
      timestamp;
      datasources;
    })

let are_value_types_equal value1 value2 =
  match value1, value2 with
  | Rrd.VT_Int64 a, Rrd.VT_Int64 b -> a = b
  | Rrd.VT_Unknown, Rrd.VT_Unknown -> true
  | Rrd.VT_Float a, Rrd.VT_Float b ->
    let diff = abs_float (a -. b) in
    diff <= 0.01
  | _, _ -> false

let print_owner = function
  | Rrd.Host -> "Host"
  | Rrd.VM vm -> "VM " ^ vm
  | Rrd.SR sr -> "SR " ^ sr
let print_string x = x
let print_type = function
  | Rrd.Absolute -> "Absolute"
  | Rrd.Derive -> "Derive"
  | Rrd.Gauge -> "Gauge"
let print_value_type = function
  | Rrd.VT_Float x -> "Float " ^ (string_of_float x)
  | Rrd.VT_Int64 x -> "Float " ^ (Int64.to_string x)
  | Rrd.VT_Unknown -> "Unknown"

let assert_ds_equal (owner1, ds1) (owner2, ds2) =
  assert_equal ~printer:print_owner owner1 owner2;
  let open Ds in
  assert_equal ~printer:print_string ds1.ds_name ds2.ds_name;
  assert_equal ~printer:print_string ds1.ds_description ds2.ds_description;
  assert_equal
    ~cmp:are_value_types_equal
    ~printer:print_value_type
    ds1.ds_value
    ds2.ds_value;
  assert_equal ~printer:print_type ds1.ds_type ds2.ds_type;
  assert_equal ~printer:string_of_bool ds1.ds_default ds2.ds_default;
  assert_equal ~printer:string_of_float ds1.ds_min ds2.ds_min;
  assert_equal ~printer:string_of_float ds1.ds_max ds2.ds_max;
  assert_equal ~printer:print_string ds1.ds_units ds2.ds_units

let assert_payloads_equal payload1 payload2 =
  Rrd_protocol.(
    assert_equal
      ~msg:"Incorrect timestamp read"
      ~printer:Int64.to_string
      payload1.timestamp
      payload2.timestamp;
    assert_equal
      ~msg:"Incorrect number of datasources read"
      ~printer:string_of_int
      (List.length payload1.datasources)
      (List.length payload2.datasources);
    List.iter2
      assert_ds_equal
      payload1.datasources
      payload2.datasources)

let make_shared_file () =
  Filename.temp_file ~temp_dir:"/dev/shm" "test-metrics" ".tmp"
