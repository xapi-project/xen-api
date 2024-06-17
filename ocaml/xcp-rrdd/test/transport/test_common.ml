let test_payload =
  Rrd_protocol.
    {
      timestamp= 1387867223L
    ; datasources=
        [
          ( Rrd.Host
          , Ds.ds_make ~name:"test_int1" ~description:"A test integer"
              ~value:(Rrd.VT_Int64 1234L) ~ty:Rrd.Gauge ~default:false
              ~units:"things" ()
          )
        ; ( Rrd.SR "test_sr"
          , Ds.ds_make ~name:"test_int2" ~description:"A test integer"
              ~value:(Rrd.VT_Int64 5678L) ~ty:Rrd.Gauge ~default:true
              ~units:"things" ()
          )
        ; ( Rrd.VM "test_vm"
          , Ds.ds_make ~name:"test_float1" ~description:"A test float"
              ~value:(Rrd.VT_Float 987.654) ~ty:Rrd.Gauge ~default:false
              ~units:"things" ()
          )
        ]
    }

let make_random_datasource _ =
  let owner =
    if Random.bool () then
      Rrd.Host
    else if Random.bool () then
      Rrd.SR "test_sr"
    else
      Rrd.VM "test_vm"
  in
  let value =
    if Random.bool () then
      Rrd.VT_Int64 (Random.int64 1048576L)
    else
      Rrd.VT_Float (Random.float 1048576.0)
  in
  let ty =
    if Random.bool () then
      Rrd.Absolute
    else if Random.bool () then
      Rrd.Gauge
    else
      Rrd.Derive
  in
  let default = Random.bool () in
  ( owner
  , Ds.ds_make ~name:"test_ds" ~description:"A datasource" ~value ~ty ~default
      ~units:"things" ()
  )

let make_random_payload timestamp datasource_count =
  let datasources = List.init datasource_count make_random_datasource in
  Rrd_protocol.{timestamp; datasources}

(* pick between absolute or relative tolerance of a number *)
let tolerance x = max 1e-4 (abs_float x *. 1e-12)

let compare_float message x y =
  Alcotest.(check @@ float @@ tolerance x) message x y

let equal_value eps a b =
  match (a, b) with
  | Rrd.VT_Int64 a, Rrd.VT_Int64 b ->
      Int64.equal a b
  | Rrd.VT_Unknown, Rrd.VT_Unknown ->
      true
  | Rrd.VT_Float a, Rrd.VT_Float b ->
      let isnan f = FP_nan = classify_float f in
      (isnan a && isnan b)
      (* compare infinities *)
      || a = b
      || abs_float (a -. b) <= eps
  | _, _ ->
      false

let equal_owner a b =
  match (a, b) with
  | Rrd.Host, Rrd.Host ->
      true
  | Rrd.VM a, Rrd.VM b | Rrd.SR a, Rrd.SR b ->
      String.equal a b
  | _ ->
      false

let print_owner = function
  | Rrd.Host ->
      "Host"
  | Rrd.VM vm ->
      "VM " ^ vm
  | Rrd.SR sr ->
      "SR " ^ sr

let owner = Alcotest.testable (Fmt.of_to_string print_owner) equal_owner

let print_string x = x

let print_type = function
  | Rrd.Absolute ->
      "Absolute"
  | Rrd.Derive ->
      "Derive"
  | Rrd.Gauge ->
      "Gauge"

let print_value = function
  | Rrd.VT_Float x ->
      "Float " ^ string_of_float x
  | Rrd.VT_Int64 x ->
      "Int64 " ^ Int64.to_string x
  | Rrd.VT_Unknown ->
      "Unknown"

let value e = Alcotest.testable (Fmt.of_to_string print_value) (equal_value e)

let assert_ds_equal d1 d2 =
  let open Ds in
  Alcotest.(check string) "Names match" d1.ds_name d2.ds_name ;
  Alcotest.(check string)
    "Descriptions match" d1.ds_description d2.ds_description ;
  Alcotest.check (value 0.01) "Values match" d1.ds_value d2.ds_value ;
  assert (d1.ds_type = d2.ds_type) ;
  Alcotest.(check bool) "Defaults match" d1.ds_default d2.ds_default ;
  compare_float "Minimums match" d1.ds_min d2.ds_min ;
  compare_float "Maximums match" d1.ds_max d2.ds_max ;
  Alcotest.(check string) "Units match" d1.ds_units d2.ds_units

let assert_ds_equal (owner1, ds1) (owner2, ds2) =
  Alcotest.check owner "Owners match" owner1 owner2 ;
  assert_ds_equal ds1 ds2

let assert_payloads_equal payload1 payload2 =
  let open Rrd_protocol in
  Alcotest.(check int64)
    "Timestamps match" payload1.timestamp payload2.timestamp ;
  Alcotest.(check int)
    "Number of datasources read matches written ones"
    (List.length payload1.datasources)
    (List.length payload2.datasources) ;
  List.iter2 assert_ds_equal payload1.datasources payload2.datasources

let make_shared_file ?(k = 0) () =
  Filename.temp_file ~temp_dir:"/dev/shm"
    (string_of_int k ^ "-test-metrics")
    ".tmp"

let tests_for_all_protos t =
  let on_proto name proto tests =
    (name, List.map (fun (name, t) -> (name, `Quick, fun () -> t proto)) tests)
  in
  [
    on_proto "V1" Rrd_protocol_v1.protocol t
  ; on_proto "V2" Rrd_protocol_v2.protocol t
  ]
