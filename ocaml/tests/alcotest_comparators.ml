
(** Creates a [testable] from the given pretty-printer using the polymorphic equality function *)
let from_to_string pp =
  Alcotest.testable (Fmt.of_to_string pp) (=)

(** A [testable] that compares values using the polymorphic equality and does not pretty-print them *)
let only_compare () = from_to_string (fun _ -> "<no pretty-printer>")

(** Only compares the error code of xapi errors and ignores the parameters *)
let error_code =
  let fmt = Fmt.pair Fmt.string (Fmt.list Fmt.string) in
  let equal aa bb = fst aa = fst bb in
  Alcotest.testable fmt equal

(** Creates a [testable] using OCaml's polymorphic equality and [Rpc.t] -> [string] conversion for formatting *)
let from_rpc_of_t rpc_of =
  from_to_string (fun t -> (rpc_of t) |> Rpc.to_string)

let vdi_nbd_server_info = from_rpc_of_t API.rpc_of_vdi_nbd_server_info_t

let vdi_nbd_server_info_set =
  let comp a b =
    let (>||=) a b = if a = 0 then b else a in
    let open API in
    (compare a.vdi_nbd_server_info_exportname b.vdi_nbd_server_info_exportname) >||=
    (compare a.vdi_nbd_server_info_address b.vdi_nbd_server_info_address) >||=
    (compare a.vdi_nbd_server_info_port b.vdi_nbd_server_info_port) >||=
    (compare a.vdi_nbd_server_info_cert b.vdi_nbd_server_info_cert) >||=
    (compare a.vdi_nbd_server_info_subject b.vdi_nbd_server_info_subject) >||=
    0
  in
  Alcotest.slist vdi_nbd_server_info comp

let vdi_type : (API.vdi_type Alcotest.testable) = from_rpc_of_t API.rpc_of_vdi_type

let db_cache_structured_op = from_rpc_of_t Db_cache_types.rpc_of_structured_op_t

let db_rpc_request = from_rpc_of_t Db_rpc_common_v2.Request.rpc_of_t

let ref () =
  from_to_string Ref.string_of

let assert_raises_match exception_match fn =
  try
    fn ();
    Alcotest.fail "assert_raises_match: failure expected"
  with failure ->
    if not (exception_match failure)
    then raise failure
    else ()
