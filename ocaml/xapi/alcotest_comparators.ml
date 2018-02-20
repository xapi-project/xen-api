
(** Only compares the error code of xapi errors and ignores the parameters *)
let error_code =
  let fmt = Fmt.pair Fmt.string (Fmt.list Fmt.string) in
  let equal aa bb = fst aa = fst bb in
  Alcotest.testable fmt equal

let vdi_nbd_server_info =
  let to_string =
    function
      API.{
        vdi_nbd_server_info_exportname = e;
        vdi_nbd_server_info_address = a;
        vdi_nbd_server_info_port = p;
        vdi_nbd_server_info_cert = c;
        vdi_nbd_server_info_subject = s
      } -> Printf.sprintf "%s %s %Ld %s %s" e a p c s
  in
  let fmt = Fmt.of_to_string to_string in
  let equal a b =
    let open API in
    (a.vdi_nbd_server_info_exportname = b.vdi_nbd_server_info_exportname) &&
    (a.vdi_nbd_server_info_address = b.vdi_nbd_server_info_address) &&
    (a.vdi_nbd_server_info_port = b.vdi_nbd_server_info_port) &&
    (a.vdi_nbd_server_info_cert = b.vdi_nbd_server_info_cert) &&
    (a.vdi_nbd_server_info_subject = b.vdi_nbd_server_info_subject)
  in
  Alcotest.testable fmt equal

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

let vdi_type =
  let fmt : API.vdi_type Fmt.t = Fmt.of_to_string (fun t -> t |> API.rpc_of_vdi_type |> Rpc.to_string) in
  Alcotest.testable fmt (=)

let ref () =
  let fmt = Fmt.of_to_string Ref.string_of in
  let cmp = (=) in
  Alcotest.testable fmt cmp
