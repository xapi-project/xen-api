
open Quicktest_common
(* provide test record type and make_test, start,
 * debug, success, and failed test functions *)

open Client
(* provide rpc ref *)

module C = Client

let is_empty = function | [] -> true | _ -> false

(** --- Helpers for reconfiguration --- *)

let reconfigure_ipv4 ~session_id ~self ~dNS =
  let netmask = C.PIF.get_netmask ~session_id ~rpc:!rpc ~self in
  let iP = C.PIF.get_IP ~session_id ~rpc:!rpc ~self in
  let gateway = C.PIF.get_gateway ~session_id ~rpc:!rpc ~self in
  let mode = C.PIF.get_ip_configuration_mode ~session_id ~rpc:!rpc ~self in
  C.PIF.reconfigure_ip ~session_id ~rpc:!rpc ~self ~iP ~dNS ~gateway ~netmask ~mode

let reconfigure_ipv6 ~session_id ~self ~dNS =

  (* confirm valid IPv6 strings exist *)
  let iPv6_lst = (C.PIF.get_IPv6 ~session_id ~rpc:!rpc ~self) |> List.filter ((<>) "") in
  if is_empty iPv6_lst
  then Alcotest.fail "No valid IPv6 strings exist.";

  let gateway = C.PIF.get_ipv6_gateway ~session_id ~rpc:!rpc ~self in
  let mode = C.PIF.get_ipv6_configuration_mode ~session_id ~rpc:!rpc ~self in
  let iPv6 = List.hd iPv6_lst in
  C.PIF.reconfigure_ipv6 ~session_id ~rpc:!rpc ~self ~iPv6 ~dNS ~gateway ~mode

(** --- Test skeleton, receives environment params before running  --- *)
let test_reconfigure_ip ~ipv6 ~session_id ~(self : API.ref_PIF) =
  let ip_string = if ipv6 then "IPv6" else "IPv4" in
  Printf.printf "Testing reconfiguring %s with clustering.\n" ip_string;
  try
    let dNS = C.PIF.get_DNS ~session_id ~rpc:!rpc ~self in
    if ipv6
    then reconfigure_ipv6 ~session_id ~self ~dNS
    else reconfigure_ipv4 ~session_id ~self ~dNS;

    Alcotest.fail "PIF.reconfigure_ip should raise clustering_enabled_on_network."
  with
  | Api_errors.(Server_error(code,_)) when code=Api_errors.clustering_enabled_on_network
      -> print_endline (Printf.sprintf "%s raised as expected." Api_errors.clustering_enabled_on_network)
  | Api_errors.(Server_error(_,_)) -> () (* Don't fail on other API errors, only test clustering *)

(** --- Check environment before calling test --- *)
let test session_id () =
  print_endline "Testing IP reconfiguration with and without clustering.";
  print_newline ();
  print_newline ();
  let pifs = Client.PIF.get_all ~session_id ~rpc:!rpc in

  List.iter
    (fun self ->
       let clustering =
         let network = C.PIF.get_network ~session_id ~rpc:!rpc ~self in
         C.Cluster.get_all ~session_id ~rpc:!rpc
         |> List.filter
           (fun cluster -> (C.Cluster.get_network ~session_id ~rpc:!rpc ~self:cluster) = network)
         |> (fun lst -> not (is_empty lst))
       in
       if clustering
       then begin
         test_reconfigure_ip ~ipv6:false ~session_id ~self
         (* IPv6 clusters not yet supported, can run this test once that changes *)
         (* test_reconfigure_ip ~ipv6:true ~session_id ~self *)
       end
       else
         print_endline "No cluster objects on this PIF, skipping tests."
    ) pifs

let tests session_id =
  [ "IP reconfiguration test", `Slow, test session_id
  ]
