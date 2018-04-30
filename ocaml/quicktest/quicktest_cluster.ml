
module Q = Quicktest_common
module C = Client.Client

let is_empty = function | [] -> true | _ -> false
let rpc = !Q.rpc

(* [Q.failed test.Q.name string_of_failure] removes [test] from a test Hashtbl
 * and is therefore only called once, in the try-with statement.
 * This exception is raised within the try-with body to trigger
 * [Q.failed test string_of_failure] *)
exception Abort_test of string

(** --- Helpers for IP reconfiguration tests --- *)

let reconfigure_ipv4 ~session_id ~self ~dNS =
  let netmask = C.PIF.get_netmask ~session_id ~rpc ~self in
  let iP = C.PIF.get_IP ~session_id ~rpc ~self in
  let gateway = C.PIF.get_gateway ~session_id ~rpc ~self in
  let mode = C.PIF.get_ip_configuration_mode ~session_id ~rpc ~self in
  C.PIF.reconfigure_ip ~session_id ~rpc ~self ~iP ~dNS ~gateway ~netmask ~mode

let reconfigure_ipv6 ~session_id ~self ~dNS =

  (* confirm valid IPv6 strings exist *)
  let iPv6_lst = (C.PIF.get_IPv6 ~session_id ~rpc ~self) |> List.filter ((<>) "") in
  if is_empty iPv6_lst
  then raise (Abort_test "No valid IPv6 strings exist.");

  let gateway = C.PIF.get_ipv6_gateway ~session_id ~rpc ~self in
  let mode = C.PIF.get_ipv6_configuration_mode ~session_id ~rpc ~self in
  let iPv6 = List.hd iPv6_lst in
  C.PIF.reconfigure_ipv6 ~session_id ~rpc ~self ~iPv6 ~dNS ~gateway ~mode

(** --- Test skeleton, receives environment params before running  --- *)
let test_reconfigure_ip ~ipv6 ~session_id ~(self : API.ref_PIF) =
  let ip_string = if ipv6 then "IPv6" else "IPv4" in
  let test =
    Q.make_test (Printf.sprintf "Testing reconfiguring %s with clustering." ip_string) 4
  in
  try
    Q.start test;

    let dNS = C.PIF.get_DNS ~session_id ~rpc ~self in
    if ipv6
    then reconfigure_ipv6 ~session_id ~self ~dNS
    else reconfigure_ipv4 ~session_id ~self ~dNS;

    Q.failed test "PIF.reconfigure_ip should raise CLUSTERING_ENABLED"
  with
  | Api_errors.(Server_error(code,_)) when code=Api_errors.clustering_enabled
      -> Q.debug test (Printf.sprintf "%s raised as expected." Api_errors.clustering_enabled);
         Q.success test
  | Api_errors.(Server_error(_,_)) -> () (* Don't fail on other API errors, only test clustering *)
  | Abort_test s -> Q.failed test s
  | e -> Q.failed test (ExnHelper.string_of_exn e)

(** --- Check environment before calling test --- *)
let test session_id =
  let test_all_pifs = Q.make_test "Testing IP reconfiguration with and without clustering." 2 in
  try
    print_newline ();
    Q.start test_all_pifs;
    print_newline ();

    let enabled_cluster_hosts =
      List.filter
        (fun self -> C.Cluster_host.get_enabled ~session_id ~rpc ~self)
        (C.Cluster_host.get_all ~session_id ~rpc)
    in
    if is_empty enabled_cluster_hosts
    then Q.debug test_all_pifs "No PIFS with clustering enabled, skipping tests."
    else begin
      enabled_cluster_hosts
      |> List.map
        (fun self -> C.Cluster_host.get_PIF ~session_id ~rpc ~self)
      |> List.iter
        (fun self ->
          test_reconfigure_ip ~ipv6:false ~session_id ~self
            (* IPv6 clusters not yet supported, can run this line once they are:
               test_reconfigure_ip ~ipv6:true ~session_id ~self *)
        );
      Q.success test_all_pifs
    end
  with e -> Q.failed test_all_pifs (ExnHelper.string_of_exn e)
