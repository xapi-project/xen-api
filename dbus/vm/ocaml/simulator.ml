open Lwt
open Lwt_io

let volumes_per_vm = 6
let networks_per_vm = 6
let total_vms = 1000

let rec repeat f = function
  | 0 -> return ()
  | n ->
    lwt () = f () in
    repeat f (n-1)

let vm_start config =
  lwt () = printlf "vm_start %s" config in
  (* Create a proxy for the remote object *)
  lwt bus = OBus_bus.session () in
  let volume = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["volume"] in
  let network = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["network"] in
  lwt () = repeat
    (fun () ->
      lwt (local_uri, id) = OBus_method.call Resource.Org_xenserver_api_resource.m_attach volume "iscsi://target/lun" in
      printlf "  got local_uri %s id %s" local_uri id) volumes_per_vm in
  lwt () = repeat
    (fun () ->
      lwt (local_uri, id) = OBus_method.call Resource.Org_xenserver_api_resource.m_attach network "sdn://magic/" in
      printlf "  got local_uri %s id %s" local_uri id) networks_per_vm in
  return ()

let vm_stop id =
  lwt () = printlf "vm_stop %s" id in
  return ()


let vm_interface =
 Vm.Org_xenserver_api_vm.(make {
   m_start = (fun obj config -> vm_start config);
   m_stop  = (fun obj id     -> vm_stop  id);
 })

let volume_attach global_uri =
  lwt () = printlf "volume_attach %s" global_uri in
  return ("file://block/device", "some id")

let volume_detach id = printlf "volume_detach %s" id 

let volume_interface =
 Resource.Org_xenserver_api_resource.(make {
   m_attach = (fun obj global_uri -> volume_attach global_uri);
   m_detach = (fun obj id         -> volume_detach id);
 })

let network_attach global_uri =
  lwt () = printlf "network_attach %s" global_uri in
  return ("vlan://eth0/100", "some id")

let network_detach id = printlf "network_detach %s" id 

let network_interface =
 Resource.Org_xenserver_api_resource.(make {
   m_attach = (fun obj global_uri -> network_attach global_uri);
   m_detach = (fun obj id         -> network_detach id);
 })

lwt () =
  lwt bus = OBus_bus.session () in

  lwt _ = OBus_bus.request_name bus "org.xenserver.vm" in
  let obj = OBus_object.make ~interfaces:[vm_interface] ["vm"] in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  lwt _ = OBus_bus.request_name bus "org.xenserver.volume" in
  let obj = OBus_object.make ~interfaces:[volume_interface] ["volume"] in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  lwt _ = OBus_bus.request_name bus "org.xenserver.network" in
  let obj = OBus_object.make ~interfaces:[network_interface] ["network"] in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
