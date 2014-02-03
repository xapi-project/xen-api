open Lwt
open Lwt_io

let vm_start config =
  lwt () = printlf "vm_start %s" config in
  (* Create a proxy for the remote object *)
  lwt bus = OBus_bus.session () in
  let proxy = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["volume"] in
  lwt (local_uri, id) = OBus_method.call Resource.Org_xenserver_api_resource.m_attach proxy "iscsi://target/lun" in
  lwt () = printlf "  got local_uri %s id %s" local_uri id in
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


lwt () =
  lwt bus = OBus_bus.session () in

  (* Request a name *)
  lwt _ = OBus_bus.request_name bus "org.xenserver.vm" in

  (* Create the object *)
  let obj = OBus_object.make ~interfaces:[vm_interface] ["vm"] in
  OBus_object.attach obj ();

  (* Export the object on the connection *)
  OBus_object.export bus obj;

  lwt _ = OBus_bus.request_name bus "org.xenserver.volume" in
  let obj = OBus_object.make ~interfaces:[volume_interface] ["volume"] in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
