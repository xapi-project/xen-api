open Lwt
open Lwt_io

let vm_start config =
  lwt () = printlf "vm_start %s" config in
  return ()

let vm_stop id =
  lwt () = printlf "vm_stop %s" id in
  return ()


let vm_interface =
 Vm.Org_xenserver_api_vm.(make {
   m_start = (fun obj config -> vm_start config);
   m_stop  = (fun obj id     -> vm_stop  id);
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

  (* Wait forever *)
  fst (wait ())
