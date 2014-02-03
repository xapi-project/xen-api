open Lwt
open Lwt_io

let volumes_per_vm = 6
let networks_per_vm = 6
let total_vms = 5

let debug_logging = ref false

let rec repeat f = function
  | 0 -> return ()
  | n ->
    lwt () = f n in
    repeat f (n-1)

let log fmt =
  Printf.kprintf
    (fun s ->
      if !debug_logging
      then Printf.fprintf Pervasives.stderr "%s\n%!" s
    ) fmt

let controller_start_multiple which =
  log "controller_start_multiple %s" which;
  lwt bus = OBus_bus.session () in
  let vm = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["vm"] in
  let start = Unix.gettimeofday () in
  lwt () = repeat
    (fun i ->
      OBus_method.call Vm.Org_xenserver_api_vm.m_start vm (string_of_int i)
    ) total_vms in
  let time = Unix.gettimeofday () -. start in
  return (string_of_float time)

let controller_stop_multiple which =
  return "unknown"

let vm_start config =
  log "vm_start %s" config;
  (* Create a proxy for the remote object *)
  lwt bus = OBus_bus.session () in
  let volume = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["volume"] in
  let network = OBus_proxy.make (OBus_peer.make bus "org.xenserver.vm") ["network"] in
  lwt () = repeat
    (fun _ ->
      lwt (local_uri, id) = OBus_method.call Resource.Org_xenserver_api_resource.m_attach volume "iscsi://target/lun" in
      log "  got local_uri %s id %s" local_uri id;
      return ()
    ) volumes_per_vm in
  lwt () = repeat
    (fun _ ->
      lwt (local_uri, id) = OBus_method.call Resource.Org_xenserver_api_resource.m_attach network "sdn://magic/" in
      log "  got local_uri %s id %s" local_uri id;
      return ()
    ) networks_per_vm in
  return ()

let vm_stop id =
  log "vm_stop %s" id;
  return ()


let vm_interface =
 Vm.Org_xenserver_api_vm.(make {
   m_start = (fun obj config -> vm_start config);
   m_stop  = (fun obj id     -> vm_stop  id);
 })

let volume_attach global_uri =
  log "volume_attach %s" global_uri;
  return ("file://block/device", "some id")

let volume_detach id =
  log "volume_detach %s" id;
  return ()

let volume_interface =
 Resource.Org_xenserver_api_resource.(make {
   m_attach = (fun obj global_uri -> volume_attach global_uri);
   m_detach = (fun obj id         -> volume_detach id);
 })

let network_attach global_uri =
  log "network_attach %s" global_uri;
  return ("vlan://eth0/100", "some id")

let network_detach id =
  log "network_detach %s" id;
  return ()

let network_interface =
 Resource.Org_xenserver_api_resource.(make {
   m_attach = (fun obj global_uri -> network_attach global_uri);
   m_detach = (fun obj id         -> network_detach id);
 })

let controller_interface =
 Controller.Org_xenserver_api_controller.(make {
   m_start_multiple = (fun obj which -> controller_start_multiple which);
   m_stop_multiple  = (fun obj which -> controller_stop_multiple which);
 })

lwt () =
  Arg.parse [
    "-debug", Arg.Set debug_logging, "Print debug logging"
  ] (fun x -> Printf.fprintf Pervasives.stderr "Ignoring argument: %s\n" x)
  "A simple system mockup";

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

  lwt _ = OBus_bus.request_name bus "org.xenserver.controller" in
  let obj = OBus_object.make ~interfaces:[controller_interface] ["controller"] in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
