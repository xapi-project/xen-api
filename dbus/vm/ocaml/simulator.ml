open Lwt
open Lwt_io

let volumes_per_vm = 6
let networks_per_vm = 6

let debug_logging = ref false

let rec serialise f = function
  | 0 -> return ()
  | n ->
    lwt () = f n in
    serialise f (n-1)

let parallelise f n =
  let rec loop acc = function
  | 0 -> Lwt.join acc
  | n ->
    let t = f n in
    loop (t :: acc) (n-1) in
  loop [] n

let log fmt =
  Printf.kprintf
    (fun s ->
      if !debug_logging
      then Printf.fprintf Pervasives.stderr "%s\n%!" s
    ) fmt

let vm_bus_name = "org.xenserver.vm1"
let volume_bus_name = "org.xenserver.volume.example"
let network_bus_name = "org.xenserver.network1"
let controller_bus_name = "org.xenserver.controller1"

let vm_path = [ "org"; "xenserver"; "vm1" ]
let volume_path = [ "org"; "xenserver"; "volume"; "example" ]
let network_path = [ "org"; "xenserver"; "network1" ]
let controller_path = [ "org"; "xenserver"; "controller1" ]
let owner_path = [ "org"; "xenserver"; "owner1" ]

let slash = Re_str.regexp_string "/"

let parse_uri uri =
  let x = Uri.of_string uri in
  match Uri.scheme x, Uri.path x with
  | Some bus_name, path ->
    let elements = List.tl (Re_str.split_delim slash path) in
    bus_name, elements
  | _, _ -> failwith (Printf.sprintf "Failed to parse object URI: %s" uri)

let owner_uri () =
  lwt bus = OBus_bus.session () in
  match OBus_connection.name bus with
  | "" -> failwith "Failed to query our unique connection name"
  | name ->
    (* remove the initial : *)
    let scheme =
      if name.[0] = ':'
      then String.sub name 1 (String.length name - 1)
      else name in
    let path = String.concat "/" ([""] @ owner_path) in
    let uri = Uri.make ~scheme ~path () in
    return (Uri.to_string uri)

let owner_ping uris =
  log "ping %d uris" (List.length uris);
  return (List.map (fun x -> true) uris)

let controller_start_multiple how_many =
  log "controller_start_multiple %ld" how_many;
  lwt bus = OBus_bus.session () in
  let vm = OBus_proxy.make (OBus_peer.make bus vm_bus_name) vm_path in
  let start = Unix.gettimeofday () in
  lwt () = parallelise
    (fun i ->
      OBus_method.call Vm.Org_xenserver_Vm1.m_start vm (string_of_int i)
    ) (Int32.to_int how_many) in
  let time = Unix.gettimeofday () -. start in
  return (string_of_float time)

let controller_stop_multiple which =
  return "unknown"

let wait_for_task bus task =
  let bus_name, path = parse_uri task in
  log "task is at %s (bus name = %s; path = [ %s ])" task bus_name (String.concat "; " path);
  let task_proxy = OBus_proxy.make (OBus_peer.make bus bus_name) path in
  let completed = Task.Org_xenserver_Task1.completed task_proxy in
  lwt event = OBus_signal.connect completed in
  let wait_for_signal = Lwt_react.E.next event in
  lwt result =
    try_lwt
      Task.Org_xenserver_Task1.get_result task_proxy
    with _ ->
      lwt () = wait_for_signal in
      Task.Org_xenserver_Task1.get_result task_proxy in
  lwt () = Task.Org_xenserver_Task1.destroy task_proxy in
  return result

let vm_start config =
  log "vm_start %s" config;
  (* Create a proxy for the remote object *)
  lwt bus = OBus_bus.session () in
  let volume = OBus_proxy.make (OBus_peer.make bus volume_bus_name) volume_path in
  let network = OBus_proxy.make (OBus_peer.make bus network_bus_name) network_path in
  lwt owner = owner_uri () in
  lwt () = parallelise 
    (fun _ ->
      lwt task = OBus_method.call Resource.Org_xenserver_Resource1.m_attach volume ("iscsi://target/lun", owner, "token") in
      lwt result = wait_for_task bus task in
      log "result = %s" (String.escaped result);
      return ()
    ) volumes_per_vm in
  lwt () = parallelise
    (fun _ ->
      lwt task = OBus_method.call Resource.Org_xenserver_Resource1.m_attach network ("sdn://magic/", owner, "token") in
      lwt result = wait_for_task bus task in
      log "result = %s" (String.escaped result);
      return ()
    ) networks_per_vm in
  return ()

let vm_stop id =
  log "vm_stop %s" id;
  return ()


let vm_interface =
 Vm.Org_xenserver_Vm1.(make {
   m_start = (fun obj config -> vm_start config);
   m_stop  = (fun obj id     -> vm_stop  id);
 })

let controller_interface =
 Controller.Org_xenserver_Controller1.(make {
   m_start_multiple = (fun obj which -> controller_start_multiple which);
   m_stop_multiple  = (fun obj which -> controller_stop_multiple which);
 })

let taskOwner_interface =
 TaskOwner.Org_xenserver_TaskOwner1.(make {
   m_ping = (fun obj uris -> owner_ping uris);
   p_tasks = (fun obj -> Lwt_react.S.return []);
 })

 lwt () =
  let implement_vm = ref false in
  let implement_control = ref false in
  let implement_all = ref false in
  Arg.parse [
    "-debug", Arg.Set debug_logging, "Print debug logging";
    "-vm",    Arg.Set implement_vm,  "Implement VM";
    "-control", Arg.Set implement_control, "Implement Control";
    "-all",     Arg.Set implement_all,     "Implement everything";
  ] (fun x -> Printf.fprintf Pervasives.stderr "Ignoring argument: %s\n" x)
  "A simple system mockup";

  lwt bus = OBus_bus.session () in

  lwt () = if !implement_vm || !implement_all then begin
    lwt _ = OBus_bus.request_name bus vm_bus_name in
    let obj = OBus_object.make ~interfaces:[vm_interface] vm_path in
    OBus_object.attach obj ();
    OBus_object.export bus obj;
    return ()
  end else return () in

  lwt () = if !implement_control || !implement_all then begin
    lwt _ = OBus_bus.request_name bus controller_bus_name in
    let obj = OBus_object.make ~interfaces:[controller_interface] controller_path in
    OBus_object.attach obj ();
    OBus_object.export bus obj;
    return ()
  end else return () in

  let obj = OBus_object.make ~interfaces:[taskOwner_interface] owner_path in
  OBus_object.attach obj ();
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
