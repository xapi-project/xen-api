open Pervasiveext

  type event_record = 
     | Session of          [`Session ] Ref.t * API.session_t 
     | Task of             [`task ] Ref.t * API.task_t
     | Event of            [`Event] Ref.t * API.event_t
     | VM of               [`VM] Ref.t * API.vM_t
     | VM_metrics of       [`VM_metrics] Ref.t * API.vM_metrics_t
     | VM_guest_metrics of [`VM_guest_metrics] Ref.t * API.vM_guest_metrics_t
     | Host of             [`host] Ref.t * API.host_t
     | Host_metrics of     [`host_metrics] Ref.t * API.host_metrics_t
     | Host_cpu of         [`host_cpu] Ref.t * API.host_cpu_t
     | Network of          [`network] Ref.t * API.network_t
     | VIF of              [`VIF] Ref.t * API.vIF_t
     | VIF_metrics of      [`VIF_metrics] Ref.t * API.vIF_metrics_t
     | PIF of              [`PIF] Ref.t * API.pIF_t
     | PIF_metrics of      [`PIF_metrics] Ref.t * API.pIF_metrics_t
     | SR of               [`SR] Ref.t * API.sR_t
     | VDI of              [`VDI] Ref.t * API.vDI_t
     | VBD of              [`VBD] Ref.t * API.vBD_t
     | VBD_metrics of      [`VBD_metrics] Ref.t * API.vBD_metrics_t
     | PBD of              [`PBD] Ref.t * API.pBD_t
     | Crashdump of        [`Crashdump] Ref.t * API.crashdump_t
     | VTPM of             [`VTPM] Ref.t *  API.vTPM_t
     | Console of          [`Console] Ref.t * API.console_t
     | User of             [`User] Ref.t * API.user_t
     | Pool of             [`pool] Ref.t *  API.pool_t
     | Message of          [`message] Ref.t * API.message_t
     | Secret of           [`secret] Ref.t * API.secret_t  

  let record_of_event ev =
   let xmlrpc = match ev.Event_types.snapshot with Some x -> x | None -> failwith "no record in event" in
    match ev.Event_types.ty with
     | "session" ->          Session (Ref.of_string ev.Event_types.reference, API.From.session_t "" xmlrpc)
     | "task" ->             Task (Ref.of_string ev.Event_types.reference, API.From.task_t "" xmlrpc)
     | "event" ->            Event (Ref.of_string ev.Event_types.reference,API.From.event_t "" xmlrpc)
     | "vm" ->               VM (Ref.of_string ev.Event_types.reference,API.From.vM_t "" xmlrpc)
     | "vm_metrics" ->       VM_metrics (Ref.of_string ev.Event_types.reference,API.From.vM_metrics_t "" xmlrpc)
     | "vm_guest_metrics" -> VM_guest_metrics (Ref.of_string ev.Event_types.reference,API.From.vM_guest_metrics_t "" xmlrpc)
     | "host" ->             Host (Ref.of_string ev.Event_types.reference,API.From.host_t "" xmlrpc)
     | "host_metrics" ->     Host_metrics (Ref.of_string ev.Event_types.reference,API.From.host_metrics_t "" xmlrpc)
     | "host_cpu" ->         Host_cpu (Ref.of_string ev.Event_types.reference,API.From.host_cpu_t "" xmlrpc)
     | "network" ->          Network (Ref.of_string ev.Event_types.reference,API.From.network_t "" xmlrpc)
     | "vif" ->              VIF (Ref.of_string ev.Event_types.reference,API.From.vIF_t "" xmlrpc)
     | "vif_metrics" ->      VIF_metrics (Ref.of_string ev.Event_types.reference,API.From.vIF_metrics_t "" xmlrpc)
     | "pif" ->              PIF (Ref.of_string ev.Event_types.reference,API.From.pIF_t "" xmlrpc)
     | "pif_metrics" ->      PIF_metrics (Ref.of_string ev.Event_types.reference,API.From.pIF_metrics_t "" xmlrpc)
     | "sr" ->               SR (Ref.of_string ev.Event_types.reference,API.From.sR_t "" xmlrpc)
     | "vdi" ->              VDI (Ref.of_string ev.Event_types.reference,API.From.vDI_t "" xmlrpc)
     | "vbd" ->              VBD (Ref.of_string ev.Event_types.reference,API.From.vBD_t "" xmlrpc)
     | "vbd_metrics" ->      VBD_metrics (Ref.of_string ev.Event_types.reference,API.From.vBD_metrics_t "" xmlrpc)
     | "pbd" ->              PBD (Ref.of_string ev.Event_types.reference,API.From.pBD_t "" xmlrpc)
     | "crashdump" ->        Crashdump (Ref.of_string ev.Event_types.reference,API.From.crashdump_t "" xmlrpc)
     | "vtpm" ->             VTPM (Ref.of_string ev.Event_types.reference,API.From.vTPM_t "" xmlrpc)
     | "console" ->          Console (Ref.of_string ev.Event_types.reference,API.From.console_t "" xmlrpc)
     | "user" ->             User (Ref.of_string ev.Event_types.reference,API.From.user_t "" xmlrpc)
     | "pool" ->             Pool (Ref.of_string ev.Event_types.reference,API.From.pool_t "" xmlrpc)
     | "message" ->          Message (Ref.of_string ev.Event_types.reference,API.From.message_t "" xmlrpc)
     | "secret" ->           Secret (Ref.of_string ev.Event_types.reference,API.From.secret_t "" xmlrpc)
  
  
