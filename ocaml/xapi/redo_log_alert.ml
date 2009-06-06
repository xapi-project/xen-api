open Threadext

module R = Debug.Debugger(struct let name = "redo_log" end)
open R

(* keeps track of the previous value of Redo_log.currently_accessible so we can detect state changes *)
let previously_accessible = ref true

let raise_system_alert news =
  (* This code may block indefinitely while attempting to look up the pool UUID and send the alert, so do it in a separate thread *)
  ignore (Thread.create (fun () ->
    debug "Raising system alert...";
    let __context = Context.make "context" in
    let pool = Helpers.get_pool ~__context in
    let obj_uuid = Db.Pool.get_uuid ~__context ~self:pool in
    debug "Pool UUID is %s" obj_uuid;
    (try ignore (Xapi_message.create ~__context ~name:news ~priority:1L ~cls:`Pool ~obj_uuid ~body:"") with _ -> ());
    debug "System alert raised"
  ) ())

let loop () =
  name_thread "HA metadata VDI monitor";
  Mutex.execute Redo_log.currently_accessible_m (fun () ->
    while true do
      (* Wait until we are signalled that a state change has occurred *)
      Condition.wait Redo_log.currently_accessible_condition Redo_log.currently_accessible_m;

      (* The variable Redo_log.currently_accessible has been updated -- send the alert if there was a change *)
      begin
        match !previously_accessible, !Redo_log.currently_accessible with
        | false, false -> ()
        | true, true -> ()
        | false, true -> debug "Raising system alert that all is now well"; raise_system_alert Api_messages.redo_log_healthy
        | true, false -> debug "Raising system alert to say that we can't access the redo log"; raise_system_alert Api_messages.redo_log_broken
      end;
      previously_accessible := !Redo_log.currently_accessible
    done
  )
