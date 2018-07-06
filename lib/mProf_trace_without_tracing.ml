(* Copyright (C) 2014, Thomas Leonard *)

type hiatus_reason =
  | Wait_for_work
  | Suspend
  | Hibernate

let note_hiatus _reason = ()
let note_resume () = ()

let label _label = ()
let named_wait _label = Lwt.wait ()
let named_task _label = Lwt.task ()
let named_condition _label = Lwt_condition.create ()
let named_mvar _label v = Lwt_mvar.create v
let named_mvar_empty _label = Lwt_mvar.create_empty ()
let should_resolve _thread = ()

let note_increase _counter _amount = ()
let note_counter_value _counter _value = ()
