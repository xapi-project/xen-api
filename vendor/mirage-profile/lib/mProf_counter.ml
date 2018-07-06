(* Copyright (C) 2014, Thomas Leonard *)

type t = {
  name : string;
  mutable value : int;
}

let create ?(init=0) ~name () = { name; value = init }
let make ~name = create ~name ()

let set_value m v =
  m.value <- v;
  MProf_trace.note_counter_value m.name v

let increase m amount =
  set_value m (m.value + amount)

let value m = m.value
