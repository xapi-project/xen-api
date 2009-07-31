module D = Debug.Debugger(struct let name = "xapi" end)
open D

(* High-level functions called when rolling upgrade 'starts' and 'stops' where
   start and stop are defined by the logic in db_gc.ml. *)

open Threadext

let start () = 
  ()

let stop () = 
  ()
