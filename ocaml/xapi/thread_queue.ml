
(* A simple locked queue implementation where a background thread pulls jobs serially from the queue and executes them.
   Useful for offloading potentially blocking but not critical tasks to background threads (like HA alerts) *)

open Pervasiveext
open Threadext

module D = Debug.Debugger(struct let name="thread_queue" end)
open D

(** The type of the function which processes elements taken from the queue *)
type 'a process_fn = 'a -> unit

(** The type of the function which pushes new elements into the queue *)
type 'a push_fn = 'a -> bool

(** Given an optional maximum queue length and a function for processing elements (which will be called in a 
    single background thread), return a function which pushes items onto the queue. *)
let make ?max_q_length ?(name="unknown") (process_fn: 'a process_fn) : 'a push_fn = 
  let q = Queue.create () in
  let c = Condition.create () in
  let m = Mutex.create () in

  (** The background thread *)
  let t = ref None in
    
  let thread_body () = 
    Mutex.execute m
      (fun () ->
	 while true do
	   (* Wait until there is work to do *)
	   while Queue.length q = 0 do Condition.wait c m done;
	   (* Make a copy of the items in the q so we can drop the lock and process them *)
	   let local_q = Queue.copy q in
	   Queue.clear q;

	   Mutex.unlock m;
	   (* Process the items dropping any exceptions (process function should do whatever logging it wants) *)
	   finally (fun () -> Queue.iter (fun x -> try process_fn x with _ -> ()) local_q) (fun () -> Mutex.lock m);
	   debug "%s: completed processing %d items" name (Queue.length local_q);
	 done
      ) in
      
  (* Called with lock already held *)
  let maybe_start_thread () = 
    match !t with
    | Some _ -> ()
    | None -> t := Some (Thread.create thread_body ()) in
	
  let push x = 
    Mutex.execute m
      (fun () -> 
	 let q_length = Queue.length q in
	 match max_q_length with
	 | Some max when q_length > max ->
	     warn "%s: Maximum length exceeded (%d): dropping item" name max;
	     false
	 | _ ->
	     Queue.push x q;
	     debug "%s: adding item: new length is %d" name (Queue.length q);
	     Condition.signal c;
	     maybe_start_thread ();
	     true
      )

  in push
