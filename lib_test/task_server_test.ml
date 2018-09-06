open OUnit

module TestInterface = struct
  let service_name = "test_interface"

  exception Does_not_exist of (string * string)
  exception Cancelled of string
  let does_not_exist (a,b) = raise @@ Does_not_exist(a,b)
  let cancelled a = raise @@ Cancelled a

  module Task = struct
    type id = string
    type async_result = string [@@deriving rpc]
    type completion_t = {
      duration : float;
      result: async_result option;
    }
    type state =
      | Pending of float
      | Completed of completion_t
      | Failed of Rpc.t
    type t = {
      id: id;
      dbg: string;
      ctime: float;
      state: state;
      subtasks: (string * state) list;
      debug_info: (string * string) list;
      backtrace: string;
      cancellable: bool;
    }
  end

  module Exception = struct
    type exnty =
      | Internal_error of string
      | Does_not_exist of string * string
      | Cancelled of string
      | Unknown of string [@@deriving rpc]
  end

  exception Internal_error of string

  let exnty_of_exn = function
    | Internal_error s -> Exception.Internal_error s
    | Cancelled s -> Exception.Cancelled s
    | Does_not_exist (x,y) -> Exception.Does_not_exist (x,y)
    | e -> Exception.Unknown (Printexc.to_string e)
  let exn_of_exnty = function
    | Exception.Internal_error s -> Internal_error s
    | Exception.Does_not_exist (x,y) -> Does_not_exist (x,y)
    | Exception.Cancelled s -> Cancelled s
    | Exception.Unknown s -> Failure s

  let marshal_exn e = e |> exnty_of_exn |> Exception.rpc_of_exnty
end

module T = Task_server.Task(TestInterface)

(* Test that we can add a task and retrieve it from the task list *)
let test_add () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun _task -> Some "done") in
  let ts = T.list t in
  assert_bool "Task in task list" (List.mem task ts)

(* Test that destroying a task removes it from the task list *)
let test_destroy () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun _task -> Some "done") in
  T.destroy task;
  let ts = T.list t in
  assert_bool "Task not in task list" (not (List.mem task ts))

(* Test 'running' a task, and that the various times associated with the
   task make sense, and that the task status is correctly completed with
   correct result *)
let test_run () =
  let t = T.empty () in
  let start = Unix.gettimeofday () in
  Thread.delay 0.001;
  let task = T.add t "dbg" (fun _task -> Thread.delay 0.001; Some "done") in
  T.run task;
  let t' = T.to_interface_task task in
  assert_bool "Task ctime" (t'.TestInterface.Task.ctime > start);
  assert_bool "Task result"
    (match t'.TestInterface.Task.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration} ->
       r = "done" && duration > 0.0
     | _ -> false)

(* Test what happens when the function passed to the task server raises an
   exception. The task result should be failed with the correct exception *)
let test_raise () =
  Debug.disable "task_server";
  let t = T.empty () in
  let task = T.add t "dbg" (fun _task -> raise (TestInterface.Internal_error "test")) in
  T.run task;
  let t' = T.to_interface_task task in
  assert_bool "Task result"
    (match t'.TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let s = TestInterface.Exception.exnty_of_rpc r in
           s = TestInterface.Exception.Internal_error "test"
         with _ -> false
       end
     | _ -> false)

(* Test cancellation of a task, in this case cancelled before the task is
   run. The state should be 'failed' with exception 'cancelled' *)
let test_cancel () =
  let t = T.empty () in
  let task = T.add t "dbg" (fun task -> T.check_cancelling task; Some "foo") in
  let id = T.id_of_handle task in
  T.cancel task;
  T.run task;
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id
         with _ -> false end
     | _ -> false)

(* Tests the 'with_cancel' function. Tests that the cancel function gets
   run on cancellation. In this case, the cancellation happens before the
   task is run *)
let test_with_cancel () =
  let t = T.empty () in
  let cancel_fn_run = ref false in
  let task = T.add t "dbg"
      (fun task -> T.with_cancel task (fun () -> cancel_fn_run := true) (fun () -> Some "foo")) in
  let id = T.id_of_handle task in
  T.cancel task;
  T.run task;
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id
         with _ -> false end
     | _ -> false);
  assert_bool "Cancel_fn run" !cancel_fn_run

(* Tests what happens when the 'cancel' function passed to 'with_cancel' itself
   fails. *)
let test_with_cancel_failure () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task -> T.with_cancel task (fun () -> failwith "moo") (fun () -> Some "foo")) in
  let id = T.id_of_handle task in
  T.cancel task;
  T.run task;
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id
         with _ -> false end
     | _ -> false)

(* Similar to test_with_cancel, but in this case the cancellation function
   is called after the task is started *)
let test_with_cancel2 () =
  let t = T.empty () in
  let delay = Scheduler.Delay.make () in
  let cancel_fn_run = ref false in
  let task = T.add t "dbg"
      (fun task ->
         T.with_cancel task (fun () -> cancel_fn_run := true)
           (fun () -> ignore (Scheduler.Delay.wait delay 1.0);
             T.check_cancelling task; Some "foo")) in
  let id = T.id_of_handle task in
  let th = Thread.create (fun () -> T.run task) () in
  Thread.delay 0.01;
  T.cancel task;
  Scheduler.Delay.signal delay;
  Thread.join th;
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id
         with _ -> false end
     | _ -> false);
  assert_bool "Cancel_fn run" !cancel_fn_run

(* Similar to test_with_cancel_failure, but as above the cancel_fn is
   called after the task has started *)
let test_with_cancel_failure2 () =
  let t = T.empty () in
  let delay = Scheduler.Delay.make () in
  let task = T.add t "dbg"
      (fun task ->
         T.with_cancel task (fun () -> failwith "moo")
           (fun () -> ignore (Scheduler.Delay.wait delay 1.0);
             T.check_cancelling task; Some "foo")) in
  let id = T.id_of_handle task in
  let th = Thread.create (fun () -> T.run task) () in
  Thread.delay 0.01;
  T.cancel task;
  Scheduler.Delay.signal delay;
  Thread.join th;
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id
         with _ -> false end
     | _ -> false)

(* Check the 'subtask' functionality. Subtasks are logged in the task
   record. *)
let test_subtasks () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task ->
        let _ : int = T.with_subtask task "subtask1" (fun () -> 0) in
         Some "done") in
  let _id = T.id_of_handle task in
  T.run task;
  assert_bool "Subtasks"
    ((List.hd (T.to_interface_task task).TestInterface.Task.subtasks |> fst) = "subtask1");
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration=_} ->
       r = "done"
     | _ -> false)

(* Check what happens when subtasks fail. The whole task should be marked
   as failed, and the individual task that caused the problem should be
   marked as failed in the task record, with the correct exception. *)
let test_subtasks_failure () =
  let t = T.empty () in
  let task = T.add t "dbg"
      (fun task ->
         let _ : int = T.with_subtask task "subtask1"
             (fun () -> raise (TestInterface.Internal_error "foo")) in
         Some "done") in
  T.run task;
  let subtask = List.hd (T.to_interface_task task).TestInterface.Task.subtasks in
  assert_bool "Subtasks"
    (fst subtask = "subtask1");
  assert_bool "Subtasks"
    (match snd subtask with
     | TestInterface.Task.Failed r ->
       r |> TestInterface.Exception.exnty_of_rpc = TestInterface.Exception.Internal_error "foo"
     | _ -> false);
  assert_bool "Task result"
    (match (T.to_interface_task task).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       r |> TestInterface.Exception.exnty_of_rpc = TestInterface.Exception.Internal_error "foo"
     | _ -> false)

(* Test the cancellation points functionality. In here, we ask for task 'dbg'
   to be cancelled at the 5th time it checks for cancellation. Verify this
   succeeds for the task specified, and doesn't for the other task *)
let test_cancel_trigger () =
  let t = T.empty () in
  T.set_cancel_trigger t "dbg" 5;
  let xxx = ref 0 in
  let dbg = ref 0 in
  let fn x task =
    let rec loop n =
      x := n;
      if n=0 then () else (T.check_cancelling task; loop (n-1))
    in loop 10;
    Some "done"
  in
  let task1 = T.add t "xxx" (fn xxx) in
  let task2 = T.add t "dbg" (fn dbg) in
  let id2 = T.id_of_handle task2 in
  T.run task1;
  T.run task2;
  assert_bool "Task result"
    (match (T.to_interface_task task2).TestInterface.Task.state with
     | TestInterface.Task.Failed r ->
       begin
         try
           let e = TestInterface.Exception.exnty_of_rpc r in
           e = TestInterface.Exception.Cancelled id2
         with _ -> false end
     | _ -> false);
  assert_bool "Task result"
    (match (T.to_interface_task task1).TestInterface.Task.state with
     | TestInterface.Task.Completed {TestInterface.Task.result=Some r; duration=_} ->
       r = "done"
      | _ -> false);
  assert_bool "cancel points xxx" (!xxx = 0);
  assert_bool "cancel points dbg" (!dbg = 6)

let tests =
  "Task_server tests" >:::
  [
    "Test adding a task" >:: test_add;
    "Test removing a task" >:: test_destroy;
    "Test run" >:: test_run;
    "Test raise" >:: test_raise;
    "Test cancel" >:: test_cancel;
    "Test with_cancel" >:: test_with_cancel;
    "Test with_cancel_failure" >:: test_with_cancel_failure;
    "Test with_cancel 2" >:: test_with_cancel2;
    "Test with_cancel_failure2" >:: test_with_cancel_failure2;
    "Test subtasks" >:: test_subtasks;
    "Test subtask failure" >:: test_subtasks_failure;
    "Test cancel trigger" >:: test_cancel_trigger;
  ]
