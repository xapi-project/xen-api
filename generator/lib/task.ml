(* Tasks API *)
open Rpc
open Idl
open Common

let doc l = String.concat " " l

type error_t = string [@@deriving rpcty]
type id = string [@@deriving rpcty] [@@doc ["Unique identifier for a task"]]
type task_list = id list [@@deriving rpcty]

type async_result_t =
  | UnitResult of unit
  | Volume of Control.volume
  [@@deriving rpcty]

type completion_t = {
  duration : float;
  result : async_result_t option
} [@@deriving rpcty]

type state =
  | Pending of float [@doc [
      "the task is in progress, with progress info from 0..1"]]
  | Completed of completion_t
  | Failed of error_t
  [@@deriving rpcty]

type task = {
  id : id;
  debug_info : string;
  ctime : float;
  state : state;
} [@@deriving rpcty]

let unit = Param.mk Types.unit
let dbg = Param.mk ~name:"dbg" ~description:["Debug context from the caller"]
    Types.string

module Task(R : RPC) = struct
  open R

  let task_p = Param.mk ~name:"id" id
  let result = Param.mk ~name:"result" task
  let stat = declare "stat"
      ["[stat task_id] returns the status of the task"]
      (dbg @-> task_p @-> returning result error)

  let cancel = declare "cancel"
      ["[cancel task_id] performs a best-effort cancellation of an ongoing ";
       "task. The effect of this should leave the system in one of two ";
       "states: Either that the task has completed successfully, or that it ";
       "had never been made at all. The call should return immediately and ";
       "the status of the task can the be queried via the [stat] call."]
      (dbg @-> task_p @-> returning unit error)

  let destroy = declare "destroy"
      ["[destroy task_id] should remove all traces of the task_id. This call ";
       "should fail if the task is currently in progress."]
      (dbg @-> task_p @-> returning unit error)

  let task_list = Param.mk task_list
  let ls = declare "ls"
      ["[ls] should return a list of all of the tasks the plugin is aware of."]
      (dbg @-> unit @-> returning task_list error)

  let implementation = implement Idl.Interface.({
      name = "Task";
      namespace = Some "Task";
      description =
        ["The task interface is for querying the status of asynchronous ";
         "tasks. All long-running operations are associated with tasks, ";
         "including copying and mirroring of data."];
      version=(1,0,0) })
end

module T = Task(Codegen.Gen ())

let interfaces = Codegen.Interfaces.create
    ~name:"task"
    ~title:"The Task interface"
    ~description:[
      "The Task interface is required if the backend supports long-running ";
      "tasks."]
    ~interfaces:[T.implementation ()]
