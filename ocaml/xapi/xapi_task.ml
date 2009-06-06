module D = Debug.Debugger(struct let name="xapi" end)
open D

let get_allowed_messages ~__context ~self = []

let create ~__context ~label ~description =
	(* This call will have a dummy task ID already but we need to make a fresh one *)
  let subtask_of = Context.get_task_id __context in
	let c = Context.make ~task_description:description ~subtask_of ~task_in_database:true label in
	let t = Context.get_task_id c in
  (*info "Task.create ref = %s; label = %s" (Ref.string_of t) label;*)
	t

let destroy ~__context ~self =
  Db.Task.destroy ~__context ~self

let cancel ~__context ~task =
        Db.Task.set_current_operations ~__context ~self:task ~value:[(Ref.string_of (Context.get_task_id __context)), `cancel]
