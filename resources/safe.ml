exception UseAfterMoveOrRelease

type 'a wrapped = {
  raw: 'a
; release: 'a -> unit
; on_finalise_leaked: ('a -> unit) option
}

(* a bool could've been used here to track whether we already called
 * [release] or not but that would've kept the underlying value alive *)
type 'a t = 'a wrapped option ref

let leaked wrapped =
  Option.iter (fun f -> f wrapped.raw) wrapped.on_finalise_leaked

let finalise t =
  ( try Option.iter leaked !t
    with _ -> (* this is a finaliser, do not let exceptions escape *)
      ()
  ) ;
  (* make sure finalizer doesn't get run again *)
  t := None

let create v =
  let r = ref (Some v) in
  if Option.is_some v.on_finalise_leaked then
    (* this is part of [create] so that move will get finalizer attached too *)
    Gc.finalise finalise r ;
  r

let borrow_exn t =
  match !t with None -> raise UseAfterMoveOrRelease | Some v -> v

let move_exn t =
  let t' = borrow_exn t in
  t := None ;
  create t'

let safe_release t =
  match !t with
  | None ->
    ()
  | Some wrapped ->
    (* make sure [release] cannot be called again, even if [relase] fails
       below *)
    t := None ;
    wrapped.release wrapped.raw

(* give access directly to the value wrapped above: *)
let borrow_exn t = (borrow_exn t).raw

let create ?on_finalise_leaked ~release raw =
  create {raw; release; on_finalise_leaked}

let within t f =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f t)
    (fun () -> safe_release t)
