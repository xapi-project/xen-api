open! Core_kernel
open! Import

module Scheduler = Scheduler1

type any = [ `Empty | `Empty_one_handler | `Empty_one_or_more_handlers | `Full | `Indir ]

(* [Handler.t] shares the same memory representation as [Empty_one_or_more_handlers].
   This allows us to save one indirection.  The magic won't be needed anymore when this
   feature is accepted:

   http://caml.inria.fr/mantis/view.php?id=5528 *)
module Handler = struct

  type 'a t = 'a Types.Handler.t =
    { (* [run] is mutable so we can set it to [ignore] when the handler is removed.  This
         is used when we install a handler on a full ivar since it is immediately added to
         the scheduler. *)
      mutable run       : 'a -> unit
    ; execution_context : Execution_context.t
    (* [prev] and [next] circularly doubly link all handlers of the same ivar. *)
    ; mutable prev      : 'a t sexp_opaque
    ; mutable next      : 'a t sexp_opaque }
  [@@deriving sexp_of]

  let create run execution_context =
    (* An optimized implementation of:

       {[
         let rec t =
           { run
           ; execution_context
           ; prev              = t
           ; next              = t }
         in
         h1 ]}

       However the compilation of recursive value in OCaml is not optimal: the value is
       allocated twice and copied once (with a loop calling caml_modify).  This is not
       necessary for simple recursive definitions like this one.

       Instead we allocate the value with dummy fields and update them after. *)
    let t =
      { run
      ; execution_context
      ; prev              = Obj.magic None
      ; next              = Obj.magic None }
    in
    t.prev <- t;
    t.next <- t;
    t;
  ;;

  let create2 run1 execution_context1 run2 execution_context2 =
    (* An optimized implementation of:

       {[
         let rec t1 =
           { run               = run1
           ; execution_context = execution_context1
           ; prev              = t2
           ; next              = t2 }
         and t2 =
           { run               = run2
           ; execution_context = execution_context2
           ; prev              = t1
           ; next              = t1 }
         in
         t1 ]} *)
    let t1 =
      { run               = run1
      ; execution_context = execution_context1
      ; prev              = Obj.magic None
      ; next              = Obj.magic None }
    in
    let t2 =
      { run               = run2
      ; execution_context = execution_context2
      ; prev              = t1
      ; next              = t1 }
    in
    t1.prev <- t2;
    t1.next <- t2;
    t1;
  ;;

  let invariant t =
    Execution_context.invariant t.execution_context;
    let r = ref t.next in
    while not (phys_equal !r t) do
      let t1 = !r in
      assert (phys_equal t1.next.prev t1);
      Execution_context.invariant t1.execution_context;
      r := !r.next;
    done;
  ;;

  let is_singleton t = phys_equal t t.next

  let length t =
    let n = ref 1 in
    let r = ref t.next in
    while not (phys_equal !r t) do
      incr n;
      r := !r.next
    done;
    !n;
  ;;

  let enqueue t scheduler v = Scheduler.enqueue scheduler t.execution_context t.run v

  let schedule_jobs t v =
    let scheduler = Scheduler.t () in
    enqueue t scheduler v;
    let r = ref t.next in
    while not (phys_equal !r t) do
      enqueue !r scheduler v;
      r := !r.next;
    done;
  ;;

  let unlink t =
    t.next.prev <- t.prev;
    t.prev.next <- t.next;
    t.prev <- t;
    t.next <- t;
  ;;

  let add t run execution_context =
    let result =
      { run
      ; execution_context
      ; prev              = t.prev
      ; next              = t }
    in
    t.prev.next <- result;
    t.prev <- result;
    result;
  ;;

  (* [splice t1 t2] creates:

     {v
       --> t1 <--> ... <--> last1 <--> t2 <--> ... <--> last2 <--
       |                                                        |
       ----------------------------------------------------------
     v} *)
  let splice t1 t2 =
    let last1 = t1.prev in
    let last2 = t2.prev in
    last1.next <- t2;
    last2.next <- t1;
    t1.prev <- last2;
    t2.prev <- last1;
  ;;

  let of_list l =
    match l with
    | [] -> None
    | (run, execution_context) :: l ->
      let first = create run execution_context in
      let rec loop prev l =
        match l with
        | [] -> first.prev <- prev
        | (run, execution_context) :: l ->
          let t =
            { run
            ; execution_context
            ; prev
            ; next              = first }
          in
          prev.next <- t;
          loop t l
      in
      loop first l;
      Some first
  ;;

  let to_list first =
    let rec loop t acc =
      let acc = (t.run, t.execution_context) :: acc in
      if phys_equal t first
      then acc
      else (loop t.prev acc)
    in
    loop first.prev []
  ;;
end

type 'a t = 'a Types.Ivar.t =
  { mutable cell : ('a, any) cell }

(* The ['b] is used to encode the constructor.  This allows us to write functions that
   take only one of the constructors, with no runtime test.

   [Empty_one_or_more_handlers] must be the first constructor with arguments, so that it
   has the tag [0] and shares the same memory representation as [Handler.t] defined above.

   We maintain the invariant that the directed graph with ivars as nodes and [Indir]s as
   edges is acyclic.  The only functions that create an [Indir] are [squash] and
   [connect], and for those, the target of the [Indir] is always a non-[Indir].  Thus, the
   newly added edges are never part of a cycle. *)
and ('a, 'b) cell = ('a, 'b) Types.Cell.t =
  | Empty_one_or_more_handlers
    :  ('a -> unit) * Execution_context.t * 'a Handler.t * 'a Handler.t
    ->                                       ('a, [> `Empty_one_or_more_handlers ]) cell
  | Empty_one_handler
    :  ('a -> unit) * Execution_context.t -> ('a, [> `Empty_one_handler          ]) cell
  | Empty                                  : ('a, [> `Empty                      ]) cell
  | Full                             : 'a -> ('a, [> `Full                       ]) cell
  | Indir                          : 'a t -> ('a, [> `Indir                      ]) cell

type 'a ivar = 'a t

let%test_unit _ =
  let handler = Handler.create ignore Execution_context.main in
  let o1 =
    Obj.repr (Empty_one_or_more_handlers (ignore, Execution_context.main, handler, handler))
  in
  let o2 = Obj.repr handler in
  assert (Obj.tag o1 = Obj.tag o2);
  assert (Obj.size o1 = Obj.size o2)
;;

(* Conversion between mutable record and constructor. *)
external handler_of_constructor : ('a, [ `Empty_one_or_more_handlers ]) cell -> 'a Handler.t
  = "%identity"
external constructor_of_handler : 'a Handler.t -> ('a, [ `Empty_one_or_more_handlers ]) cell
  = "%identity"

(* Compiled as the identity. *)
let cell_of_handler handler =
  match constructor_of_handler handler with
  | Empty_one_or_more_handlers _ as x -> (x :> (_, any) cell)
;;

let equal (t : _ t) t' = phys_equal t t'

let indir t = { cell = Indir t }

include Scheduler.Ivar

(* [squash t] returns the non-[Indir] ivar at the end of the (possibly empty) chain of
   [Indir]s starting with [t] and ensures that all [Indir]s along that chain are replaced
   with an [Indir] pointing to the end of the chain. *)
let squash =
  let rec follow indir t =
    (* [indir = Indir t] *)
    match t.cell with
    | Indir t' as indir' -> follow indir' t'
    | _ -> indir
  in
  let rec update t indir =
    match t.cell with
    | Indir t' -> t.cell <- indir; update t' indir
    | _ -> t
  in
  fun t ->
    match t.cell with
    | Indir t' ->
      begin match t'.cell with
      | Indir t'' as indir -> update t (follow indir t'')
      | _ -> t' (* nothing to do, since [t] is a chain with a single [Indir] *)
      end
    | _ -> t (* nothing to do, since [t] isn't an [Indir]. *)
;;

let invariant a_invariant t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> a_invariant a
  | Empty -> ()
  | Empty_one_handler (_, execution_context) ->
    Execution_context.invariant execution_context
  | Empty_one_or_more_handlers _ as cell ->
    Handler.invariant (handler_of_constructor cell);
;;

let sexp_of_t sexp_of_a t : Sexp.t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> List [ Atom "Full"; sexp_of_a a ]
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> Atom "Empty"
;;

let peek t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> Some a
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> None
;;

let value t ~if_empty_then_failwith =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> a
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ ->
    failwith if_empty_then_failwith
;;

let value_exn t =
  value t ~if_empty_then_failwith:"Ivar.value_exn called on empty ivar"
;;

let is_empty t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full _ -> false
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> true
;;

let is_full t = not (is_empty t)

let fill t v =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full _ -> raise_s [%message "Ivar.fill of full ivar" (t : _ t)]
  | Empty -> t.cell <- Full v;
  | Empty_one_handler (run, execution_context) ->
    t.cell <- Full v;
    Scheduler.(enqueue (t ())) execution_context run v;
  | Empty_one_or_more_handlers _ as cell ->
    t.cell <- Full v;
    Handler.schedule_jobs (handler_of_constructor cell) v;
;;

let remove_handler t (handler : _ Handler.t) =
  handler.run <- ignore;
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Empty | Empty_one_handler _ ->
    (* These are only possible if [handler] was already removed.  *)
    ()
  | Full _ ->
    (* This is possible if [t] was filled before we try to remove the handler.  E.g.
       [Deferred.choose] will do this. *)
    ()
  | Empty_one_or_more_handlers _ as cell ->
    if Handler.is_singleton handler
    then (t.cell <- Empty)
    else (
      if phys_equal handler (handler_of_constructor cell)
      then (t.cell <- cell_of_handler handler.next);
      Handler.unlink handler);
;;

let add_handler t run execution_context =
  let t = squash t in
  match t.cell with
  | Indir _ ->
    assert false (* fulfilled by [squash] *)
  | Empty ->
    let handler = Handler.create run execution_context in
    t.cell <- cell_of_handler handler;
    handler
  | Empty_one_handler (run', execution_context') ->
    let handler =
      Handler.create2
        run  execution_context
        run' execution_context'
    in
    t.cell <- cell_of_handler handler;
    handler
  | Empty_one_or_more_handlers _ as cell ->
    Handler.add (handler_of_constructor cell) run execution_context;
  | Full v ->
    let handler = Handler.create run execution_context in
    (* [run] calls [handler.run], which, if [handler] has been removed, has been changed
       to [ignore]. *)
    let run v = handler.run v in
    Scheduler.(enqueue (t ())) execution_context run v;
    handler
;;

let has_handlers t =
  let t = squash t in
  match t.cell with
  | Indir _ ->
    assert false (* fulfilled by [squash] *)
  | Empty_one_handler _
  | Empty_one_or_more_handlers _ ->
    true
  | Empty
  | Full _ ->
    false

let upon' t run = add_handler t run Scheduler.(current_execution_context (t ()))

(* [upon] is conceptually the same as:

   {[
     let upon t f = ignore (upon' t run) ]}

   However, below is a more efficient implementation, which is worth doing because [upon]
   is very widely used and is so much more common than [upon'].  The below implementation
   avoids the use of the bag of handlers in the extremely common case of one handler for
   the deferred. *)
let upon =
  fun t run ->
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    let t = squash t in
    match t.cell with
    | Indir _ ->
      assert false (* fulfilled by [squash] *)
    | Full v -> Scheduler.enqueue scheduler execution_context run v;
    | Empty -> t.cell <- Empty_one_handler (run, execution_context);
    | Empty_one_handler (run', execution_context') ->
      t.cell <- cell_of_handler
                  (Handler.create2
                     run  execution_context
                     run' execution_context');
    | Empty_one_or_more_handlers _ as cell ->
      ignore (Handler.add (handler_of_constructor cell) run execution_context
              : _ Handler.t);
;;

(* [connect] takes ivars [bind_result] and [bind_rhs], and makes [bind_rhs]
   be an [Indir] pointing to the non-indir cell reachable from [bind_result].  On entry
   to [connect], [bind_result] and [bind_rhs] may be chains, since [bind_rhs] is an
   arbitrary user-supplied deferred, and [bind_result] is returned to the user prior to
   being [connect]ed, and may have been converted to an indirection in the case of
   right-nested binds.

   The purpose of [connect] is to make tail-recursive bind loops use constant space.
   E.g.:

   {[
     let rec loop i =
       if i = 0
       then return ()
       else (
         let%bind () = after (sec 1.) in
         loop (i - 1)) ]}

   [connect] makes intermediate bind results all be [Indir]s pointing at the outermost
   bind, rather than being a linear-length chain, with each pointing to the previous one.
   Then, since the program is only holding on to the innermost and outermost binds all the
   intermediate ones can be garbage collected.

   [connect] works by squashing its arguments so that the [bind_rhs] always points at the
   ultimate result. *)
let connect =
  (* [repoint_indirs ~ivar ~indir ~bind_result] repoints to [indir] all the ivars in the
     chain reachable from [ivar], and returns the non-[Indir] cell at the end of the
     chain.  After repointing, we will merge the handlers in that cell with the handlers
     in [bind_result], and put the merged set of handlers in [bind_result]. *)
  let rec repoint_indirs ~ivar ~indir ~bind_result =
    let cell = ivar.cell in
    match cell with
    | Indir ivar' -> ivar.cell <- indir; repoint_indirs ~ivar:ivar' ~indir ~bind_result
    | Full _ -> cell
    | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ ->
      (* It is possible that [bind_result] and [bind_rhs] are not equal, but their chains
         of indirs lead to the same non-[Indir] cell, in which case we cannot set that
         cell to point to itself, because that would introduce a cycle. *)
      if not (phys_equal ivar bind_result) then (ivar.cell <- indir);
      cell
  in
  fun ~bind_result ~bind_rhs ->
    if not (phys_equal bind_result bind_rhs)
    then (
      let bind_result = squash bind_result in
      let indir = Indir bind_result in
      let bind_rhs_contents = repoint_indirs ~ivar:bind_rhs ~indir ~bind_result in
      (* update [bind_result] with the union of handlers in [bind_result] and
         [bind_rhs] *)
      match bind_result.cell, bind_rhs_contents with
      | Indir _, _ | _, Indir _
        -> assert false (* fulfilled by [squash] and [repoint_indirs] *)
      (* [connect] is only used in bind, whose ivar is only ever exported as a read-only
         deferred.  Thus, [bind_result] must be empty. *)
      | Full _, _ -> assert false
      | _, Empty -> ()
      | Empty, _ -> bind_result.cell <- bind_rhs_contents;
      | Empty_one_handler (run, execution_context), Full v ->
        bind_result.cell <- bind_rhs_contents;
        Scheduler.(enqueue (t ())) execution_context run v;
      | Empty_one_or_more_handlers _ as cell, Full v ->
        bind_result.cell <- bind_rhs_contents;
        Handler.schedule_jobs (handler_of_constructor cell) v;
      | Empty_one_handler (run1, execution_context1),
        Empty_one_handler (run2, execution_context2) ->
        let handler1 =
          Handler.create2
            run1 execution_context1
            run2 execution_context2
        in
        bind_result.cell <- cell_of_handler handler1;
      | (Empty_one_or_more_handlers _ as cell1),
        Empty_one_handler (run2, execution_context2) ->
        let handler1 = handler_of_constructor cell1 in
        ignore (Handler.add handler1 run2 execution_context2 : _ Handler.t);
      | Empty_one_handler (run1, execution_context1),
        (Empty_one_or_more_handlers _ as cell2) ->
        let handler2 = handler_of_constructor cell2 in
        let handler1 = Handler.add handler2 run1 execution_context1 in
        bind_result.cell <- cell_of_handler handler1;
      | (Empty_one_or_more_handlers _ as cell1),
        (Empty_one_or_more_handlers _ as cell2) ->
        let handler1 = handler_of_constructor cell1 in
        Handler.splice handler1 (handler_of_constructor cell2));
;;

let%test_module _ =
  (module struct

    let (-->) i1 i2 =
      match i1.cell with
      | Indir i2' -> phys_equal i2' i2
      | _ -> false
    ;;

    let r = ref 0
    let run i = r := !r + i
    let execution_context = Execution_context.main
    let empty_one_handler = Empty_one_handler (run, execution_context)

    let execution_context1 = Execution_context.create_like Execution_context.main
    let execution_context2 = Execution_context.create_like Execution_context.main
    let handler1 = (run, execution_context1)
    let handler2 = (run, execution_context2)

    let eq_handlers (r1, ec1) (r2, ec2) = phys_equal r1 r2 && phys_equal ec1 ec2

    let cell_of_handler_list l =
      match Handler.of_list l with
      | None -> Empty
      | Some h -> cell_of_handler h;
    ;;

    let handler_list_of_cell c = Handler.to_list (handler_of_constructor c)

    let squash t = ignore (squash t : _ t)

    let connect bind_result bind_rhs = connect ~bind_result ~bind_rhs

    let stabilize () = Result.ok_exn (Scheduler.(stabilize (t ())))

    (* ==================== peek, is_empty, is_full ==================== *)

    let%test_unit _ =
      let t = create () in
      assert (is_empty t);
      assert (not (is_full t));
      assert (peek t = None)
    ;;

    let%test_unit _ =
      let t = create_full 13 in
      assert (not (is_empty t));
      assert (is_full t);
      assert (peek t = Some 13)
    ;;

    (* ==================== equal ==================== *)

    let%test_unit _ =
      let t1 = create () in
      let t2 = create () in
      assert (equal t1 t1);
      assert (equal t2 t2);
      assert (not (equal t1 t2))
    ;;

    (* ==================== squash ==================== *)

    let%test_unit _ =
      let t = create () in
      squash t;
      assert (t.cell = Empty)
    ;;

    let%test_unit _ =
      let t1 = create () in
      let t2 = create_with_cell (Indir t1) in
      squash t2;
      assert (t2 --> t1)
    ;;

    let%test_unit _ =
      let t1 = create () in
      let t2 = create_with_cell (Indir t1) in
      let t3 = create_with_cell (Indir t2) in
      let t4 = create_with_cell (Indir t3) in
      squash t4;
      assert (t2 --> t1);
      assert (t3 --> t1);
      assert (t4 --> t1)
    ;;

    (* ==================== fill ==================== *)

    let%test_unit _ =
      let t = create () in
      fill t 13;
      assert (peek t = Some 13)
    ;;

    let%test_unit _ =
      let t = create () in
      fill t 13;
      assert (try fill t 14; false with _ -> true)
    ;;

    let%test_unit _ =
      let t1 = create () in
      let t2 = create_with_cell (Indir t1) in
      fill t2 13;
      assert (peek t1 = Some 13);
      assert (peek t2 = Some 13)
    ;;

    let%test_unit _ =
      r := 13;
      let t = create_with_cell empty_one_handler in
      fill t 17;
      stabilize ();
      assert (t.cell = Full 17);
      assert (!r = 30)
    ;;

    let%test_unit _ =
      r := 13;
      let t = create_with_cell (cell_of_handler_list [ handler1; handler2]) in
      fill t 17;
      stabilize ();
      assert (t.cell = Full 17);
      assert (!r = 47)
    ;;

    (* ==================== upon ==================== *)

    let%test_unit _ =
      let t = create () in
      r := 1;
      upon t (fun i -> r := !r + i);
      stabilize ();
      assert (!r = 1);
      fill t 13;
      stabilize ();
      assert (!r = 14)
    ;;

    let%test_unit _ =
      let t = create () in
      r := 1;
      upon t (fun i -> r := !r + i);
      upon t (fun i -> r := !r + i);
      stabilize ();
      assert (!r = 1);
      fill t 13;
      stabilize ();
      assert (!r = 27)
    ;;

    let%test_unit _ =
      let t = create () in
      r := 1;
      let num_handlers = 1000 in
      for _ = 1 to num_handlers do
        upon t (fun i -> r := !r + i);
      done;
      stabilize ();
      assert (!r = 1);
      fill t 13;
      stabilize ();
      assert (!r = num_handlers * 13 + 1)
    ;;

    let%test_unit _ =
      let t1 = create () in
      let t2 = create_with_cell (Indir t1) in
      r := 1;
      upon t2 (fun i -> r := !r + i);
      fill t1 13;
      stabilize ();
      assert (!r = 14)
    ;;

    (* ==================== upon' ==================== *)

    let%test_unit _ =
      let t = create () in
      r := 1;
      let u = upon' t (fun i -> r := !r + i) in
      stabilize ();
      assert (!r = 1);
      remove_handler t u;
      fill t 13;
      stabilize ();
      assert (!r = 1)
    ;;

    let%test_unit _ =
      let t = create () in
      r := 1;
      let u = upon' t (fun i -> r := !r + i) in
      stabilize ();
      assert (!r = 1);
      fill t 13;
      stabilize ();
      assert (!r = 14);
      remove_handler t u;
      stabilize ();
      assert (!r = 14)
    ;;

    let%test_unit _ =
      let t = create () in
      r := 1;
      let u1 = upon' t (fun i -> r := !r + i) in
      let _ : _ Handler.t = upon' t (fun i -> r := !r + i) in
      stabilize ();
      assert (!r = 1);
      remove_handler t u1;
      fill t 13;
      stabilize ();
      assert (!r = 14)
    ;;

    let%test_unit _ =
      let t1 = create () in
      let t2 = create () in
      r := 1;
      let u1 = upon' t1 (fun () -> r := !r + 13) in
      let _ : _ Handler.t = upon' t2 (fun () -> r := !r + 17) in
      connect t1 t2;
      remove_handler t1 u1;
      fill t1 ();
      stabilize ();
      assert (!r = 18)
    ;;

    (* ==================== connect ==================== *)

    let%test_unit _ =
      let i1 = create () in
      let i2 = create () in
      connect i1 i2;
      stabilize ();
      assert (i1.cell = Empty);
      assert (i2 --> i1)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let b1 = create () in
      let b2 = create_with_cell (Indir b1) in
      connect a1 b2;
      stabilize ();
      assert (a1.cell = Empty);
      assert (b1 --> a1);
      assert (b2 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let a2 = create_with_cell (Indir a1) in
      let b1 = create () in
      let b2 = create_with_cell (Indir b1) in
      connect a2 b2;
      stabilize ();
      assert (a1.cell = Empty);
      assert (a2 --> a1);
      assert (b1 --> a1);
      assert (b2 --> a1)
    ;;

    let%test_unit _ =
      let a = create () in
      let b = create_with_cell (Indir a) in
      let c = create_with_cell (Indir a) in
      connect b c;
      stabilize ();
      assert (a.cell = Empty);
      assert (b --> a);
      assert (c --> a)
    ;;

    let%test_unit _ =
      let a1 = create () in
      connect a1 a1;
      stabilize ();
      assert (a1.cell = Empty)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let a2 = create_with_cell (Indir a1) in
      connect a1 a2;
      stabilize ();
      assert (a1.cell = Empty);
      assert (a2 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let a2 = create_with_cell (Indir a1) in
      connect a2 a1;
      stabilize ();
      assert (a1.cell = Empty);
      assert (a2 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let b1 = create_with_cell empty_one_handler in
      connect a1 b1;
      stabilize ();
      assert (phys_equal a1.cell empty_one_handler);
      assert (b1 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create_with_cell empty_one_handler in
      let b1 = create () in
      connect a1 b1;
      stabilize ();
      assert (phys_equal a1.cell empty_one_handler);
      assert (b1 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create_with_cell empty_one_handler in
      let b1 = create_with_cell empty_one_handler in
      connect a1 b1;
      stabilize ();
      begin match a1.cell with
      | Empty_one_or_more_handlers _ as c ->
        assert (Handler.length (handler_of_constructor c) = 2);
        List.iter (handler_list_of_cell c) ~f:(fun (run', execution_context') ->
          assert (phys_equal execution_context' execution_context);
          assert (phys_equal run' run));
      | _ -> assert false
      end;
      assert (b1 --> a1)
    ;;

    let%test_unit _ =
      let empty_many_handlers1 = cell_of_handler_list [ handler1 ] in
      let a1 = create_with_cell empty_many_handlers1 in
      let b1 = create_with_cell (cell_of_handler_list [ handler2 ]) in
      connect a1 b1;
      stabilize ();
      assert (phys_equal a1.cell empty_many_handlers1);
      begin match a1.cell with
      | Empty_one_or_more_handlers _ as c ->
        begin match handler_list_of_cell c with
        | [ h1; h2 ] ->
          assert (eq_handlers h1 handler1 && eq_handlers h2 handler2
                  || eq_handlers h1 handler2 && eq_handlers h2 handler1);
        | _ -> assert false
        end
      | _ -> assert false
      end;
      assert (b1 --> a1)
    ;;

    let%test_unit _ =
      let empty_many_handlers1 = cell_of_handler_list [ handler1 ] in
      let a1 = create_with_cell empty_many_handlers1 in
      let b1 = create_with_cell empty_one_handler in
      connect a1 b1;
      stabilize ();
      assert (phys_equal a1.cell empty_many_handlers1);
      begin match a1.cell with
      | Empty_one_or_more_handlers _ as c ->
        begin match handler_list_of_cell c with
        | [ h1; h2 ] ->
          assert (eq_handlers h1 handler1 || eq_handlers h2 handler1);
        | _ -> assert false
        end
      | _ -> assert false
      end;
      assert (b1 --> a1)
    ;;

    let%test_unit _ =
      let i1 = create () in
      let i2 = create_with_cell (Full 13) in
      connect i1 i2;
      stabilize ();
      assert (i1.cell = Full 13);
      assert (i2.cell = Full 13)
    ;;

    let%test_unit _ =
      let a1 = create () in
      let b1 = create_with_cell (Full 13) in
      let b2 = create_with_cell (Indir b1) in
      connect a1 b2;
      stabilize ();
      assert (a1.cell = Full 13);
      assert (b1.cell = Full 13);
      assert (b2 --> a1)
    ;;

    let%test_unit _ =
      let a1 = create_with_cell empty_one_handler in
      let b1 = create_with_cell (Full 13) in
      connect a1 b1;
      stabilize ();
      assert (a1.cell = Full 13);
      assert (b1.cell = Full 13)
    ;;

  end)
