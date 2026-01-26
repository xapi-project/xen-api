(*
 *
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let rpc_of ty x = Rpcmarshal.marshal ty.Rpc.Types.ty x

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module StringMap = Map.Make (String)

let push_with_coalesce should_keep item queue =
  (* [filter_with_memory p xs] returns elements [x \in xs] where [p (x_i,
     [x_0...x_i-1])] *)
  let filter_with_memory p xs =
    List.fold_left (fun (acc, xs) x -> (xs :: acc, x :: xs)) ([], []) xs
    |> fst
    |> List.rev
    |> List.combine xs
    (* association list of (element, all previous elements) *)
    |> List.filter p
    |> List.map fst
  in
  let to_list queue = Queue.fold (fun xs x -> x :: xs) [] queue |> List.rev in
  let of_list xs =
    let q = Queue.create () in
    List.iter (fun x -> Queue.push x q) xs ;
    q
  in
  Queue.push item queue ;
  let queue' =
    to_list queue
    |> filter_with_memory (fun (this, prev) -> should_keep this prev)
    |> of_list
  in
  Queue.clear queue ;
  Queue.transfer queue' queue

module Queues = struct
  (** A set of queues where 'pop' operates on each queue in a round-robin
      fashion *)

  (** Each distinct 'tag' value creates a separate virtual queue *)
  type tag = string

  type 'a t = {
      mutable qs: 'a Queue.t StringMap.t
    ; mutable last_tag: string
    ; m: Mutex.t
    ; c: Condition.t
  }

  let create () =
    {
      qs= StringMap.empty
    ; last_tag= ""
    ; m= Mutex.create ()
    ; c= Condition.create ()
    }

  let get tag qs =
    with_lock qs.m (fun () ->
        match StringMap.find_opt tag qs.qs with
        | Some x ->
            x
        | None ->
            Queue.create ()
    )

  let tags qs =
    with_lock qs.m (fun () -> StringMap.fold (fun x _ acc -> x :: acc) qs.qs [])

  let get_last_tag qs = with_lock qs.m (fun () -> qs.last_tag)

  let push_with_coalesce should_keep tag item qs =
    with_lock qs.m (fun () ->
        let q =
          match StringMap.find_opt tag qs.qs with
          | Some x ->
              x
          | None ->
              Queue.create ()
        in
        push_with_coalesce should_keep item q ;
        qs.qs <- StringMap.add tag q qs.qs ;
        Condition.signal qs.c
    )

  let pop qs =
    with_lock qs.m (fun () ->
        while StringMap.is_empty qs.qs do
          Condition.wait qs.c qs.m
        done ;
        (* partition based on last_tag *)
        let before, after =
          StringMap.partition (fun x _ -> x <= qs.last_tag) qs.qs
        in
        (* the min_binding in the 'after' is the next queue *)
        let last_tag, q =
          StringMap.min_binding
            ( if StringMap.is_empty after then
                before
              else
                after
            )
        in
        qs.last_tag <- last_tag ;
        let item = Queue.pop q in
        (* remove empty queues from the whole mapping *)
        qs.qs <-
          ( if Queue.is_empty q then
              StringMap.remove last_tag qs.qs
            else
              qs.qs
          ) ;
        (last_tag, item)
    )

  let transfer_tag tag a b =
    with_lock a.m (fun () ->
        with_lock b.m (fun () ->
            if StringMap.mem tag a.qs then (
              b.qs <- StringMap.add tag (StringMap.find tag a.qs) b.qs ;
              a.qs <- StringMap.remove tag a.qs ;
              Condition.signal b.c
            )
        )
    )
end

type rpc_t = Rpc.t

let typ_of_rpc_t =
  Rpc.Types.(
    Abstract
      {
        aname= "Rpc.t"
      ; test_data= [Rpc.Null]
      ; rpc_of= (fun x -> x)
      ; of_rpc= (fun x -> Ok x)
      }
  )

module type Item = sig
  type t

  val describe_item : t -> string

  val dump_task : t -> Rpc.t

  val execute : t -> unit

  val finally : t -> unit

  val should_keep : t -> t list -> bool
end

module type Dump = sig
  type t

  val typ_of : t Rpc.Types.typ

  val make : unit -> t
end

module type S = sig
  type item

  module Redirector : sig
    type t

    module Dump : Dump

    val default : t

    val parallel_queues : t

    val nested_parallel_queues : t

    val receive_memory_queues : t

    val push : t -> string -> item -> unit

    val alias : tag:string -> alias:string -> unit

    val to_string : t -> string
  end

  module WorkerPool : sig
    module Dump : Dump

    val start : int -> unit

    val set_size : int -> unit
  end
end

module Make (I : Item) = struct
  open I

  type item = I.t

  module Redirector = struct
    type t = {queues: item Queues.t; mutex: Mutex.t}

    (* When a thread is not actively processing a queue, items are placed here: *)
    let default = {queues= Queues.create (); mutex= Mutex.create ()}

    (* We create another queue only for Parallel atoms so as to avoid a situation
     where Parallel atoms can not progress because all the workers available for
     the default queue are used up by other operations depending on further
     Parallel atoms, creating a deadlock. *)
    let parallel_queues = {queues= Queues.create (); mutex= Mutex.create ()}

    (* We create another queue only for Nested_parallel atoms for the same reason
     as parallel_queues. When a Nested_parallel atom is inside a Parallel atom,
     they are both using a worker whilst not doing any work, so they each need
     additional space to prevent a deadlock. *)
    let nested_parallel_queues =
      {queues= Queues.create (); mutex= Mutex.create ()}

    (* We create another queue only for VM_receive_memory operations for the same reason again.
     Migration spawns 2 operations, send and receive, so if there is limited available worker space
     a deadlock can happen when VMs are migrating between hosts or on localhost migration
     as the receiver has no free workers to receive memory. *)
    let receive_memory_queues =
      {queues= Queues.create (); mutex= Mutex.create ()}

    (* we do not want to use = when comparing queues: queues can contain
     (uncomparable) functions, and we are only interested in comparing the
     equality of their static references *)
    let is_same_redirector q1 q2 = q1 == q2

    let to_string r =
      match r with
      | w when is_same_redirector w parallel_queues ->
          "Parallel"
      | w when is_same_redirector w nested_parallel_queues ->
          "Nested_parallel"
      | _ ->
          "Default"

    (* When a thread is actively processing a queue, items are redirected to a
     thread-private queue *)
    let overrides = ref StringMap.empty

    let aliases = ref StringMap.empty

    let m = Mutex.create ()

    let push t tag item =
      Debug.with_thread_associated "queue"
        (fun () ->
          with_lock m (fun () ->
              let real_tag, aliased =
                match StringMap.find_opt tag !aliases with
                | Some x ->
                    (x, true)
                | None ->
                    (tag, false)
              in
              let q, redirected =
                match StringMap.find_opt real_tag !overrides with
                | Some x ->
                    (x, true)
                | None ->
                    (t.queues, false)
              in
              debug "Queue.push %s onto %s%s:[ %s ]" (describe_item item)
                ( if aliased then
                    "aliased "
                  else if redirected then
                    "redirected "
                  else
                    ""
                )
                real_tag
                (String.concat ", "
                   (List.rev
                      (Queue.fold
                         (fun acc item -> describe_item item :: acc)
                         [] (Queues.get tag q)
                      )
                   )
                ) ;
              Queues.push_with_coalesce should_keep real_tag item q
          )
        )
        ()

    let pop t () =
      (* We must prevent worker threads all calling Queues.pop before we've
       successfully put the redirection in place. Otherwise we end up with
       parallel threads operating on the same VM. *)
      with_lock t.mutex (fun () ->
          let tag, item = Queues.pop t.queues in
          with_lock m (fun () ->
              let q = Queues.create () in
              Queues.transfer_tag tag t.queues q ;
              overrides := StringMap.add tag q !overrides ;
              (* All items with [tag] will enter queue [q] *)
              (tag, q, item)
          )
      )

    let finished t tag queue =
      with_lock m (fun () ->
          Queues.transfer_tag tag queue t.queues ;
          overrides := StringMap.remove tag !overrides ;
          (* All items with [tag] will enter the queues queue *)
          (* Sanity check: there should be no override for tag in overrides *)
          aliases := StringMap.filter (fun _ v -> v <> tag) !aliases
      )

    let alias ~tag ~alias =
      with_lock m (fun () ->
          if StringMap.mem tag !overrides then (
            debug "Queue: Aliasing existing tag '%s' to new tag '%s'" tag alias ;
            aliases := StringMap.add alias tag !aliases
          ) else
            debug "Queue: Warning: Not aliasing non-existing tag"
      )

    module Dump = struct
      type q = {tag: string; items: string list} [@@deriving rpcty]

      type t = q list [@@deriving rpcty]

      let rpc_of_t = Rpcmarshal.marshal typ_of

      let make () =
        with_lock m (fun () ->
            let one queue =
              List.map
                (fun t ->
                  {
                    tag= t
                  ; items=
                      List.rev
                        (Queue.fold
                           (fun acc b -> describe_item b :: acc)
                           [] (Queues.get t queue)
                        )
                  }
                )
                (Queues.tags queue)
            in
            List.concat_map one
              (default.queues
              :: parallel_queues.queues
              :: nested_parallel_queues.queues
              :: receive_memory_queues.queues
              :: List.map snd (StringMap.bindings !overrides)
              )
        )
    end
  end

  module Worker = struct
    type state = Idle | Processing of item | Shutdown_requested | Shutdown

    type t = {
        mutable state: state
      ; mutable shutdown_requested: bool
      ; m: Mutex.t
      ; c: Condition.t
      ; mutable t: Thread.t option
      ; redirector: Redirector.t
    }

    let get_state_locked t =
      if t.shutdown_requested then
        Shutdown_requested
      else
        t.state

    let get_state t = with_lock t.m (fun () -> get_state_locked t)

    let join t =
      with_lock t.m (fun () ->
          assert (t.state = Shutdown) ;
          Option.iter Thread.join t.t
      )

    let is_active t =
      with_lock t.m (fun () ->
          match get_state_locked t with
          | Idle | Processing _ ->
              true
          | Shutdown_requested | Shutdown ->
              false
      )

    let shutdown t =
      with_lock t.m (fun () ->
          if not t.shutdown_requested then (
            t.shutdown_requested <- true ;
            true (* success *)
          ) else
            false
      )

    let restart t =
      with_lock t.m (fun () ->
          if t.shutdown_requested && t.state <> Shutdown then (
            t.shutdown_requested <- false ;
            true (* success *)
          ) else
            false
      )

    let create redirector =
      let t =
        {
          state= Idle
        ; shutdown_requested= false
        ; m= Mutex.create ()
        ; c= Condition.create ()
        ; t= None
        ; redirector
        }
      in
      let thread =
        Thread.create
          (fun () ->
            while
              not
                (with_lock t.m (fun () ->
                     if t.shutdown_requested then t.state <- Shutdown ;
                     t.shutdown_requested
                 )
                )
            do
              with_lock t.m (fun () -> t.state <- Idle) ;
              let tag, queue, item = Redirector.pop redirector () in
              (* blocks here *)
              debug "Queue.pop returned %s" (describe_item item) ;
              with_lock t.m (fun () -> t.state <- Processing item) ;
              ( try execute item
                with e -> debug "Queue caught: %s" (Printexc.to_string e)
              ) ;
              Redirector.finished redirector tag queue ;
              (* The task must have succeeded or failed. *)
              try finally item
              with e -> debug "Queue finally caught: %s" (Printexc.to_string e)
            done
          )
          ()
      in
      t.t <- Some thread ;
      t
  end

  module WorkerPool = struct
    module Date = Clock.Date

    (* Store references to Worker.ts here *)
    let pool = ref []

    let m = Mutex.create ()

    module Dump = struct
      type w = {state: string; task: rpc_t option} [@@deriving rpcty]

      type t = w list [@@deriving rpcty]

      let make () =
        with_lock m (fun () ->
            List.map
              (fun t ->
                match Worker.get_state t with
                | Worker.Idle ->
                    {state= "Idle"; task= None}
                | Worker.Processing item ->
                    {
                      state= Printf.sprintf "Processing %s" (describe_item item)
                    ; task= Some (dump_task item)
                    }
                | Worker.Shutdown_requested ->
                    {state= "Shutdown_requested"; task= None}
                | Worker.Shutdown ->
                    {state= "Shutdown"; task= None}
              )
              !pool
        )
    end

    (* Compute the number of active threads ie those which will continue to
     operate *)
    let count_active queues =
      with_lock m (fun () ->
          List.map
            (fun w ->
              Redirector.is_same_redirector w.Worker.redirector queues
              && Worker.is_active w
            )
            !pool
          |> List.filter (fun x -> x)
          |> List.length
      )

    let find_one queues f =
      List.fold_left
        (fun acc x ->
          acc
          || (Redirector.is_same_redirector x.Worker.redirector queues && f x)
        )
        false

    (* Clean up any shutdown threads and remove them from the master list *)
    let gc queues pool =
      List.fold_left
        (fun acc w ->
          if
            Redirector.is_same_redirector w.Worker.redirector queues
            && Worker.get_state w = Worker.Shutdown
          then (
            Worker.join w ; acc
          ) else
            w :: acc
        )
        [] pool

    let incr queues =
      debug "Adding a new worker to the thread pool" ;
      with_lock m (fun () ->
          pool := gc queues !pool ;
          if not (find_one queues Worker.restart !pool) then
            pool := Worker.create queues :: !pool
      )

    let decr queues =
      debug "Removing a worker from the thread pool" ;
      with_lock m (fun () ->
          pool := gc queues !pool ;
          if not (find_one queues Worker.shutdown !pool) then
            debug "There are no worker threads left to shutdown."
      )

    let start size =
      for _i = 1 to size do
        incr Redirector.default ;
        incr Redirector.parallel_queues ;
        incr Redirector.nested_parallel_queues ;
        incr Redirector.receive_memory_queues
      done

    let set_size size =
      let inner queues =
        let active = count_active queues in
        debug "XXX active = %d" active ;
        for _i = 1 to max 0 (size - active) do
          incr queues
        done ;
        for _i = 1 to max 0 (active - size) do
          decr queues
        done
      in
      inner Redirector.default ;
      inner Redirector.parallel_queues ;
      inner Redirector.nested_parallel_queues ;
      inner Redirector.receive_memory_queues
  end
end
