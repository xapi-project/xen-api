(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module type FLOW = sig
  type t
  type label
  val max_capacity : label -> t
  val flow : label -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val zero : t
  val compare : t -> t -> int
end

module type G_GOLDBERG_TARJAN = sig
  type t
  module V : Sig.COMPARABLE
  module E : Sig.EDGE with type vertex = V.t
  val nb_vertex : t -> int
  val nb_edges : t -> int
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Goldberg_Tarjan
  (G: G_GOLDBERG_TARJAN)
  (F: FLOW with type label = G.E.label) =
struct

  (* This code is a contribution of Guyslain Naves

  Design notes:
   This is an implementation of the classical Goldberg-Tarjan push-relabel
   algorithm to compute maximum flow in directed graphs with upper capacities
   on arcs. Several common optimizations are implemented to make it
   more efficient than the pseudocode found in most textbooks on algorithms.

   About the push-relabel algorithm.
   --------------------------------------

   Instead of keeping a valid flow and improving it by iteration (similar to
   Ford-Fulkerson algorithm and its variants), the push-relabel always keep
   a preflow and try make it becoe a flow. A preflow is a function on arcs that
   violates the flow condition:
     "flow that enters = flow that leaves (in every non-terminal vertex)"
   and replaces it by:
     " flow that enters >= flow that leaves (in every non-source vertex)"
   That means that any vertex may have *excessive* flow entering it.

   The algorithm proceed by making flow going down. Here down is defined
   by a *potential*, an integer attached to every vertex. The excess at some
   vertex can be routed to a successor in the residual graph with lower
   potential. This is a *push* operation on the corresponding arc.

   Pushing along all arcs leaving a vertex is a *discharge* of this vertex.

   If a vertex is excessive, but no successor has lower potential, then we
   increase the potential to make it slightly higher than at least one
   of its successor. This is a *relabel* operation of that vertex.

   The source (potential = n) and sink (potential = 0) may never be relabel.

   The algorithm consists in doing push and relabel steps,
   until no more are possible. Then the preflow is a maximum flow.

   Optimizations.
   --------------

   - The simplest (and less efficient) way to optimize this algorithm is
   to play with the order on which push and relabel operations are performed.
   Here, the strategy used is the following:
   1) sort excessive vertices by decreasing potential
   2) for each vertex in that order:
      a) discharge it
      b) if still in excess, relabel it
   (see [augmenting_step] and [discharge])
   This is a basic strategy that could be improved.

   - Textbook algorithms starts with non-source vertices with potential 0.
   This forces the algorithm to perform a lot of relabel operations to get
   to a more realistic and usable potential function. Here we use as initial
   potential the distance from a vertex to the sink (even for the source).
   (see [initialize_potential])

   - The most important optimization: empirically one can check that the
   push-relabel algorithm converges very quickly toward a preflow that maximizes
   the flow sent to the sink. But then it takes very long to send back the
   excessive flow to the source.  Here we detect every few iterations if the
   preflow is maximal. This is done by a bfs in the reversal of the residual
   graph, by determining whether the source is reachable.
   (see [is_maximum_preflow]).
   Once the preflow is maximum (first pahse), we compute a maximum preflow from
   sink to source in the reversed graph, with maximum capacities given by the
   values of the maximum preflow just computed (second phase). This will
   compute a reversed maximum preflow that must be actually a flow.
   (see [compute_maximum_flow], and the use of [forward_context]
   and [backward_context] for each of the two phases)


   Implementation.
   ---------------

   The most important thing is to understand that we are interested only
   in the residual graph, and not by the original graph. Original arcs
   may appears in one or two directions in the residual graph. This is
   why we manipulate almost only [residual_arc] and no [arc = G.E.t].

   It also implies that the incident arcs of a vertex are not those in
   the original graph. Because we will work with both the residual graph
   and its reversal, and that it depends on the current flow values, we
   define two functions [incidence_residual] and [incidence_reversal].
   Notice that their roles interchanged during the second phase.

   [Forward] and [Backward] only refers to the orientation compared to the
   orientation in the graph. Hence, in a reversed graph, backward residual arcs
   are [Forward], and forward residual arcs are [Backward].

   We define a type [context] containing the current state of computation.
   We hide the data structures in [context] by providing [set] and [get]
   functions. It makes the code more readable, and easier to modify if one
   would like to change the data structures.


   Structure of the code:
   The first part of the code contains mostly helpers, up to the bfs algorithm.
   With the bfs comes the function to compute the initial potential and
   check the maximality of a preflow.
   Then we define the push, relabel, discharge operations, and the functions
   computing maximal preflows.
   Finally we define how to initialize a context, and the max flow algorithm.

   We choose to require by a functor the implementations for the main data
   structures used by the algorithm. VMap and EMap could be replaced by
   arrays for better efficiency, or using vertex and edge labels.
   They are used to record potentials, excesses and flows.
   VSet is used to track vertices that are still in excess. Because
   we sort those vertices, using search trees would not be a problem,
   but an array of size [G.nb_vertex] could degrade the performance.
   The default is a hash table densely filled, hence [sort_by_potential]
   is the asymptotical bottleneck.

   Parameter:
   - [param_freq_check_preflow]: used to parametrized how often one should check
   whether the current preflow is maximum.

  *)

  type vertex = G.V.t
  type arc = G.E.t
  type flow = F.t

  let (|>) x f = f x

  open G

  module Q = PersistentQueue
  module VH = Hashtbl.Make(G.V)
  module EM = Map.Make(G.E)

  module VMap = struct
    type 'a t = 'a VH.t
    let create = VH.create
    let add tbl key value = VH.add tbl key value
    let remove tbl key = VH.remove tbl key
    let find tbl key =
      try Some (VH.find tbl key)
      with Not_found -> None
  end

  module VSet = struct
    type t = unit VH.t
    let create () = VH.create 16
    let add tbl v =
      if not (VH.mem tbl v) then VH.add tbl v ()
    let elements tbl = VH.fold (fun v () list -> v::list) tbl []
  end

  module EMap = struct
    type 'a t = 'a EM.t ref
    let create _ = ref EM.empty
    let add map edge value =
      map := EM.add edge value !map
    let find map edge =
      try Some (EM.find edge !map)
      with Not_found -> None
  end

  let min_flow a b = if F.compare a b < 0 then a else b
  let (+-) = F.add
  let (--) = F.sub
  let is_positive a = F.compare a F.zero > 0
  let max_capacity e = F.max_capacity (E.label e)

  type residual_arc =
  | Forward of arc
  | Backward of arc

  (* context for computations *)
  type context =
    {
      nb_vertices : int;
      source : vertex;
      sink : vertex;
      reversed : bool;
      incident : context -> vertex -> residual_arc list;
      reverse_incident : context -> vertex -> residual_arc list;
      max_capacity : arc -> F.t;
      excess : flow VMap.t;
      potential : int VMap.t;
      mutable excessives : VSet.t;
      flow : flow EMap.t
    }

  let get_excess ctxt vertex =
    match VMap.find ctxt.excess vertex with
    | Some value -> value
    | None -> F.zero
  let get_potential ctxt vertex =
    match VMap.find ctxt.potential vertex with
    | Some value -> value
    | None -> 2 * ctxt.nb_vertices (* sink is not reachable from vertex *)
  let set_excess ctxt vertex value =
    VMap.remove ctxt.excess vertex;
    VMap.add ctxt.excess vertex value
  let set_potential ctxt vertex pi =
    VMap.remove ctxt.potential vertex;
    VMap.add ctxt.potential vertex pi
  let mark_excessive ctxt vertex =
    VSet.add ctxt.excessives vertex
  let extract_excessives ctxt =
    let in_excess = VSet.elements ctxt.excessives in
    ctxt.excessives <- VSet.create ();
    in_excess
  let get_flow context arc =
    match EMap.find context.flow arc with
    | Some value -> value
    | None -> F.zero
  let set_flow context arc value =
    EMap.add context.flow arc value
  let get_capacity context = function
    | Backward arc
    | Forward arc -> context.max_capacity arc

  (* residual graph helpers *)

  let origin : residual_arc -> vertex = function
    | Forward arc -> E.src arc
    | Backward arc -> E.dst arc
  let destination : residual_arc -> vertex = function
    | Forward arc -> E.dst arc
    | Backward arc -> E.src arc


  let forward arc = Forward arc
  let backward arc = Backward arc

  let residual_capacity : context -> residual_arc -> flow =
    fun context residual_arc -> match context.reversed, residual_arc with
    | true, Forward arc
    | false, Backward arc -> get_flow context arc
    | _, Backward arc
    | _, Forward arc -> F.sub (context.max_capacity arc) (get_flow context arc)

  let is_forward context arc =
    is_positive (context.max_capacity arc -- get_flow context arc)
  let is_backward context arc =
    is_positive (get_flow context arc)


  let augment : context -> residual_arc -> F.t -> unit =
    fun context residual_arc delta -> match context.reversed, residual_arc with
    | true, Backward arc
    | false, Forward arc ->
      get_flow context arc +- delta |> set_flow context arc
    | _, Backward arc
    | _, Forward arc ->
      get_flow context arc -- delta |> set_flow context arc

  let cons e l = e::l

  (* incidence function in the residual graph
     and in the reversal of the residual of the reversed graph *)
  let incidence_residual graph context vertex =
    begin
      fold_succ_e cons graph vertex []
      |> List.filter (is_forward context)
      |> List.map forward
    end @ begin
      fold_pred_e cons graph vertex []
      |> List.filter (is_backward context)
      |> List.map backward
    end

  (* incidence function in the reversal of the residual graph
     and in the residual of the reversed graph *)
  let incidence_reversal graph context vertex =
    begin
      fold_succ_e cons graph vertex []
      |> List.filter (is_backward context)
      |> List.map forward
    end @ begin
      fold_pred_e cons graph vertex []
      |> List.filter (is_forward context)
      |> List.map backward
    end


  (* Breadth-first search algorithm, with application of
   * a function on each arc of the BFS tree. *)
  let generic_bfs :
      int ->
      (vertex -> residual_arc list) ->
      (residual_arc -> unit) ->
      vertex -> unit
    =
    fun nb_vertices incidence iter_fun source ->
      let reached = VMap.create nb_vertices in
      let frontier = ref Q.empty in
      let add_arc arc =
	let dest = destination arc in
	if VMap.find reached dest = None then
	  ( VMap.add reached dest ();
	    iter_fun arc;
	    frontier := Q.add !frontier dest
	  )
      in
      let explore vertex = List.iter add_arc (incidence vertex) in
      VMap.add reached source ();
      explore source;
      while not (Q.is_empty !frontier) do
	explore (Q.head !frontier);
	frontier := Q.tail !frontier
      done

  (* labels the vertices by their distance to the sink.
     This is used to initial the potential of vertices,
     and drastically improve the performance of the algorithm. *)
  let initialize_potential context sink =
    let update arc =
      get_potential context (origin arc) + 1
      |> set_potential context (destination arc)
    in
    set_potential context sink 0;
    generic_bfs
      context.nb_vertices
      (context.reverse_incident context)
      update
      sink

  (* checks whether a preflow is maximum.
     Happens if no excessive vertex is reverse-reachable from the sink.
  *)
  exception Break
  let is_maximum_preflow context =
    let check_arc arc =
      if F.compare (get_excess context (destination arc)) F.zero <> 0 then
	raise Break
    in
    try
      generic_bfs
	context.nb_vertices
	(context.reverse_incident context)
	check_arc
	context.sink;
      true
    with Break -> false

  (* Push-relabel operations *)

  (* push excessive flow along an residual arc *)
  let push context arc =
    let (u,v) = (origin arc, destination arc) in
    let exc_u = get_excess context u in
    if is_positive exc_u then
      begin
	let delta = min_flow exc_u (residual_capacity context arc) in
	exc_u -- delta |> set_excess context u;
	get_excess context v +- delta |> set_excess context v;
	augment context arc delta;
	mark_excessive context v
    end

  (* Augment potential of a vertex to get a lower-potential successor *)
  let relabel context vertex =
    context.incident context vertex
    |> List.map (fun arc -> get_potential context (destination arc))
    |> List.fold_left min (get_potential context vertex)
    |> fun pi -> set_potential context vertex (pi+1)

  (* push can be done only on arc with difference of potential = -1 *)
  let is_admissible context arc =
    let (u,v) = (origin arc, destination arc) in
    get_potential context v - get_potential context u = -1

  (* push as much flow from a vertex as allowed. *)
  let discharge context vertex =
    context.incident context vertex
    |> List.filter (is_admissible context)
    |> List.iter (push context)
    |> fun () ->
      if is_positive (get_excess context vertex) then
	begin
	  relabel context vertex;
	  mark_excessive context vertex
	end

  (* Optimization: push vertices ordered by their potential.
     (better strategies may be possible). *)
  let compare_potential context u v =
    get_potential context v - get_potential context u
  let sort_by_potential context = List.sort (compare_potential context)

  let is_dischargeable context v =
        v <> context.source
    && v <> context.sink
    && is_positive (get_excess context v)

  let augmenting_step context currently_in_excess =
    context.excessives <- VSet.create ();
    currently_in_excess
    |> List.filter (is_dischargeable context)
    |> sort_by_potential context
    |> List.iter (discharge context)

  let param_freq_check_preflow = ref 1000

  let compute_max_preflow context =
    let nb_steps = ref 0 in
    let in_excess = ref (extract_excessives context) in
    let check_freq = context.nb_vertices / !param_freq_check_preflow + 1 in
    let is_maximum () =
      ( !in_excess = [] )
      || ( !nb_steps mod check_freq = 0 && is_maximum_preflow context )
    in
    while not (is_maximum ()) do
      augmenting_step context !in_excess;
      in_excess := extract_excessives context;
      incr nb_steps
    done

  (* Maximally push each arc leaving the source,
     set the potential of any vertex at distance to sink (optimization). *)
  let init_context context =
    let out_source = context.incident context context.source in
    initialize_potential context context.sink;
    set_potential context context.source context.nb_vertices;
    out_source
    |> List.map (get_capacity context)
    |> List.fold_left F.add F.zero
    |> set_excess context context.source;
    out_source
    |> List.iter (push context);
   context

  let new_context graph ~source ~sink ~reversed ~max_capacity ~flow =
    let nb_vertices = G.nb_vertex graph in
    init_context
      { nb_vertices; source; sink; reversed; max_capacity; flow;
	incident =
	  if reversed then incidence_reversal graph
	  else incidence_residual graph;
	reverse_incident =
	  if reversed then incidence_residual graph
	  else incidence_reversal graph;
	excess = VMap.create nb_vertices;
	potential = VMap.create nb_vertices;
	excessives = VSet.create ();
      }

  let maxflow graph source sink =
    let init_flow () =
      let flow = EMap.create (G.nb_edges graph) in
      G.fold_edges_e (fun e () -> EMap.add flow e F.zero) graph ();
      flow
    in
    let forward_context =
      new_context graph ~source ~sink
	~reversed:false
	~max_capacity
	~flow:(init_flow ())
    in
    compute_max_preflow forward_context;
    let backward_context =
      new_context graph
	~source:sink
	~sink:source
	~reversed:true
	~max_capacity:(get_flow forward_context)
	~flow:(init_flow ())
    in
    compute_max_preflow backward_context;
    let max_flow_value =
      fold_succ_e cons graph source []
      |> List.map (get_flow backward_context)
      |> List.fold_left F.add F.zero
    in
    let f e =
      match EMap.find backward_context.flow e with
      | Some x -> x | None -> F.zero in
    f, max_flow_value

end

(*****************************************************************************)

module type G_FORD_FULKERSON = sig
  type t
  module V : Sig.HASHABLE
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
    val label : t -> label
  end
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
end

module type FLOWMIN = sig
  include FLOW
  val min_capacity : label -> t
end

module Ford_Fulkerson
    (G: G_FORD_FULKERSON)
    (F: FLOWMIN with type label = G.E.label) =
struct

  (* redefinition of F *)
  module F = struct
    include F

    type u =
      | Flow of F.t
      | Infinity

    let min x y = match x, y with
      | Flow _, Infinity -> x
      | Flow fx, Flow fy when F.compare fx fy < 0 -> x
      | (Infinity, _) | (Flow _, Flow _) -> y
  end

  module Mark = struct
    module H = Hashtbl.Make(G.V)
    type mark = Plus | Minus

    let marked = H.create 97
    let unvisited = Queue.create ()

    let clear () = H.clear marked

    let mem = H.mem marked

    let set s e tag =
      assert (not (mem s));
      H.add marked s (e, tag);
      Queue.add s unvisited

    let get s : G.E.t * mark =
      let e, tag = H.find marked s in
      (match e with None -> assert false | Some e -> e), tag

    let next () = Queue.pop unvisited
  end

  module Result = struct
    module H =
      Hashtbl.Make
        (struct
          open G
          type t = E.t
          module U = Util.HTProduct(V)(V)
          let equal e1 e2 = U.equal (E.src e1, E.dst e1) (E.src e2, E.dst e2)
          let hash e = U.hash (E.src e, E.dst e)
        end)

    let create () = H.create 97

    let find = H.find

    let flow r e =
      try
        find r e
      with Not_found ->
        let f = F.flow (G.E.label e) in
        H.add r e f;
        f

    let change op r e f =
      try
        H.replace r e (op (find r e) f);
      with Not_found ->
        assert false

    let grow = change F.add
    let reduce = change F.sub
  end

  let is_full r e =
    F.compare (F.max_capacity (G.E.label e)) (Result.flow r e) = 0

  let is_empty r e =
    F.compare (F.min_capacity (G.E.label e)) (Result.flow r e) = 0

  let set_flow r s t a =
    let rec loop t =
      if not (G.V.equal s t) then
        let e, tag = Mark.get t in
        match tag with
        | Mark.Plus -> Result.grow r e a; loop (G.E.src e)
        | Mark.Minus -> Result.reduce r e a; loop (G.E.dst e)
    in
    loop t

  let grow_flow r s t a =
    let rec loop u b =
      if G.V.equal s u then begin
        match b with
        | F.Infinity -> (* source = destination *)
          assert (G.V.equal s t);
          a
        | F.Flow f ->
          set_flow r s t f;
          F.add a f
      end else
        let e, tag = Mark.get u in
        let l = G.E.label e in
        match tag with
        | Mark.Plus ->
          loop
            (G.E.src e)
            (F.min b (F.Flow (F.sub (F.max_capacity l) (Result.flow r e))))
        | Mark.Minus ->
          loop
            (G.E.dst e)
            (F.min b (F.Flow (F.sub (Result.flow r e) (F.min_capacity l))))
    in
    loop t F.Infinity

  let maxflow g s t =
    let r = Result.create () in
    let succ s =
      G.iter_succ_e
        (fun e ->
           assert (G.V.equal s (G.E.src e));
           let t = G.E.dst e in
           if not (Mark.mem t || is_full r e) then
             Mark.set t (Some e) Mark.Plus)
        g s
    in
    let pred s =
      G.iter_pred_e
        (fun e ->
           assert (G.V.equal s (G.E.dst e));
           let t = G.E.src e in
           if not (Mark.mem t || is_empty r e) then
             Mark.set t (Some e) Mark.Minus)
        g s
    in
    let internal_loop a =
      try
        while true do let s = Mark.next () in succ s; pred s done;
        assert false
      with Queue.Empty ->
        if Mark.mem t then grow_flow r s t a else a
    in
    let rec external_loop a =
      Mark.clear ();
      Mark.set s None Mark.Plus;
      let a' = internal_loop a in
      if a = a' then a else external_loop a'
    in
    let a = external_loop F.zero in
    (fun e -> try Result.find r e with Not_found -> F.flow (G.E.label e)), a

end
