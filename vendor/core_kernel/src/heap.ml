open! Import
open Std_internal

module Pointer = Pool.Pointer

(* This pool holds nodes that would be represented more traditionally as:

   {[
     type 'a t =
       | Empty
       | Heap of 'a * 'a t list ]}

   We will represent them as a left-child, right-sibling tree in a triplet
   (value * left_child * right_sibling).  The left child and all right siblings
   of the left child form a linked list representing the subheaps of a given heap:

   {v
         A
        /
       B -> C -> D -> E -> F
      /         /         /
     G         H->I->J   K->L
   v} *)

module Node : sig
  (* Exposing [private int] is a significant performance improvement, because it allows
     the compiler to skip the write barrier. *)
  type 'a t = private int

  module Id : sig
    type t
    val of_int : int -> t
    val equal : t -> t -> bool
  end

  module Pool : sig
    type 'a node = 'a t
    type 'a t

    val create  : min_size:int -> 'a t
    val is_full : 'a t -> bool
    val length  : 'a t -> int
    val grow    : 'a t -> 'a t
    val copy    : 'a t -> 'a node -> ('a node * 'a t)
  end

  (** [allocate v ~pool] allocates a new node from the pool with no child or sibling *)
  val allocate : 'a -> pool:'a Pool.t -> id:Id.t -> 'a t

  (** [free t ~pool] frees [t] for reuse.  It is an error to access [t] after this. *)
  val free : 'a t -> pool:'a Pool.t -> unit

  (** a special [t] that represents the empty node *)
  val empty    : unit -> 'a t
  val is_empty : 'a t -> bool

  val equal : 'a t -> 'a t -> bool

  (** [value_exn t ~pool] return the value of [t], raise if [is_empty t] *)
  val value_exn : 'a t -> pool:'a Pool.t -> 'a

  val id : 'a t -> pool:'a Pool.t -> Id.t

  val child   : 'a t -> pool:'a Pool.t -> 'a t
  val sibling : 'a t -> pool:'a Pool.t -> 'a t
  val prev    : 'a t -> pool:'a Pool.t -> 'a t
  (** [prev t] is either the parent of [t] or the sibling immediately left of [t] *)

  (** [add_child t ~child ~pool] Add a child to [t], preserving existing children as
      siblings of [child]. [t] and [child] should not be empty and [child] should have no
      sibling and have no prev node. *)
  val add_child : 'a t -> child:'a t -> pool:'a Pool.t -> unit

  (** disconnect and return the sibling *)
  val disconnect_sibling : 'a t -> pool:'a Pool.t -> 'a t

  (** disconnect and return the child *)
  val disconnect_child   : 'a t -> pool:'a Pool.t -> 'a t

  (** [detach t ~pool] removes [t] from the tree, adjusting pointers around it. After
      [detach], [t] is the root of a standalone heap, which is detached from the original
      heap. *)
  val detach : 'a t -> pool:'a Pool.t -> unit
end = struct

  module Id = Int
  let dummy_id : Id.t = -1

  type 'a node =
    ( 'a
    , 'a node Pointer.t
    , 'a node Pointer.t
    , 'a node Pointer.t
    , Id.t
    ) Pool.Slots.t5

  type 'a t = 'a node Pointer.t

  let empty    = Pointer.null
  let is_empty = Pointer.is_null

  let equal = Pointer.phys_equal

  let value   t ~pool = Pool.get pool t Pool.Slot.t0
  let child   t ~pool = Pool.get pool t Pool.Slot.t1
  let sibling t ~pool = Pool.get pool t Pool.Slot.t2
  let prev    t ~pool = Pool.get pool t Pool.Slot.t3
  let id      t ~pool = Pool.get pool t Pool.Slot.t4

  (* let set_value   t v ~pool = Pool.set pool t Pool.Slot.t0 v *)
  let set_child   t v ~pool = Pool.set pool t Pool.Slot.t1 v
  let set_sibling t v ~pool = Pool.set pool t Pool.Slot.t2 v
  let set_prev    t v ~pool = Pool.set pool t Pool.Slot.t3 v

  let value_exn t ~pool =
    assert (not (is_empty t));
    value t ~pool
  ;;

  let allocate value ~pool ~id =
    Pool.new5 pool value (empty ()) (empty ()) (empty ()) id
  ;;

  let free t ~pool = Pool.unsafe_free pool t

  let disconnect_sibling t ~pool =
    let sibling = sibling t ~pool in
    if not (is_empty sibling) then begin
      set_sibling t (empty ()) ~pool;
      set_prev sibling (empty ()) ~pool;
    end;
    sibling
  ;;

  let disconnect_child t ~pool =
    let child = child t ~pool in
    if not (is_empty child) then begin
      set_child t (empty ()) ~pool;
      set_prev child (empty ()) ~pool;
    end;
    child
  ;;

  let add_child t ~child:new_child ~pool =
    (* assertions we would make, but for speed:
       assert (not (is_empty t));
       assert (not (is_empty new_child));
       assert (is_empty (sibling new_child ~pool));
       assert (is_empty (prev new_child ~pool));
    *)
    let current_child = disconnect_child t ~pool in
    (* add [new_child] to the list of [t]'s children (which may be empty) *)
    set_sibling new_child current_child ~pool;
    if not (is_empty current_child) then set_prev current_child new_child ~pool;

    set_child t new_child ~pool;
    set_prev new_child t ~pool;
  ;;

  let detach t ~pool =
    if not (is_empty t) then begin
      let prev = prev t ~pool in
      if not (is_empty prev) then begin
        let relation_to_prev = if equal t (child prev ~pool) then `child else `sibling in
        set_prev t (empty ()) ~pool;
        let sibling = disconnect_sibling t ~pool in
        begin match relation_to_prev with
        | `child   -> set_child   prev sibling ~pool
        | `sibling -> set_sibling prev sibling ~pool
        end;
        if not (is_empty sibling) then set_prev sibling prev ~pool;
      end;
    end
  ;;

  module Pool = struct
    type 'a t = 'a node Pool.t
    type nonrec 'a node = 'a node Pointer.t

    let create (type a) ~min_size:capacity : a t =
      Pool.create Pool.Slots.t5 ~capacity
        ~dummy:((Obj.magic None : a), Pointer.null (), Pointer.null (), Pointer.null (), dummy_id)
    ;;

    let is_full t = Pool.is_full t
    let length  t = Pool.length t
    let grow    t = Pool.grow t

    let copy t start =
      let t' = create ~min_size:(Pool.capacity t) in
      let copy_node node to_visit =
        if is_empty node
        then (empty (), to_visit)
        else begin
          (* we use the same id, but that's ok since ids should be unique per heap *)
          let new_node = allocate (value_exn node ~pool:t) ~pool:t' ~id:(id node ~pool:t) in
          let to_visit =
            (new_node, `child,   child   node ~pool:t) ::
            (new_node, `sibling, sibling node ~pool:t) ::
            to_visit
          in
          (new_node, to_visit)
        end
      in
      let rec loop to_visit =
        match to_visit with
        | [] -> ()
        | (node_to_update, slot, node_to_copy) :: rest ->
          let new_node, to_visit = copy_node node_to_copy rest in
          begin match slot with
          | `child -> set_child node_to_update new_node ~pool:t';
          | `sibling -> set_sibling node_to_update new_node ~pool:t';
          end;
          if not (is_empty new_node) then (set_prev new_node node_to_update ~pool:t');
          loop to_visit
      in
      let new_start, to_visit = copy_node start [] in
      loop to_visit;
      (new_start, t')
    ;;
  end
end

type 'a t = {
  (* cmp is placed first to short-circuit polymorphic compare *)
  cmp          : 'a -> 'a -> int;
  mutable pool : 'a Node.Pool.t;
  (* invariant:  [root] never has a sibling *)
  mutable root : 'a Node.t;
  mutable num_of_allocated_nodes : int;
}

let invariant t =
  let rec loop to_visit =
    match to_visit with
    | [] -> ()
    | (node, expected_prev, maybe_parent_value) :: rest ->
      if not (Node.is_empty node) then begin
        let this_value = Node.value_exn node ~pool:t.pool in
        assert (Node.equal (Node.prev node ~pool:t.pool) expected_prev);
        Option.iter maybe_parent_value ~f:(fun parent_value ->
          assert (t.cmp parent_value this_value <= 0)
        );
        loop ((Node.child   node ~pool:t.pool, node, Some this_value) ::
              (Node.sibling node ~pool:t.pool, node, maybe_parent_value) ::
              rest)
      end
      else loop rest
  in
  assert (Node.is_empty t.root || Node.is_empty (Node.sibling t.root ~pool:t.pool));
  loop [(t.root, Node.empty (), None)]
;;

let create ?(min_size = 1) ~cmp () =
  { cmp
  ; pool = Node.Pool.create ~min_size
  ; root = Node.empty ()
  ; num_of_allocated_nodes = 0
  }
;;

let copy { cmp; pool; root; num_of_allocated_nodes } =
  let root, pool = Node.Pool.copy pool root in
  { cmp
  ; pool
  ; root
  ; num_of_allocated_nodes
  }
;;

let allocate t v =
  if Node.Pool.is_full t.pool then begin
    t.pool <- Node.Pool.grow t.pool;
  end;
  t.num_of_allocated_nodes <- t.num_of_allocated_nodes + 1;
  Node.allocate v ~pool:t.pool ~id:(Node.Id.of_int t.num_of_allocated_nodes)
;;

(* translation:
   {[
     match root1, root2 with
     | None, h | h, None -> h
     | Some (Node (v1, children1)), Some (Node (v2, children2)) ->
       if v1 < v2
       then Some (Node (v1, root2 :: children1))
       else Some (Node (v2, root1 :: children2))
   ]}

   This function assumes neither root has a prev node (usually because the inputs come
   from [disconnect_*] or are the top of the heap or are the output of this function). *)
let merge t root1 root2 =
  if Node.is_empty root1 then
    root2
  else if Node.is_empty root2 then
    root1
  else
    let add_child t node ~child =
      Node.add_child node ~pool:t.pool ~child;
      node
    in
    let v1 = Node.value_exn root1 ~pool:t.pool in
    let v2 = Node.value_exn root2 ~pool:t.pool in
    if t.cmp v1 v2 < 0
    then add_child t root1 ~child:root2
    else add_child t root2 ~child:root1
;;

let top_exn t =
  if Node.is_empty t.root
  then failwith "Heap.top_exn called on an empty heap"
  else Node.value_exn t.root ~pool:t.pool
;;

let top t = if Node.is_empty t.root then None else Some (top_exn t)

let add_node t v =
  let node = allocate t v in
  t.root <- merge t t.root node;
  node
;;

let add t v = ignore (add_node t v : _ Node.t)

(* [merge_pairs] takes a list of heap roots and merges consecutive pairs, reducing the
   list of length n to n/2.  Then it merges the merged pairs into a single heap.  One
   intuition is that this is somewhat like building a single level of a binary tree.

   The output heap does not contain the value that was at the root of the input heap.

   We break the function into two parts.  A first stage that is willing to use limited
   stack instead of heap allocation for bookkeeping, and a second stage that shifts to
   using a list as an accumulator if we go too deep.

   This can be made tail recursive and non-allocating by starting with an empty heap and
   merging merged pairs into it. Unfortunately this "left fold" version is not what is
   described in the original paper by Fredman et al.; they specifically say that
   children should be merged together from the end of the list to the beginning of the
   list. ([merge] is not associative, so order matters.)
*)
(* translation:
   {[
     let rec loop acc = function
       | [] -> acc
       | [head] -> head :: acc
       | head :: next1 :: next2 -> loop (merge head next1 :: acc) next2
     in
     match loop [] children with
     | [] -> None
     | [h] -> Some h
     | x :: xs -> Some (List.fold xs ~init:x ~f:merge)
   ]}
*)
let allocating_merge_pairs t head =
  let rec loop acc head =
    if Node.is_empty head then
      acc
    else
      let next1 = Node.disconnect_sibling head ~pool:t.pool in
      if Node.is_empty next1 then
        head :: acc
      else
        let next2 = Node.disconnect_sibling next1 ~pool:t.pool in
        loop (merge t head next1 :: acc) next2
  in
  match loop [] head with
  | []      -> Node.empty ()
  | [h]     -> h
  | x :: xs -> List.fold xs ~init:x ~f:(fun acc heap -> merge t acc heap)
;;

(* translation:
   {[
     match t.root with
     | Node (_, children) ->
       let rec loop depth children =
         if depth >= max_stack_depth
         then allocating_merge_pairs t childen
         else begin
           match children with
           | [] -> None
           | [head] -> Some head
           | head :: next1 :: next2 ->
             merge (merge head next1) (loop (depth + 1) next2)
         end
       in
       loop 0 children
   ]}
*)
let merge_pairs =
  let max_stack_depth = 1_000 in
  let rec loop t depth head =
    if depth >= max_stack_depth
    then allocating_merge_pairs t head
    else begin
      if Node.is_empty head
      then head
      else begin
        let next1 = Node.disconnect_sibling head ~pool:t.pool in
        if Node.is_empty next1
        then head
        else begin
          let next2 = Node.disconnect_sibling next1 ~pool:t.pool in
          (* merge the first two nodes in our list, and then merge the result with the
             result of recursively calling merge_pairs on the tail *)
          merge t
            (merge t head next1)
            (loop t (depth + 1) next2);
        end
      end
    end
  in
  fun t head -> loop t 0 head
;;

let remove_non_empty t node =
  let pool = t.pool in
  Node.detach node ~pool;
  let merged_children = merge_pairs t (Node.disconnect_child node ~pool) in
  let new_root =
    if Node.equal t.root node then
      merged_children
    else
      merge t t.root merged_children
  in
  Node.free node ~pool;
  t.root <- new_root;
;;

let remove_top t = if not (Node.is_empty t.root) then remove_non_empty t t.root

let pop_exn t =
  let r = top_exn t in
  remove_top t;
  r
;;

let pop t = if Node.is_empty t.root then None else Some (pop_exn t)

let pop_if t f =
  match top t with
  | None   -> None
  | Some v ->
    if f v
    then begin
      remove_top t;
      Some v
    end else
      None
;;

(* pairing heaps are not balanced trees, and therefore we can't rely on a balance
   property to stop ourselves from overflowing the stack. *)
let fold t ~init ~f =
  let pool = t.pool in
  let rec loop acc to_visit =
    match to_visit with
    | [] -> acc
    | node :: rest ->
      if Node.is_empty node then
        loop acc rest
      else begin
        let to_visit = (Node.sibling ~pool node) :: (Node.child ~pool node) :: rest in
        loop (f acc (Node.value_exn ~pool node)) to_visit
      end
  in
  loop init [t.root]
;;

(* almost identical to fold, copied for speed purposes *)
let iter t ~f =
  let pool = t.pool in
  let rec loop to_visit =
    match to_visit with
    | [] -> ()
    | node :: rest ->
      if Node.is_empty node then
        loop rest
      else begin
        f (Node.value_exn ~pool node);
        let to_visit = (Node.sibling ~pool node) :: (Node.child ~pool node) :: rest in
        loop to_visit
      end
  in
  loop [t.root]
;;

module C = Container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

(* we can do better than the O(n) of [C.length] *)
let length t = Node.Pool.length t.pool
let is_empty t = Node.is_empty t.root

let mem      = C.mem
let exists   = C.exists
let for_all  = C.for_all
let count    = C.count
let sum      = C.sum
let find     = C.find
let find_map = C.find_map
let to_list  = C.to_list
let to_array = C.to_array
let min_elt  = C.min_elt
let max_elt  = C.max_elt
let fold_result = C.fold_result
let fold_until  = C.fold_until

let of_array arr ~cmp =
  let t = create ~min_size:(Array.length arr) ~cmp () in
  Array.iter arr ~f:(fun v -> add t v);
  t
;;

let of_list l ~cmp = of_array (Array.of_list l) ~cmp

let sexp_of_t f t = Array.sexp_of_t f (to_array t |> Array.sorted_copy ~compare:t.cmp)

let%test_module _ =
  (module struct
    let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    let t = of_list data ~cmp:Int.compare
    let () = invariant t
    (* pop the zero at the top to force some heap structuring.  This does not touch the
       sum. *)
    let _ : int option = pop t
    let () = invariant t
    let list_sum      = List.fold data ~init:0 ~f:(fun sum v -> sum + v)
    let heap_fold_sum = fold t ~init:0 ~f:(fun sum v -> sum + v)
    let heap_iter_sum =
      let r = ref 0 in
      iter t ~f:(fun v -> r := !r + v);
      !r
    let%test _ = Int.(=) list_sum heap_fold_sum
    let%test _ = Int.(=) list_sum heap_iter_sum
  end)

module Elt = struct

  type nonrec 'a t =
    { mutable node : 'a Node.t
    ; node_id : Node.Id.t
    ; heap : 'a t
    }

  (* If ids are different, it means that the node has already been removed by some
     other means (and possibly reused). *)
  let is_node_valid t =
    Node.Id.equal (Node.id ~pool:t.heap.pool t.node) t.node_id

  let value t =
    if is_node_valid t then
      Some (Node.value_exn t.node ~pool:t.heap.pool)
    else
      None
  ;;

  let value_exn t =
    if is_node_valid t then
      Node.value_exn t.node ~pool:t.heap.pool
    else
      failwith "Heap.value_exn: node was removed from the heap"
  ;;

  let sexp_of_t sexp_of_a t = [%sexp (value t : a option) ]
end

let remove t (token : _ Elt.t) =
  if not (phys_equal t token.heap) then
    failwith "cannot remove from a different heap"
  else if not (Node.is_empty token.node)
  then begin
    if Elt.is_node_valid token then remove_non_empty t token.node;
    token.node <- Node.empty ();
  end
;;

let add_removable t v =
  let node = add_node t v in
  { Elt.node; heap = t; node_id = Node.id ~pool:t.pool node }
;;

let update t token v =
  remove t token;
  add_removable t v
;;

let find_elt =
  let rec loop t f nodes =
    match nodes with
    | [] -> None
    | node :: rest ->
      if Node.is_empty node then
        loop t f rest
      else if f (Node.value_exn node ~pool:t.pool) then
        Some { Elt.node; heap = t; node_id = Node.id ~pool:t.pool node }
      else
        loop t f
          ((Node.sibling node ~pool:t.pool) ::
           (Node.child   node ~pool:t.pool) ::
           rest)
  in
  fun t ~f -> loop t f [t.root]
;;

module Unsafe = struct
  module Elt = struct
    type 'a heap = 'a t
    type 'a t = 'a Node.t
    let value t heap = Node.value_exn ~pool:heap.pool t
  end

  let add_removable = add_node
  let remove = remove_non_empty

  let update t elt v =
    remove t elt;
    add_removable t v
  ;;
end


let%test_module _ =
  (module struct
    module type Heap_intf = sig
      type 'a t [@@deriving sexp_of]

      val create     : cmp:('a -> 'a -> int) -> 'a t
      val add        : 'a t -> 'a -> unit
      val pop        : 'a t -> 'a option
      val length     : 'a t -> int
      val top        : 'a t -> 'a option
      val remove_top : 'a t -> unit
      val to_list    : 'a t -> 'a list
      val invariant  : 'a t -> unit
    end

    module That_heap : Heap_intf = struct
      type 'a t = {
        cmp          : 'a -> 'a -> int;
        mutable heap : 'a list;
      }

      let sexp_of_t sexp_of_v t = List.sexp_of_t sexp_of_v t.heap

      let create ~cmp = { cmp; heap = [] }
      let add t v = t.heap <- List.sort ~compare:t.cmp (v :: t.heap)

      let pop t =
        match t.heap with
        | [] -> None
        | x :: xs ->
          t.heap <- xs;
          Some x
      ;;

      let length t     = List.length t.heap
      let top t        = List.hd t.heap
      let remove_top t = match t.heap with [] -> () | _ :: xs -> t.heap <- xs
      let to_list t    = t.heap
      let invariant    = Fn.ignore
    end

    module This_heap : Heap_intf = struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let create ~cmp = create ~cmp ()
      let add         = add
      let pop         = pop
      let length      = length
      let top         = top
      let remove_top  = remove_top
      let to_list     = to_list
      let invariant   = invariant
    end

    let this_to_string this = Sexp.to_string (This_heap.sexp_of_t Int.sexp_of_t this)
    let that_to_string that = Sexp.to_string (That_heap.sexp_of_t Int.sexp_of_t that)

    let length_check (t_a, t_b) =
      let this_len = This_heap.length t_a in
      let that_len = That_heap.length t_b in
      if this_len <> that_len then
        failwithf "error in length: %i (for %s) <> %i (for %s)"
          this_len (this_to_string t_a)
          that_len (that_to_string t_b) ()
    ;;

    let create () =
      let cmp = Int.compare in
      (This_heap.create ~cmp, That_heap.create ~cmp)
    ;;

    let add (this_t, that_t) v =
      This_heap.add this_t v;
      That_heap.add that_t v;
      length_check (this_t, that_t)
    ;;

    let pop (this_t, that_t) =
      let res1 = This_heap.pop this_t in
      let res2 = That_heap.pop that_t in
      if res1 <> res2 then
        failwithf "pop results differ (%s, %s)"
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res2)) ()
    ;;

    let top (this_t, that_t) =
      let res1 = This_heap.top this_t in
      let res2 = That_heap.top that_t in
      if res1 <> res2 then
        failwithf "top results differ (%s, %s)"
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res2)) ()
    ;;

    let remove_top (this_t, that_t) =
      This_heap.remove_top this_t;
      That_heap.remove_top that_t;
      length_check (this_t, that_t)
    ;;

    let internal_check (this_t, that_t) =
      let this_list = List.sort ~compare:Int.compare (This_heap.to_list this_t) in
      let that_list = List.sort ~compare:Int.compare (That_heap.to_list that_t) in
      assert (this_list = that_list);
      This_heap.invariant this_t;
      That_heap.invariant that_t;
    ;;

    let test_dual_ops () =
      let t = create () in

      let rec loop ops =
        if ops = 0 then ()
        else begin
          let r = Random.int 100 in
          begin
            if r < 40 then
              add t (Random.int 100_000)
            else if r < 70 then
              pop t
            else if r < 80 then
              top t
            else if r < 90 then
              remove_top t
            else
              internal_check t
          end;
          loop (ops - 1)
        end
      in
      loop 1_000
    ;;

    let%test_unit _ = test_dual_ops ()
  end)

let test_copy ~add_removable ~remove =
  let sum t = fold t ~init:0 ~f:(fun acc i -> acc + i) in
  let t = create ~cmp:Int.compare () in
  for i = 1 to 99 do
    add t i;
    if i % 10 = 0
    (* We need to pop from time to time to trigger the amortized tree reorganizations.  If
       we don't do this the resulting structure is just a linked list and the copy
       function is not flexed as completely as it should be. *)
    then begin
      ignore (pop t);
      add t i
    end
  done;
  let token = add_removable t 100 in
  invariant t;
  let t' = copy t in
  invariant t';
  assert (sum t = sum t');
  assert (to_list t = to_list t');
  remove t token;
  assert (sum t = sum t' - 100);
;;
let%test_unit _ = test_copy ~add_removable ~remove
let%test_unit _ = test_copy ~add_removable:Unsafe.add_removable ~remove:Unsafe.remove

let test_removal ~add_removable ~remove ~elt_value_exn =
  let t = create ~cmp:Int.compare () in
  let tokens = ref [] in
  for i = 1 to 10_000 do
    tokens := add_removable t i :: !tokens;
  done;
  invariant t;
  List.iter !tokens ~f:(fun token ->
    if elt_value_exn token t % 2 <> 0
    then remove t token);
  invariant t;
  let rec loop count =
    if count % 1000 = 0 then invariant t;
    match pop t with
    | None   -> assert (count = 10_000 / 2);
    | Some v ->
      assert ((1 + count) * 2 = v);
      loop (count + 1)
  in
  loop 0
;;
let%test_unit _ =
  test_removal ~add_removable ~remove ~elt_value_exn:(fun token _ -> Elt.value_exn token)
let%test_unit _ =
  test_removal ~add_removable:Unsafe.add_removable ~remove:Unsafe.remove
    ~elt_value_exn:Unsafe.Elt.value

let test_ordering () =
  let t = create ~cmp:Int.compare () in
  for _ = 1 to 10_000 do
    add t (Random.int 100_000);
  done;
  let rec loop last count =
    if (count % 1_000 = 0) then invariant t;
    match pop t with
    | None -> ()
    | Some v ->
      assert (v >= last);
      loop v (count + 1)
  in
  loop (-1) 0
;;
let%test_unit _ = test_ordering ()

let%test_unit _ = ignore (of_array [| |] ~cmp:Int.compare)

let%test_unit "operations on removed elements" =
  let h = create ~cmp:Int.compare () in
  let elt = add_removable h 1 in
  [%test_eq: string] (Sexp.to_string (Elt.sexp_of_t sexp_of_int elt)) "(1)";
  ignore (pop_exn h : int);
  assert (Result.is_error (Result.try_with (fun () -> Elt.value_exn elt)));
  [%test_eq: string] (Sexp.to_string (Elt.sexp_of_t sexp_of_int elt)) "()";
;;
