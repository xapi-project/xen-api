open! Import
open Std_internal

include Univ_map_intf

module Uid = Type_equal.Id.Uid

module Make1 (Data : sig type ('s, 'a) t [@@deriving sexp_of] end) = struct
  type ('s, 'a) data = ('s, 'a) Data.t

  module Packed = struct
    type 's t = T : 'a Key.t * ('s, 'a) Data.t -> 's t

    let sexp_of_t sexp_of_a (T (key,data)) =
      Data.sexp_of_t sexp_of_a (Key.to_sexp key) data

    let type_id_name (T (key, _)) = Key.name key

    let type_id_uid  (T (key, _)) = Key.uid  key
  end

  type 's t = 's Packed.t Uid.Map.t

  let sexp_of_t sexp_of_a t =
    Map.data t
    |> List.map ~f:(fun u -> (Packed.type_id_name u, u))
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> [%sexp_of: (string * a Packed.t) list]

  let invariant (t : _ t) =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      Map.iteri t ~f:(fun ~key ~data ->
        assert (Uid.equal key (Packed.type_id_uid data))))

  let set t key data = Map.set t ~key:(Key.uid key) ~data:(Packed.T (key, data))

  let mem_by_id t id = Map.mem t id

  let mem t key = mem_by_id t (Key.uid key)

  let remove_by_id t id = Map.remove t id

  let remove t key = remove_by_id t (Key.uid key)

  let empty = Uid.Map.empty

  let is_empty = Map.is_empty

  let find (type b) t (key : b Key.t) =
    match Map.find t (Key.uid key) with
    | None -> None
    | Some (Packed.T (key', value)) ->
      (* cannot raise -- see [invariant] *)
      let Type_equal.T = Key.same_witness_exn key key' in
      Some (value : (_, b) Data.t)

  let find_exn t key =
    match find t key with
    | Some data -> data
    | None -> failwithf "Univ_map.find_exn on unknown key %s" (Key.name key) ()

  let add t key data = if mem t key then `Duplicate else `Ok (set t key data)

  let add_exn t key data =
    match add t key data with
    | `Ok t -> t
    | `Duplicate -> failwithf "Univ_map.add_exn on existing key %s" (Key.name key) ()

  let change_exn t key ~f:update =
    match find t key with
    | Some data -> set t key (update data)
    | None -> failwithf "Univ_map.change_exn on unknown key %s" (Key.name key) ()

  let change t key ~f:update =
    let orig = find t key in
    let next = update orig in
    match next with
    | Some data -> set t key data
    | None -> if Option.is_none orig then t else remove t key

  let update t key ~f = change t key ~f:(fun data -> Some (f data))

  let to_alist t = Map.data t
end

module Make (Data : sig type 'a t [@@deriving sexp_of] end) = struct
  module M = Make1 (struct
      type (_, 'a) t = 'a Data.t [@@deriving sexp_of]
    end)

  type t = unit M.t [@@deriving sexp_of]

  type 'a data = 'a Data.t

  let invariant  = M.invariant
  let empty      = M.empty
  let is_empty   = M.is_empty
  let set        = M.set
  let mem        = M.mem
  let mem_by_id  = M.mem_by_id
  let find       = M.find
  let find_exn   = M.find_exn
  let add        = M.add
  let add_exn    = M.add_exn
  let change     = M.change
  let change_exn = M.change_exn
  let update     = M.update
  let remove     = M.remove
  let remove_by_id = M.remove_by_id

  module Packed = struct
    type t = T : 'a Key.t * 'a Data.t -> t
  end

  let to_alist t =
    List.map (M.to_alist t) ~f:(function M.Packed.T (key, data) -> Packed.T (key, data))
end

include Make (struct type 'a t = 'a [@@deriving sexp_of] end)

let%test_module _ =
  (module struct

    let size = Key.create ~name:"size" Int.sexp_of_t
    let name = Key.create ~name:"name" String.sexp_of_t
    let foo  = Key.create ~name:"foo"  Float.sexp_of_t
    let kids = Key.create ~name:"kids" (List.sexp_of_t sexp_of_t)

    let%test _ = is_empty empty

    let test_contains t k v =
      assert (not (is_empty t));
      assert (mem t k);
      begin (* these do not raise *)
        ignore (change_exn t k ~f:Fn.id);
        ignore (change t k ~f:(function None -> assert false | o -> o));
      end;
      match find t k with
      | None -> assert false
      | Some v' -> assert (phys_equal v v')

    let test_add t k v = test_contains (set t k v) k v

    let test_find t k =
      let f1 = find t k in
      let f2 = Option.try_with (fun () -> find_exn t k) in
      match (f1, f2) with
      | (None,    None)    -> ()
      | (Some v1, Some v2) -> assert (phys_equal v1 v2)
      | (Some _,  None)    -> assert false
      | (None,    Some _)  -> assert false

    let test_change t k v =
      let t_minus = change t k ~f:(fun _ -> None) in
      assert (not (mem t_minus k));
      let t_plus = change t k ~f:(fun _ -> Some v) in
      test_contains t_plus k v;
      ()

    let test_remove t k v =
      let t_minus = remove t k in
      assert (not (mem t_minus k));
      let t_plus = set t k v in
      test_contains t_plus k v;
      let t_minus = remove t_plus k in
      assert (not (mem t_minus k))

    let test_remove_by_id t k v =
      let t_minus = remove_by_id t (Key.uid k) in
      assert (not (mem t_minus k));
      let t_plus = set t k v in
      test_contains t_plus k v;
      let t_minus = remove_by_id t_plus (Key.uid k) in
      assert (not (mem t_minus k))

    let test t =
      (* add *)
      test_add t size 12;
      test_add t name "hank";
      test_add t kids [t; empty];
      (* find *)
      test_find t size;
      test_find t name;
      test_find t kids;
      (* change *)
      test_change t size 33;
      test_change t name "frank";
      test_change t kids [];
      (* remove *)
      test_remove t size 33;
      test_remove t name "frank";
      test_remove t kids [];
      (* remove_by_id *)
      test_remove_by_id t size 33;
      test_remove_by_id t name "frank";
      test_remove_by_id t kids [];
      ()

    let t0 = empty
    let t1 = set t0 size 9
    let t2 = set t1 foo 13.25
    let t3 = set t2 size 15

    let%test_unit _ = test t0
    let%test_unit _ = test t1
    let%test_unit _ = test t2
    let%test_unit _ = test t3

    let%test _ = sexp_of_t t3 = Sexp.of_string "((foo 13.25)(size 15))"

  end)

module With_default = struct

  module Key = struct
    type 'a t = { key : 'a Key.t; default : 'a; }
    let create ~default ~name sexp_of = { default; key = Key.create ~name sexp_of }
    let id t = t.key
  end

  let find t {Key.key; default} = Option.value ~default (find t key)

  let set t {Key.key; default=_ } v = set t key v

  let change t k ~f:update = set t k (update (find t k))

  let%test_unit _ =
    let key = Key.create ~default:0 ~name:"default 0" Int.sexp_of_t in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = set empty key 2 in
    assert (find t key = 2);
    let t = change t key ~f:(~-) in
    assert (find t key = -2)

  let%test _ =
    let key = Key.create ~default:1 ~name:"default 1" Int.sexp_of_t in
    find (change empty key ~f:(~-)) key = -1
end

module With_fold = struct

  module Key = struct
    type ('a, 'b) t = { key : 'b With_default.Key.t; f : 'b -> 'a -> 'b; }
    let create ~init ~f ~name sexp_of =
      {f; key = With_default.Key.create ~default:init ~name sexp_of}
    let id t = With_default.Key.id t.key
  end

  let find t {Key.key; f=_ } = With_default.find t key

  let set t {Key.key; f=_ } v = With_default.set t key v

  let change t {Key.key; f=_ } ~f:update = With_default.change t key ~f:update

  let add t {Key.key; f} v = With_default.change t key ~f:(fun acc -> f acc v)

  let%test_unit _ =
    let key = Key.create ~init:5 ~f:(+) ~name:"init 5" Int.sexp_of_t in
    assert (find empty key = 5);
    let t = add empty key 3 in
    assert (find t key = 8);
    let t = change t key ~f:(~-) in
    assert (find t key = -8)

  let%test_unit _ =
    let key =
      Key.create ~init:0 ~f:(fun _ -> assert false) ~name:"don't fold this" Int.sexp_of_t
    in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = change t key ~f:(~-) in
    assert (find t key = -1)
end

module Multi = struct
  open With_fold
  module Key = struct
    type 'a t = ('a, 'a list) Key.t
    let create ~name sexp_of =
      Key.create ~init:[] ~f:(fun xs x -> x :: xs) ~name (List.sexp_of_t sexp_of)
    let id = With_fold.Key.id
  end
  let set = set
  let find = find
  let add = add
  let change = change

  let%test_unit _ =
    let key = Key.create ~name:"int list" Int.sexp_of_t in
    assert (find empty key = []);
    let t = add empty key 1 in
    assert (find t key = [1]);
    let t = set t key [2;3] in
    assert (find t key = [2;3]);
    let t = change t key ~f:(List.map ~f:(~-)) in
    assert (find t key = [-2;-3])
end
