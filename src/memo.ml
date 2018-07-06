open! Import
open Std_internal

module Result = struct

  type 'a t = Rval of 'a | Expt of exn

  let return = function
    | Rval v -> v
    | Expt e -> raise e

  let capture f x =
    try Rval (f x) with
    | Sys.Break as e -> raise e
    | e -> Expt e

end

let unit f =
  let l = Lazy.from_fun f in
  (fun () -> Lazy.force l)

let unbounded (type a) ?(hashable = Hashtbl.Hashable.poly) f =
  let cache =
    let module A =
      Hashable.Make_plain_and_derive_hash_fold_t (struct
        type t = a
        let {Hashtbl.Hashable.hash; compare; sexp_of_t} = hashable
      end)
    in
    A.Table.create () ~size:0
  in
  (fun arg ->
     Result.return begin
       Hashtbl.find_or_add cache arg
         ~default:(fun () -> Result.capture f arg)
     end)

(* the same but with a bound on cache size *)
let lru (type a) ?(hashable = Hashtbl.Hashable.poly) ~max_cache_size f =
  if max_cache_size <= 0
  then failwithf "Memo.lru: max_cache_size of %i <= 0" max_cache_size ();
  let module Cache =
    Hash_queue.Make (struct
      type t = a
      let {Hashtbl.Hashable.hash; compare; sexp_of_t} = hashable
    end)
  in
  let cache = Cache.create () in
  (fun arg ->
     Result.return begin
       match Cache.lookup_and_move_to_back cache arg with
       | Some result ->
         result
       | None ->
         let result = Result.capture f arg in
         Cache.enqueue_exn cache arg result;
         (* eject least recently used cache entry *)
         if Cache.length cache > max_cache_size then ignore (Cache.dequeue_exn cache);
         result
     end)

let general ?hashable ?cache_size_bound f =
  match cache_size_bound with
  | None -> unbounded ?hashable f
  | Some n -> lru ?hashable ~max_cache_size:n f

let%test_module "lru" =
  (module struct
    let count = ref 0  (* number of times f underlying function is run *)
    let f = lru ~max_cache_size:3 (fun i -> incr count; i)

    let%test _ = f 0 = 0
    let%test _ = !count = 1

    let%test _ = f 1 = 1
    let%test _ = !count = 2

    let%test _ = f 0 = 0
    let%test _ = !count = 2

    let%test _ = f 3 = 3                       (* cache full *)
    let%test _ = !count = 3

    let%test _ = f 4 = 4                       (* evict 1 *)
    let%test _ = !count = 4

    let%test _ = f 0 = 0
    let%test _ = !count = 4

    let%test _ = f 1 = 1                       (* recompute 1 *)
    let%test _ = !count = 5
  end)
