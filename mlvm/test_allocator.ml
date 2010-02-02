open Kaputt.Abbreviations
open Pervasiveext
open Lvm.Allocator
open Fun
open Listext

(* ToDO: find a way to integrate this tests into the Makefile and run them from there. *)

let (=>>) a b = (not a) || b

(* ToDo: Generate some test-data to test those propositions hold: *)

(* let bind f p ga = Gen.map2 ((++) f) p +++ Gen.zip2 *)

let pv_name_gen = (Gen.string (Gen.make_int 0 32) (Gen.alphanum))
let pv_pos_size = Gen.zip2 (Gen.make_int64 0L 121212131L) (Gen.make_int64 0L 121212131L)

let gen_area = (Gen.map3 make_area to_string1 (pv_name_gen, (Gen.make_int64 0L 121212131L), (Gen.make_int64 0L 121212131L)))
(* let gen_area pv_name = (Gen.map3 make_area to_string1 (pv_name, (Gen.make_int64 0L 121212131L), (Gen.make_int64 0L 121212131L))) *)
(* let gen_3area pv_name = *)
(*     let ga = gen_area pv_name *)
(*     in Gen.zip3 ga ga ga *)


(* Does manual lifting.  ToDo: Find a way to make it look less ugly. *)

let gen_3area = 
    let f (name, (p1, p2, p3)) =
	let m = Gen.apply2 (make_area name)
	in (m p1, m p2, m p3)
    and p = Kaputt.Utils.make_string_of_tuple3 to_string1 to_string1 to_string1
    in Gen.map1 f p (Gen.zip2 pv_name_gen (Gen.zip3 pv_pos_size pv_pos_size pv_pos_size))

let prop_contained_reflexive a = contained a a
let () = Test.add_random_test
    ~title:"contained is reflexive"
    gen_area
    Fun.id
    [Spec.always ==> prop_contained_reflexive]

let () = Test.add_random_test
   ~title:"contained is transitive"
   gen_3area
   Fun.id
   [(fun (a,b,c) -> contained a b && contained b c) ==> (fun (a,b,c) -> contained a c)]
    

let prop_same_pv a b = (=>>) (contained a b) (get_name a == get_name b);;


(* allocate some random stuff.  make sure at all times, that (union
   alloced free) = all, and (intersection alloced free) = empty and
   that normalize does not change anything material. *)

let test_make_area =
    let name, start, size = "pv_123", Random.int64 (Int64.of_int 1024), Random.int64 (Int64.of_int 2025) in
    let area = make_area name start size in
    let test0 = ((name, (start, size)) = unpack_area area) in
    let test1 = (get_end area = Int64.add start size) in
    let test2 = (area = make_area_by_end name start (Int64.add start size)) in
    test0 && test1 && test2

let sum64 l = List.fold_left Int64.add Int64.zero l
let foldM op l acc =
    let op_ item = function
	| (Some acc) -> op item acc
	| None -> None
    in List.fold_right op_ l acc

let () =
    Test.add_random_test
      ~title:"alloc allocs all free space and nothing more.  On a single pv for a start."
      (Gen.zip2
	 (Gen.make_int64 (-10L) 10L)
	 (Gen.list (Gen.make_int 0 1000)
	    (Gen.make_int64 0L 1000L)))
      (fun (a, l) ->
	 let free_list = create "pv0" (max 0L $ Int64.add a (sum64 l))
	 in foldM ($) (List.map (fun demand free -> Opt.map snd $ safe_alloc free demand) l) (Some free_list))
      [Spec.always => fun ((a,l), res) -> (((max 0L $ Int64.add a (sum64 l)) < (sum64 l)) = (res = None))]

let size_create_destroy : int64 -> (int64 * int64 * int64) Gen.t = fun max_size -> 
  Gen.zip3 (Gen.make_int64 0L max_size) Gen.int64 Gen.int64

(* needlessly quadratic.  make it linear as the need arises. *)
let cumSum64 l = List.map sum64 ++ List.tails ++ List.rev $ l
let maximum1 (x::xs) = List.fold_left max x xs

let simulate_space : (int64 * int64 * int64) list -> int64 = fun l -> 
  let op (size, d1, d2) = [(min d1 d2,size); (max d1 d2,(Int64.sub 0L size))]
  in maximum1 ++ List.cons 0L ++
       cumSum64 ++ List.map snd ++
       List.sort (on compare fst) ++ List.flatten ++ List.map op $ l

type date = int64
type size = int64
type index = int64
type op = Alloc of (date * size * index) | DeAlloc of (date * index)
let get_date = function | Alloc (date, _, _) | DeAlloc (date, _) -> date

let add_index : 'a list -> (int64 * 'a) list = List.rev ++ fst ++ List.fold_left (fun (l, i) x -> ((i,x)::l, Int64.add i 1L)) ([],0L)

let toOps : (int64 * int64 * int64) list -> op list = 
    let toOp1 (index, (size, d1, d2)) = [Alloc (min d1 d2, size, index); DeAlloc (max d1 d2, index)]
    in List.sort (on compare get_date) ++ List.flatten ++ List.map toOp1 ++ add_index

module IndexMap = Mapext.Make (Int64)
	 
let simulate_full : op list -> t -> (t * (area list) IndexMap.t) option = fun ops free_list ->
  let op (fl, alloced) = function
      | Alloc (_, size, index) ->
	  (match (try safe_alloc fl size with x -> (print_endline "safe_alloc:";
						    print_endline ++ to_string ++ List.sort (on compare (snd ++ snd ++ unpack_area))$ fl;
						    print_endline ++ Int64.to_string $ size;
						    print_endline "";
						    raise x))
	   with | None -> None
                | Some (segs, fl_) -> 
		    Some (fl_, IndexMap.add index segs alloced))
      | DeAlloc (_, index) ->
	  Some (free (IndexMap.find index alloced) fl, IndexMap.remove index alloced)
	      
  in List.fold_left (Opt.default (Fun.const None) ++ Opt.map op) (Some (free_list, IndexMap.empty)) $ ops

let show_op = function
    | Alloc x -> "Alloc " ^ Kaputt.Utils.make_string_of_tuple3 Int64.to_string Int64.to_string Int64.to_string x
    | DeAlloc x -> "DeAlloc " ^ Kaputt.Utils.make_string_of_tuple2 Int64.to_string Int64.to_string x

let () =
    let pv_size = 1000L in
    Test.add_random_test
      ~title:"alloc works when there's enough free space."
      (Gen.list (Gen.make_int 0 300) (size_create_destroy 1000L))
      (Opt.is_boxed ++ Fun.flip simulate_full (create "pv_name0" pv_size) ++ toOps)
      [(fun pOps -> ((simulate_space $ pOps) <= pv_size)) ==> Fun.id;]
let () =
    let pv_size = 1000L in
    Test.add_random_test
      ~title:"and alloc doesn't work when there's not enough free space."
      (Gen.list (Gen.make_int 0 300) (size_create_destroy 1000L))
      (Opt.is_boxed ++ Fun.flip simulate_full (create "pv_name0" pv_size) ++ toOps)
      [(fun pOps -> ((simulate_space $ pOps) > pv_size)) ==> not]

(* tests to add:
   + alloced_segment <*> new_free = empty (intersection)

   generators:

   + make a generator for partly alloced disks. Needs to have
   knowledge of inside stuff --- or do a long sequence of alloc and
   free commands.  We could just generate a random bitmap of alloced
   and free stuff.  Or create random extends after each other.

   (The long list of commands is what we do at the moment.)
*)

(* This revealed a problem with normalize when allocating 0 bytes! It's fixed now.*)
let () =
    let pv_size = 79000L
    and numOps = 300 in
    Test.add_random_test
      ~title:"forall size >= 0: (uncurry free <<= alloc size) == Fun.id # modulo Option types"
      ~nb_runs:200
      (Gen.zip3 (Gen.make_int64 0L 300L)
	 (Gen.make_int 0 (numOps * 2))
	 (Gen.list (Gen.lift numOps (string_of_int numOps)) (size_create_destroy 1000L)))
      (fun (alloc_size, take_ops, pOps) ->
	 match Fun.flip simulate_full (create "pv_name0" pv_size) ++ List.take take_ops ++ toOps $ pOps with
	     | Some (free_list, _) ->
		 (Some (normalize free_list), 
		  (match safe_alloc free_list alloc_size with
		       | Some (alloced, free_list2) -> Some (free alloced free_list2)
		       | None -> None))
	     | None -> (None, None)) (* ToDo: This last line is not the right choice. *)
      [Spec.always ==> (function (Some a, b) -> Some a = b)]
      (* None is not matched for a reason. We do not care about the
	 exception in the test here.  At least not enough to do
	 anything about it.
      
	 Getting None in the first part of the pair just means that
	 there was not enough space left to allocate.  A meaningful
	 fix would mean filtering out on the left hand side of ==> .*)

      (* ToDo: Catch if there's enough space to get free_list, but not enough for free_list2 (i.e. to allocate alloc_size) *)

let _ =	
    let free_list =
	let m = make_area "pv_name0"
	in [m 65652L 11L; m 26860L 9L; m 25282L 5L; m 15696L 8L]
    in match safe_alloc free_list 162L with
	| Some (alloced, free_list2) -> 
	    (print_endline $ "free_list: " ^ to_string free_list;
	     print_endline $ "alloced: " ^ to_string alloced;
	     print_endline $ "free_list2: " ^ to_string free_list2;)
	| None -> print_endline "Not enough space."




let () =
    Test.launch_tests ()
