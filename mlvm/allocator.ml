open Pervasiveext
open Listext

(* Sparse allocation should be fast. Expanding memory should be fast, for a bunch of volumes. *)

type area = (string * (int64 * int64)) with rpc
type t = area list with rpc

let to_string1 (p,(s,l)) = Printf.sprintf "(%s: [%Ld,%Ld])" p s l
let to_string t =
  String.concat ", "
    (List.map to_string1 t)

let create name size = [(name,(0L,size))]
let empty = []

let get_name (name,(_,_)) = name
let get_start (_,(start,_)) = start
let get_size (_,(_,size)) = size
let unpack_area (pv_name, (start,size)) = (pv_name, (start,size))

let get_end a = Int64.add (get_start a) (get_size a)

let make_area pv_name start size = (pv_name, (start,size))
let make_area_by_end name start endAr = make_area name start (Int64.sub endAr start)

(* Define operations on areas, and then use those to build the
   allocation algorithms.  That should make it easier to test, and the
   algorithms are easier to read without arithmetic in them.
*)

let intersect : area -> area -> area list = 
    fun a a2 ->
	let (name, (start, size)) = unpack_area a in
	let (name2, (start2, size2)) = unpack_area a2 in
	let enda = get_end a in
	let enda2 = get_end a2 in
	let startI = max start start2 in
	let endI = min enda enda2 in
	let sizeI = max Int64.zero (Int64.sub endI startI) in
	if name = name2 
	then make_area name (max start start2) (max Int64.zero sizeI) :: []
	else []

let combine : t -> t -> t = (* does not guarantee normalization *)
    fun t1 t2 ->
	t1 @ t2 

let union : area -> area -> t = (* does not guarantee normalization *)
    fun a a2 ->
	a::a2::[]
let minus : area -> area -> t = (* does not guarantee normalization *)
    fun a a2 ->
	let (name, (start, size)) = unpack_area a in
	let (name2, (start2, size2)) = unpack_area a2 in
	let enda = get_end a in
	let enda2 = get_end a2 in
        if name = name2
	then List.filter ((<) Int64.zero ++ get_size) ++ List.fold_left combine [] ++ List.map (intersect a ++ Fun.uncurry (make_area_by_end name2)) $ ((start, start2) :: (enda2, enda)::[])
	else a :: []

(* Is a contained in a2? *)
let contained : area -> area -> bool =
    fun a a2 ->
	let (name, (start, size)) = unpack_area a in
	let (name2, (start2, size2)) = unpack_area a2 in
	name=name2 && start >= start2 && Int64.add start size <= Int64.add start2 size2

exception PVS_DONT_MATCH of string * string

(* assumes all areas stem from the same pv *)
let normalize_single_pv areas =
    (* Underlying structure for merge1: foldM merge1 (for a1) on WriterMonad (for acc) over segs *)
    (* The type of the accumulator here is a bit ugly.  Perhaps a real non-empty list would be better? *)
    let merge1 (a1, acc) a2 =
	let (name, (start1, size1)) = unpack_area a1
	and (name2, (start2, size2)) = unpack_area a2 in
	if (name <> name2) then raise (PVS_DONT_MATCH (name, name2))
	else if (Int64.add start1 size1) = start2 then
	    (make_area name start1 (Int64.add size1 size2), acc)
	else
	    (a2, List.cons a1 acc) in
    (function
	 | start::segs -> 
	       (Fun.uncurry List.cons) $ List.fold_left merge1 (start, []) segs
	 | [] -> [] (* shouldn't be necessary! *))
    ++ List.sort (Fun.on compare get_start) ++ List.filter ((<) 0L ++ get_size) $ areas
let normalize : t -> t = 
    fun areas ->
    (* The next lines are to be read backwards, since we defined function composition that way. *)

    let module StringMap = Mapext.Make (String) in
    (* put free areas of all PVs back together *)
    List.flatten ++ StringMap.values
	(* normalize each pv's areas *)
    ++ StringMap.map normalize_single_pv
	(* separate by pv *)
    ++ StringMap.fromListWith List.append ++ List.map (fun seg -> (get_name seg, [seg]))
	$ areas

(* Which invariants does t have to satisfy?  Which invariants does our
   result here satisfy?

   E.g. is it possible for areas to overlap or contain each other?  If not, should we warn if they do?

   t is a free list.

   What if there's no containing area? Is this only called under certain circumstances? Verify. *)
exception NonSingular_Containing_Area
let alloc_specified_area (free_list : t) (a : area) =
    (* We assume areas don't overlap, or do we? *)
    (* Match against [] instead of _: Better die as soon as possible, when something is wrong. 
     * And that was right!  Just caught a bug that would have been masked otherwise. *)
    match List.partition (contained a) ++ normalize $ free_list with
	| (containing_area::[]), other_areas -> normalize $ combine (minus containing_area a) other_areas
	| x,_ -> (print_string "alloc_specified_area:\t";
		  print_endline ++ to_string $ x;
		  raise NonSingular_Containing_Area;)

let alloc_specified_areas : t -> t -> t =
   List.fold_left alloc_specified_area

let safe_alloc (free_list : t) (newsize : int64) =
    (* switched from best-fit (smallest free area that's large enough)
       to worst-fit (largest area): This may reduce fragmentation, and
       makes the code slightly easier. *)
    let rec alloc_h newsize = function
	| (seg::rest) -> 
	    let remainder = Int64.sub newsize (get_size seg) in
	    if (remainder > Int64.zero) then
                (* We couldn't find one contiguous region to allocate. Call alloc again
		   with the remainder of the size and the new list of allocated areas *)
		match alloc_h remainder rest with
		    | Some (allocd,newt) -> Some (seg::allocd, newt)
		    | None -> None
	    else
                let (name, (start, _)) = unpack_area seg in
                let area = make_area name start newsize in
                Some ([area], try (alloc_specified_area (seg::rest) area) with (Match_failure x) -> (print_endline "alloc_specified_area"; raise (Match_failure x)))
	| [] -> None in
    alloc_h newsize
    ++ List.rev ++ List.sort (Fun.on compare get_size) $ free_list
      
let alloc (free_list : t) (newsize : int64) =
    match safe_alloc free_list newsize
    with  Some x -> x
	| None -> failwith "Failed to find individual area!"

(* Probably de-allocation won't be used much. *)
let free to_free free_list = normalize (combine to_free free_list)

let dotest a n =
    let before = List.sort compare a in
    let (alloced,after)=alloc a n in
    let dealloced = List.sort compare (free after alloced) in
    before=dealloced
