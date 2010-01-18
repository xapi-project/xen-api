type t = (string * (int64 * int64)) list with rpc

let create name size = [(name,(0L,size))]

let get_size (_,(_,s)) = s
let get_start (_,(s,_)) = s
let get_name (n,(_,_)) = n

let make_area name start size = (name,(start,size))

let alloc_specified_area (t : t) a =
  let size = get_size a in
  let start = get_start a in
  let name = get_name a in
  let test a2 = 
    let size2 = get_size a2 in
    let start2 = get_start a2 in
    let name2 = get_name a2 in
    name=name2 && start >= start2 && start < Int64.add size2 start2 
  in
  let containing_areas,other_areas = List.partition test t in
  let containing_area = List.hd containing_areas in
  let ca_start = get_start containing_area in
  let ca_size = get_size containing_area in
  let ca_name = get_name containing_area in
  let new_areas = 
    if start=ca_start then other_areas else (make_area ca_name ca_start (Int64.sub start ca_start))::other_areas
  in
  let new_areas =
    if (Int64.add start size) = (Int64.add ca_start ca_size) 
    then new_areas 
    else (make_area ca_name (Int64.add start size) (Int64.sub (Int64.add ca_start ca_size) (Int64.add start size)))::new_areas
  in
  new_areas

let alloc_specified_areas =
  List.fold_left alloc_specified_area
  
let rec alloc t newsize = 
  let l = List.sort (fun a1 a2 -> compare (get_size a1) (get_size a2)) t in
  let rec find xs ys =
    match ys with
      | seg::[] ->
	  (* If there's only one segment left, it's the largest. Allocate. *)
	  seg,xs
      |	seg::rest -> 
	  let size = get_size seg in
	  if size >= newsize 
	  then seg,(xs@rest)
	  else find (seg::xs) rest
      | _ -> failwith "Failed to find individual segment!"
  in
  let seg,rest = find [] l in
  let size = get_size seg in
  if (size < newsize) then
    (* We couldn't find one contiguous region to allocate. Call alloc again
       with the remainder of the size and the new list of allocated segments *)
    let allocd,newt = alloc (rest) (Int64.sub newsize size) in
    (seg::allocd, newt)
  else
    let name = get_name seg in
    let start = get_start seg in
    let area = make_area name start newsize in
    ([area], alloc_specified_area t area)

let rec setify = function
        | [] -> []
        | (x::xs) -> if List.mem x xs then setify xs else x::(setify xs)

let free t segs =
  let l = List.sort (fun a1 a2 -> compare (get_start a1) (get_start a2)) (t@segs) in
  let pvs = List.map get_name l in
  let pvs = setify pvs in

  let rec test acc segs =
    match segs with
      | a1::a2::rest ->
	  let start1 = get_start a1 in
	  let size1 = get_size a1 in
	  let start2 = get_start a2 in
	  let size2 = get_size a2 in
	  let name = get_name a1 in
	  if (Int64.add start1 size1) = start2 then
	    test acc ((make_area name start1 (Int64.add size1 size2))::rest)
	  else
	    test ((List.hd segs)::acc) (List.tl segs)
      | [x] -> x::acc
      | [] -> acc (* shouldn't be necessary! *)
  in
  
  List.fold_left (fun acc pv -> test acc (List.filter (fun seg -> get_name seg = pv) l)) [] pvs

let to_string t =
  String.concat ", "
    (List.map (fun (p,(s,l)) -> Printf.sprintf "(%s: [%Ld,%Ld])" p s l) t)

let dotest a n =
    let before = List.sort compare a in
    let (alloced,after)=alloc a n in
    let dealloced = List.sort compare (free after alloced) in
    before=dealloced
