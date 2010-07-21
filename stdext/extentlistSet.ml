
module type Number = sig
	type t
	val zero: t
	val add : t -> t -> t
	val sub : t -> t -> t
end

module ExtentlistSet (A : Number) =
struct
	type extent = A.t * A.t
	type t = extent list

	let ($+) = A.add
	let ($-) = A.sub

	let empty = []

	let sort list : t =
		List.sort (fun x y -> compare (fst x) (fst y)) list

	let remove_zeroes = List.filter (fun (_, y) -> y <> A.zero)

	let union (list1: t) (list2: t) : t =
		let combined = sort (list1 @ list2) in
		let rec inner l acc =
			match l with
				| (s1,e1)::(s2,e2)::ls ->
					let extent1_end = s1 $+ e1 in
					if extent1_end < s2 then
						inner ((s2,e2)::ls) ((s1,e1)::acc)
					else
						let extent2_end = s2 $+ e2 in
						if extent1_end > extent2_end then
							inner ((s1,e1)::ls) acc
						else
							inner ((s1,s2 $+ e2 $- s1)::ls) acc
				| (s1,e1)::[] -> (s1,e1)::acc
				| [] -> []
		in List.rev (inner combined [])

	let intersection (list1: t) (list2: t) =
		let rec inner l1 l2 acc =
			match (l1,l2) with
				| (s1,e1)::l1s , (s2,e2)::l2s ->
					if s1 > s2 then inner l2 l1 acc else
						if s1 $+ e1 < s2 then inner l1s l2 acc else
							if s1 < s2 then inner ((s2,e1 $+ s1 $- s2)::l1s) l2 acc else
								(* s1=s2 *)
								if e1 < e2 then
									inner l1s ((s2 $+ e1,e2 $- e1)::l2s) ((s1,e1)::acc)
								else if e1 > e2 then
									inner ((s1 $+ e2,e1 $- e2)::l1s) l2s ((s2,e2)::acc)
								else (* e1=e2 *)
									inner l1s l2s ((s1,e1)::acc)
				| _ -> List.rev acc
		in
		remove_zeroes(inner list1 list2 [])

	let difference (list1: t) (list2: t) : t =
		let rec inner l1 l2 acc =
			match (l1,l2) with
				| (s1,e1)::l1s , (s2,e2)::l2s ->
					if s1<s2 then begin
						if s1 $+ e1 > s2 then
							inner ((s2,s1 $+ e1 $- s2)::l1s) l2 ((s1,s2 $- s1)::acc)
						else
							inner l1s l2 ((s1,e1)::acc)
					end else if s1>s2 then begin
						if s2 $+ e2 > s1 then
							inner l1 ((s1,s2 $+ e2 $- s1)::l2s) acc
						else
							inner l1 l2s acc
					end else begin
						(* s1=s2 *)
						if e1 > e2 then
							inner ((s1 $+ e2,e1 $- e2)::l1s) l2s acc
						else if e1 < e2 then
							inner l1s ((s2 $+ e1,e2 $- e1)::l2s) acc
						else
							inner l1s l2s acc
					end
				| l1s, [] -> (List.rev acc) @ l1s
				| [], _ -> List.rev acc
		in
		remove_zeroes(inner list1 list2 [])

	let of_list (list: extent list) : t =
		let l = sort list in
		let rec inner ls acc =
			match ls with
				| (s1,e1)::(s2,e2)::rest ->
					(* extents should be non-overlapping *)
					if s1 $+ e1 > s2 then failwith "Bad list"
					(* adjacent extents should be coalesced *)
					else if s1 $+ e1=s2 then inner ((s1,e1 $+ e2)::rest) acc
					else inner ((s2,e2)::rest) ((s1,e1)::acc)
				| (s1,e1)::[] -> List.rev ((s1,e1)::acc)
				| [] -> List.rev acc
		in
		inner l []

	let fold_left = List.fold_left

	let to_list x = x
end
