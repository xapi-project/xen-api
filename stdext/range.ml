type t = { l : int; u : int }

let make l u =
	if l <= u then { l = l; u = u } else invalid_arg "Range.make"

let get r = r.l, r.u

let mem i r = r.l <= i && i < r.u

let rec fold_left_aux f accu l u =
	if l < u then
		fold_left_aux f (f accu l) (l + 1) u
	else accu

let fold_left f accu r = fold_left_aux f accu r.l r.u

let rec fold_right_aux f l u accu =
	if l < u then
		let u = u - 1 in
		fold_right_aux f l u (f u accu)
	else
		accu

let fold_right f r accu = fold_right_aux f r.l r.u accu

let string_of_range r =
	"[" ^ string_of_int r.l ^ ", " ^ string_of_int r.u ^ ")"

let to_list r =
	fold_right (fun x y -> x :: y) r []

