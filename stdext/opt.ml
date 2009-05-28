let iter f = function
	| Some x -> f x
	| None -> ()

let map f = function
	| Some x -> Some(f x)
	| None -> None

let default d = function
	| Some x -> x
	| None -> d

let unbox = function
	| Some x -> x
	| None -> raise Not_found

let is_boxed = function
	| Some _ -> true
	| None -> false

let to_list = function
	| Some x -> [x]
	| None -> []

let fold_left f accu = function
	| Some x -> f accu x
	| None -> accu

let fold_right f opt accu =
	match opt with
	| Some x -> f x accu
	| None -> accu
