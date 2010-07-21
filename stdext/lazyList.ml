(* A lazy-list implementation *)

type 'a elt =
	| Empty
	| Cons of 'a * 'a t
and 'a t = 'a elt lazy_t

let rec map f xs = lazy(match Lazy.force xs with
	| Empty -> Empty
	| Cons(x, xs) -> Cons(f x, map f xs))
	
let rec take n xs = lazy(match n, Lazy.force xs with
	| 0, _ -> Empty
	| n, Empty -> raise Not_found
	| n, Cons(x, xs) -> Cons(x, take (n - 1) xs)) 
	
let rec iter f xs = match Lazy.force xs with
	| Empty -> ()
	| Cons(x, xs) -> f x; iter f xs

