
type 'a t = { size: int; mutable current: int; data: 'a array; }

(** create a ring structure with @size record. records inited to @initval *)
let make size initval =
	{ size = size; current = size - 1; data = Array.create size initval; }

(** length of the ring *)
let length ring = ring.size

(** push into the ring one element *)
let push ring e =
	ring.current <- ring.current + 1;
	if ring.current = ring.size then
		ring.current <- 0;
	ring.data.(ring.current) <- e

(** get the @ith old element from the ring *)
let peek ring i =
	if i >= ring.size then
		raise (Invalid_argument "peek: index");
	let index =
		let offset = ring.current - i in
		if offset >= 0 then offset else ring.size + offset in
	ring.data.(index)

(** get the top element of the ring *)
let top ring = ring.data.(ring.current)

(** iterate over nb element of the ring, starting from the top *)
let iter_nb ring f nb =
	if nb > ring.size then
		raise (Invalid_argument "iter_nb: nb");
	(* FIXME: OPTIMIZE ME with 2 Array.iter ? *)
	for i = 0 to nb - 1
	do
		f (peek ring i)
	done

(** iter directly on all element without using the index *)
let raw_iter ring f =
	Array.iter f ring.data

(** iterate over all element of the ring, starting from the top *)
let iter ring f = iter_nb ring f (ring.size)

(** get array of latest #nb value *)
let get_nb ring nb =
	if nb > ring.size then
		raise (Invalid_argument "get_nb: nb");
	let a = Array.create nb (top ring) in
	for i = 1 to nb - 1
	do
		(* FIXME: OPTIMIZE ME with 2 Array.blit *)
		a.(i) <- peek ring i
	done;
	a

let get ring = get_nb ring (ring.size)
