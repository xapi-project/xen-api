
type t = { size: int; mutable current: int; data: (float,Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ; }

(** create a ring structure with @size record. records inited to @initval *)
let make size init =
	let ring = 
		{ size = size; current = size - 1; data = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout size; }
	in
	for i = 0 to Bigarray.Array1.dim ring.data - 1 do
		Bigarray.Array1.set ring.data i init
	done;
	ring

(** length of the ring *)
let length ring = ring.size

(** push into the ring one element *)
let push ring e =
	ring.current <- ring.current + 1;
	if ring.current = ring.size then
		ring.current <- 0;
	Bigarray.Array1.set ring.data ring.current e

(** get the @ith old element from the ring *)
let peek ring i =
	if i >= ring.size then
		raise (Invalid_argument "peek: index");
	let index =
		let offset = ring.current - i in
		if offset >= 0 then offset else ring.size + offset in
	Bigarray.Array1.get ring.data index

(** get the top element of the ring *)
let top ring = Bigarray.Array1.get ring.data ring.current

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
let iter f a = 
	for i=0 to Bigarray.Array1.dim a - 1 do
		f (Bigarray.Array1.get a i)
	done

let raw_iter ring f =
	iter f ring.data

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
