
type 'a t =
	| Node of string * 'a option * ('a t list)

(* Invariant: the only node with an empty string is the root *)
let empty = Node("", None, [])

let is_prefix a b =
	true
	&& String.length b >= (String.length a)
	&& String.sub b 0 (String.length a) = a

let common_prefix a b =
	let j = ref 0 in (* length of common prefix *)
	let skip = ref false in
	for i = 0 to min (String.length a) (String.length b) - 1 do
		if not !skip
		then if a.[i] = b.[i] then incr j else skip := true
	done;
	String.sub a 0 !j

let sub b a =
	let length = String.length b - (String.length a) in
	String.sub b (String.length b - length) length

let string = function
	| Node(s, _, _) -> s

exception Duplicate_key of string

(* Relying on the invariant that only the root node has an empty string, it is
   safe to examine the first characters of the child strings. Moreover since
   common prefixes are always represented as shared nodes, there can be at most
   one child with the same initial character as the key we're looking up. *)
let choose remaining ns = match List.partition (fun x -> (string x).[0] = remaining.[0]) ns with
	| [ n ], rest -> Some(n, rest)
	| [], _ -> None
	| _ :: _, _ -> assert false

let rec insert k v = function
		(* k could be equal to s *)
	| Node(s, Some _, _) when k = s -> raise (Duplicate_key k)
	| Node(s, None, ns) when k = s -> Node(s, Some v, ns)
		(* k could be a prefix of s *)
	| Node(s, v', ns) when is_prefix k s ->
		assert(sub s k <> "");
		Node(k, Some v, [ Node(sub s k, v', ns) ])
		(* s could be a prefix of k *)
	|  Node(s, v', ns) when is_prefix s k ->
		let remaining = sub k s in
		assert(remaining <> "");
		begin match choose remaining ns with
			| Some (n, rest) -> Node(s, v', insert remaining v n :: rest)
			| None -> Node(s, v', Node(remaining, Some v, []) :: ns)
		end
		(* s and k could share a non-empty common prefix *)
	| Node(s, v', ns) ->
		let p = common_prefix s k in
		let s' = sub s p and k' = sub k p in
		assert (s' <> "");
		assert (k' <> "");
		Node(p, None, [ Node(s', v', ns); Node(k', Some v, []) ])

let rec fold_over_path f str acc = function
	| Node(p, v, _) when p = str -> f acc v
	| Node(p, v, ns) when is_prefix p str ->
		let remaining = sub str p in
		begin match choose remaining ns with
			| Some(n, _) -> fold_over_path f remaining (f acc v) n
			| None -> f acc v
		end
	| _ -> acc

let better acc = function | None -> acc | Some x -> Some x

let longest_prefix str t = fold_over_path better str None t

let fold f acc t =
	let open Xapi_stdext_monadic in
	let rec inner p acc = function
		| Node (p', v, ns) ->
			let pp = p ^ p' in
			let acc = Opt.default acc (Opt.map (fun v -> f pp v acc) v) in
			List.fold_left (fun acc n -> inner pp acc n) acc ns in
	inner "" acc t
