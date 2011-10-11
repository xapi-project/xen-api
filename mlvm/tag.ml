open Stringext

type t = string with rpc

module CharSet = Set.Make(struct type t = char let compare = compare end)

let first_char_list = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+."

let first_char_set = String.fold_left (fun set x -> CharSet.add x set) CharSet.empty first_char_list

let other_char_set = CharSet.add '-' first_char_set

(** This function assumes "String.length s = len" and "len > 0". *)
let has_valid_chars s len =
	if not (CharSet.mem s.[0] first_char_set) then false else
	let rec check_char_at i = (* Tail-recursion. *)
		(i >= len) || (CharSet.mem s.[i] other_char_set && check_char_at (i + 1)) in
	check_char_at 1

let is_valid s =
	let len = String.length s in
	(0 < len) && (len <= 128) && (has_valid_chars s len)

let of_string s =
	if is_valid s then s else failwith "Tag string does not conform to the rules."

let string_of t =
  t
