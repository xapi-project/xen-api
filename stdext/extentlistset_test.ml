(* We will check if a list of set equalities hold over random inputs *)

open Set_test

(* We test using the integer domain only. *)
module Intextentlist = ExtentlistSet.ExtentlistSet(struct 
  type t=int 
  let zero=0 
  let add=(+) 
  let sub=(-) 
end)
open Intextentlist

(* Sets are finite, up to cardinality [size] *)
let size = 1000

module Tests = SetEqualities(struct
	type t = Intextentlist.t
	let universe = of_list [(0, size)]
	let (+) = union
	let (^) = intersection
	let (-) = difference

	let not x = universe - x
	let (=) x y = (x - y = empty) && (y - x = empty)
	let extent_to_string (s, l) = Printf.sprintf "(%d, %d)" s l
	let to_string xs = String.concat ", " (List.map extent_to_string (to_list xs))
end)
(* Given a triple of inputs, check that all the set equalities hold *)
let one (a, b, c) = List.iter (fun f -> f a b c) Tests.all

open LazyList

(** [make p s e] return an extentlist starting at [s], ending before [e] where
    an integer x is covered by the extentlist iff [p x] *)
let make p s e =
  let rec ints acc a b = if a < b then ints (a :: acc) (a + 1) b else acc in
  of_list (List.fold_left (fun acc x -> if p x then (x, 1)::acc else acc) [] (ints [] s e))

(* A lazy-list of random triples (a, b, c)*)
let random_inputs =	
  let one () = make (fun _ -> Random.bool ()) 0 (size - 1) in
  (* Create triples of random inputs for the checker *)
  let three () = one (), one (), one () in
  let rec f () = lazy (Cons(three (), f ())) in
  f ()

let _ = 
  let n = 1000 in
  iter (fun _ -> ()) (take n (map one random_inputs));
  Printf.printf "%d random sets of maximum size %d checked.\n" n size

type run = 
  | Empty of int
  | Full of int
let to_run_list xs = 
  let rec inner acc index = function
	| [] -> acc
	| (x, y) :: xs -> inner (Full y :: (Empty (x - index)) :: acc) (x + y) xs in  let map f xs = 
	let rec inner acc f = function
	  | [] -> acc
	  | (x :: xs) -> inner ((f x)::acc) f xs in
	  inner [] f xs in

	List.rev (inner [] 0 xs)

let _ =
  (* vhds have max size of 2 TiB, in 2 MiB blocks => 2**20 blocks *)
  (* The BAT consists of up to 2**20 blocks in any order *)
  (* Worst case for us is as many singleton blocks as possible, which *)
  (* can't be coalesced because they don't have neighbours. The maximum *)
  (* number of blocks is achieved with the allocation pattern 10101010... *)
  (* i.e. 2**19 singleton blocks. *)

  (* As a bitmap we would have 2**19 / 2**3 = 2**16 bytes (64kbit) *)
  let worst_case = make (fun x -> x mod 2 = 1) 0 (1024*1024/2/12) in
  let hex (a, b) = Printf.sprintf "%x,%x" a b in
  let to_string xs = "[" ^ (String.concat ";" (Listext.List.map_tr hex xs)) ^ "]" in


  Printf.printf "generated\n";
	let x = to_list worst_case in
Printf.printf "got a list\n";
	  let y = Listext.List.map_tr hex x in
Printf.printf "got lots of strings\n";
  let s = to_string (to_list worst_case) in
  Printf.printf "Extent size=%d\n" (String.length s);
	let string_of_run = function
	  | Empty x -> Printf.sprintf "-%d" x
	  | Full x -> Printf.sprintf "+%d" x in
	let s' = String.concat ";" (Listext.List.map_tr string_of_run (to_run_list x)) in
	  Printf.printf "Runs size=%d\n" (String.length s')
