(* Bloom filter *)

type t = {
	filter: string;
	k: int;
    s: string array;} with rpc

let rndstr () =
	let buf = String.make 20 '\000' in
	let ic = open_in_bin "/dev/urandom" in
	really_input ic buf 0 20;
	close_in ic;
	buf

let create k m =
	{ filter=String.make ((m+6)/7) '\000'; k;
	  s=Array.init k string_of_int }

let bitop t n f =
	let c = n/7 in
	let cur = Char.code t.filter.[c] in
	let bit = 1 lsl (n mod 7) in
	f c cur bit

let set_bit t n =
	bitop t n (fun c cur bit ->
		t.filter.[c] <- Char.chr (cur lor bit))

let test_bit t n =
	bitop t n (fun _ cur bit ->
		(cur land bit) = bit)

let get_bits t x =
	let max_bit = String.length t.filter * 7 in
	let rec inner i =
		if i=t.k then [] else
			let n = Hashtbl.hash (Digest.string (t.s.(i)^x)) in
			let bit = n mod max_bit in
			bit::inner (i+1)
	in inner 0

let add t x = 
	let bits = get_bits t x in
	List.iter (set_bit t) bits

let test t x =
	let bits = get_bits t x in
	List.fold_left (fun r b -> r && test_bit t b) true bits


let time_this f = 
  let start_time = Unix.gettimeofday () in
  let res = f () in
  let end_time = Unix.gettimeofday () in
  (end_time -. start_time, res)

let unittest () =
	let rec gen_strings n =
		if n=0 then [] else
			(rndstr ())::gen_strings (n-1)
	in
	let strings = gen_strings 2000 in
	let others = gen_strings 2000 in
	Printf.printf "Strings created\n";
	let t = create 7 20000 in
	let (time,_) = time_this (fun () -> List.iter (fun s -> add t s) strings) in
	Printf.printf "Strings added (%f seconds)\n" time;
	Printf.printf "Testing for false positives\n";
	let (time,false_positives) = 
		time_this (fun () -> List.filter (fun s -> test t s) others) in
	Printf.printf "Took %f seconds\n" time;
	let n = List.length false_positives in
	Printf.printf "Testing for false negatives\n";
	let (time,check) = time_this (fun () -> List.filter (fun s -> test t s) strings) in
	Printf.printf "Took %f seconds\n" time;
	assert (List.length check = 2000);
	Printf.printf "Error rate: %f\n" (float_of_int n /. 4000.0);




	


