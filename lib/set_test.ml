module type Set = sig
  type t
  val (+): t -> t -> t (* union *)
  val (^): t -> t -> t (* intersection *)
  val (-): t -> t -> t (* difference *)
  val not: t -> t      (* complement *)
  val (=): t -> t -> bool

  val to_string: t -> string
end

module SetEqualities(S: Set) = struct
  open S

  let w txt f a b c = 
	if Pervasives.not(f a b c)
	then failwith (Printf.sprintf "%s a=%s b=%s c=%s" txt (S.to_string a) (S.to_string b) (S.to_string c))
	  
  let all = [
	w "commutative_1" (fun a b _ -> a + b = b + a);
	w "commutative_2" (fun a b _ -> a ^ b = b ^ a);
	w "associative_1" (fun a b c -> (a + b) + c = a + (b + c));
	w "associative_2" (fun a b c -> (a ^ b) ^ c = a ^ (b ^ c));
	w "distributive_1" (fun a b c -> a + (b ^ c) = (a + b) ^ (a + c));
	w "distributive_2" (fun a b c -> a ^ (b + c) = (a ^ b) + (a ^ c));
	w "complement_1" (fun a _ _ -> not (not a) = a);
	w "demorgan_1" (fun a b _ -> not (a + b) = (not a) ^ (not b));
	w "demorgan_2" (fun a b _ -> not (a ^ b) = (not a) + (not b));
  ]
end

