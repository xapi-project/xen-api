(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)


module Make = functor(A: Map.OrderedType) -> functor(B: Map.OrderedType) -> struct
	(** Store a (a: A.t, b:B.t) relation R such that given
		an a, finding the largest set bs such that
          \forall b \in bs. (a, b)\in R
		and v.v. on b are efficient. *)

	module A_Set = Set.Make(A)
	module A_Map = Map.Make(A)
	module B_Set = Set.Make(B)
	module B_Map = Map.Make(B)

	type t = {
		a_to_b: B_Set.t A_Map.t;
		b_to_a: A_Set.t B_Map.t;
	}
	let empty = {
		a_to_b = A_Map.empty;
		b_to_a = B_Map.empty;
	}
	let get_bs a t =
		if A_Map.mem a t.a_to_b
		then A_Map.find a t.a_to_b
		else B_Set.empty
	let get_as b t =
		if B_Map.mem b t.b_to_a
		then B_Map.find b t.b_to_a
		else A_Set.empty
			
	let add a b t =
		{
			a_to_b = A_Map.add a (B_Set.add b (get_bs a t)) t.a_to_b;
			b_to_a = B_Map.add b (A_Set.add a (get_as b t)) t.b_to_a;
		}
	let of_list = List.fold_left (fun t (a, b) -> add a b t) empty
	let to_list t = A_Map.fold (fun a bs acc -> B_Set.fold (fun b acc -> (a, b) :: acc) bs acc) t.a_to_b []

	let remove_a a t =
		let bs = get_bs a t in
		{
			a_to_b = A_Map.remove a t.a_to_b;
			b_to_a =
				B_Set.fold
					(fun b acc ->
						let as' =
							if B_Map.mem b acc
							then B_Map.find b acc
							else A_Set.empty in
						let as' = A_Set.remove a as' in
						if as' = A_Set.empty
						then B_Map.remove b acc
						else B_Map.add b as' acc
					) bs t.b_to_a;
		}
	let remove_b b t =
		let as' = get_as b t in
		{
			a_to_b =
				A_Set.fold
					(fun a acc ->
						let bs =
							if A_Map.mem a acc
							then A_Map.find a acc
							else B_Set.empty in
						let bs = B_Set.remove b bs in
						if bs = B_Set.empty
						then A_Map.remove a acc
						else A_Map.add a bs acc
					) as' t.a_to_b;
			b_to_a = B_Map.remove b t.b_to_a;
		}

	let equal t1 t2 =
		true
		&& A_Map.equal B_Set.equal t1.a_to_b t2.a_to_b
		&& B_Map.equal A_Set.equal t1.b_to_a t2.b_to_a

end


