(*
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Camlp4
open PreCast
open Ast
open Syntax


let is_base = function
	| "int64" | "int32" | "int" | "float" | "string" | "unit" -> true
	| _ -> false

let rpc_of n = "rpc_of_" ^ n

let of_rpc n = n ^ "_of_rpc"

let rpc_of_polyvar a = "__rpc_of_" ^ a ^ "__"
let of_rpc_polyvar a = "__" ^ a ^ "_of_rpc__"

let rpc_of_i i = "__rpc_of_" ^ string_of_int i ^ "__"
let of_rpc_i i = "__" ^ string_of_int i ^ "_of_rpc__"

(* Utils *)

let list_foldi f step0 l =
	fst (List.fold_left (fun (accu, i) x -> f accu x i, i+1) (step0, 0) l)

let list_of_ctyp_decl tds =
	let rec aux accu = function
	| Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
	| Ast.TyDcl (loc, id, args, ty, []) -> (id, args, ty) :: accu
	| _                               ->  failwith "list_of_ctyp_decl: unexpected type"
	in aux [] tds

let rec decompose_fields _loc fields =
	match fields with
	| <:ctyp< $t1$; $t2$ >> ->
		decompose_fields _loc t1 @ decompose_fields _loc t2
	| <:ctyp< $lid:field_name$: mutable $t$ >> | <:ctyp< $lid:field_name$: $t$ >> ->
		[ field_name, t ]
	| _ -> failwith "unexpected type while processing fields"

let expr_list_of_list _loc exprs =
	match List.rev exprs with
	| []   -> <:expr< [] >>
	| h::t -> List.fold_left (fun accu x -> <:expr< [ $x$ :: $accu$ ] >>) <:expr< [ $h$ ] >> t 

let patt_list_of_list _loc patts =
	match List.rev patts with
	| []   -> <:patt< [] >>
	| h::t -> List.fold_left (fun accu x -> <:patt< [ $x$ :: $accu$ ] >>) <:patt< [ $h$ ] >> t

let expr_tuple_of_list _loc = function
	| []   -> <:expr< >>
	| [x]  -> x
	| h::t -> ExTup (_loc, List.fold_left (fun accu n -> <:expr< $accu$, $n$ >>) h t)

let patt_tuple_of_list _loc = function
	| []   -> <:patt< >>
	| [x]  -> x
	| h::t -> PaTup (_loc, List.fold_left (fun accu n -> <:patt< $accu$, $n$ >>) h t)

let name_of_polyvar _loc = function
	| <:ctyp< '$lid:a$ >> -> a
	| _ -> failwith "name_of_polyvar"

let rec decompose_args _loc = function
	| <:ctyp< $x$ $y$ >> -> decompose_args _loc x @ decompose_args _loc y
	| <:ctyp< $x$     >> -> [x]

let decompose_variants _loc variant =
	let rec fn accu = function
	| <:ctyp< $t$ | $u$ >>        -> fn (fn accu t) u
	| <:ctyp< $uid:id$ of $t$ >>  -> ((id, `V) , list_of_ctyp t []) :: accu
	| <:ctyp< `$uid:id$ of $t$ >> -> ((id, `PV), list_of_ctyp t []) :: accu
	| <:ctyp< $uid:id$ >>         -> ((id, `V) , []) :: accu
	| <:ctyp< `$uid:id$ >>        -> ((id, `PV), []) :: accu
	| _ -> failwith "decompose_variant"
	in
	List.split (fn [] variant)

let recompose_variant _loc (n, t) patts =
	match t, patts with
	| `V , [] -> <:patt< $uid:n$ >>
	| `PV, [] -> <:patt< `$uid:n$ >>
	| `V , _  -> <:patt< $uid:n$ $patt_tuple_of_list _loc patts$ >>
	| `PV, _  -> <:patt< `$uid:n$ $patt_tuple_of_list _loc patts$ >>

let count = ref 0
let new_id _loc =
	incr count;
	let new_id = Printf.sprintf "__x%i__" !count in
	<:expr< $lid:new_id$ >>, <:patt< $lid:new_id$ >>

let new_id_list _loc l =
	List.split (List.map (fun _ -> new_id _loc) l)

let type_not_supported ty =
	let module PP = Camlp4.Printers.OCaml.Make(Syntax) in
	let pp = new PP.printer () in
	Format.eprintf "Type %a@. not supported.\n%!" pp#ctyp ty;
	failwith "type not supported by rpc-light"

let apply _loc fn fn_i create id modules t a =
	let args = decompose_args _loc a in
	let app expr = list_foldi (fun accu _ i -> <:expr< $accu$ $lid:fn_i i$ >>) expr args in
	let expr = match modules with
		| None    -> <:expr< $app <:expr< $lid:fn t$ >>$ $id$ >>
		| Some ms -> <:expr< $app <:expr< $id:ms$ . $lid:fn t$ >>$ $id$ >> in
	list_foldi
		(fun accu arg i ->
			 let id, pid = new_id _loc in
			 <:expr< let $lid:fn_i i$ = fun $pid$ -> $create id arg$ in $accu$ >>)
		expr
		args

let is_option = function
	| <:ctyp@loc< option $_$ >> -> true
	| _                         -> false

let is_string _loc key =
	if key = "string" then
		<:expr< True >>
	else if is_base key then
		<:expr< False >>
	else <:expr< try let ( _ : $lid:key$ ) = $lid:of_rpc key$ (Rpc.String "") in True with [ _ -> False ] >>

(* Conversion ML type -> Rpc.value *)
module Rpc_of = struct
	
	let rec product get_field t =
		let _loc = loc_of_ctyp t in
		let fields = decompose_fields _loc t in
        let ids, pids = new_id_list _loc fields in
		let bindings = List.map2 (fun pid (f, _) -> <:binding< $pid$ = $get_field f$ >>) pids fields in
		let aux nid (n, ctyp) accu =
			if is_option ctyp then begin
				let new_id, new_pid = new_id _loc in
				<:expr<
					match $create nid ctyp$ with [
					  Rpc.Enum []            -> $accu$
					| Rpc.Enum [ $new_pid$ ] -> [ ($str:n$, $new_id$) :: $accu$ ]
					| _                      -> assert False
					] >>
			end else
				<:expr< [ ($str:n$, $create nid ctyp$) :: $accu$ ] >> in
		let expr = <:expr< Rpc.Dict $List.fold_right2 aux ids fields <:expr< [] >>$ >> in
		<:expr< let $biAnd_of_list bindings$ in $expr$ >>

	and create id ctyp =
		let _loc = loc_of_ctyp ctyp in
		match ctyp with
		| <:ctyp< unit >>    -> <:expr< Rpc.Null >>
		| <:ctyp< int >>     -> <:expr< Rpc.Int (Int64.of_int $id$) >>
		| <:ctyp< int32 >>   -> <:expr< Rpc.Int (Int64.of_int32 $id$) >>
		| <:ctyp< int64 >>   -> <:expr< Rpc.Int $id$ >>
		| <:ctyp< float >>   -> <:expr< Rpc.Float $id$ >>
		| <:ctyp< char >>    -> <:expr< Rpc.Int (Int64.of_int (Char.code $id$)) >>
		| <:ctyp< string >>  -> <:expr< Rpc.String $id$ >>
		| <:ctyp< bool >>    -> <:expr< Rpc.Bool $id$ >>

		| <:ctyp< list (string * $t$) >> ->
			let nid, pid = new_id _loc in
			<:expr<
				let dict = List.map (fun (key, $pid$) -> (key, $create nid t$)) $id$ in
				Rpc.Dict dict >>

		| <:ctyp< list ($lid:key$ * $t$) >> when not (is_base key) ->
			let nid1, pid1 = new_id _loc in
			let nid2, pid2 = new_id _loc in
			<:expr<
				let is_a_real_dict = $is_string _loc key$ in
				let dict = List.map (fun ($pid1$, $pid2$) -> ($lid:rpc_of key$ $nid1$, $create nid2 t$)) $id$ in
				if is_a_real_dict then
					Rpc.Dict (List.map (fun [ (Rpc.String k, v) -> (k, v) | _ -> assert False ]) dict)
				else
					Rpc.Enum (List.map (fun (k, v) -> Rpc.Enum [k; v] ) dict) >>

		| <:ctyp< Hashtbl.t string $t$ >> ->
			let nid, pid = new_id _loc in
			<:expr<
				let dict = Hashtbl.fold (fun a $pid$ c -> [(a, $create nid t$)::c]) $id$ [] in
				Rpc.Dict dict >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let ids, ctyps = decompose_variants _loc t in
			let pattern (n, t) ctyps =
				let ids, pids = new_id_list _loc ctyps in
				let body =
					if ids = [] then
						<:expr< Rpc.String $str:n$ >>
					else
						<:expr< Rpc.Enum [ Rpc.String $str:n$ :: $expr_list_of_list _loc (List.map2 create ids ctyps)$ ] >> in
				<:match_case< $recompose_variant _loc (n,t) pids$ -> $body$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [ Some $new_pid$ -> Rpc.Enum [ $create new_id t$ ] | None -> Rpc.Enum [] ] >> 

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 create ids ctyps in
			<:expr<
				let $patt_tuple_of_list _loc pids$ = $id$ in
				Rpc.Enum $expr_list_of_list _loc exprs$
			>>

		| <:ctyp< list $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< Rpc.Enum (List.map (fun $new_pid$ -> $create new_id t$) $id$) >>

		| <:ctyp< array $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< Rpc.Enum (Array.to_list (Array.map (fun $new_pid$ -> $create new_id t$) $id$)) >>

		| <:ctyp< { $t$ } >>              -> product (fun field -> <:expr< $id$ . $lid:field$ >>) t
		| <:ctyp< < $t$ > >>              -> product (fun field -> <:expr< $id$ # $lid:field$ >>) t

		| <:ctyp< '$lid:a$ >>             -> <:expr< $lid:rpc_of_polyvar a$ $id$  >>

		| <:ctyp< $lid:t$ >>              -> <:expr< $lid:rpc_of t$ $id$  >>
		| <:ctyp< $id:m$ . $lid:t$ >>     -> <:expr< $id:m$ . $lid:rpc_of t$ $id$  >>

		| <:ctyp< $lid:t$ $a$ >>          -> apply _loc rpc_of rpc_of_i create id None t a
		| <:ctyp< $id:m$ . $lid:t$ $a$ >> -> apply _loc rpc_of rpc_of_i create id (Some m) t a

		| _ -> type_not_supported ctyp

	let gen_one (name, args, ctyp) =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $lid:rpc_of name$ =
			$List.fold_left
				(fun accu arg -> <:expr< fun $lid:rpc_of_polyvar (name_of_polyvar _loc arg)$ -> $accu$ >>)
				(<:expr< fun $pid$ -> $create id ctyp$ >>)
				args$
		>>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let bindings = List.map gen_one (list_of_ctyp_decl tds) in
		biAnd_of_list bindings
end


(* Conversion Rpc.value -> ML type *)
module Of_rpc = struct

	let str_of_id id = match id with <:expr@loc< $lid:s$ >> -> <:expr@loc< $str:s$ >> | _ -> assert false

	let runtime_error name id expected =
		let _loc = Loc.ghost in
		<:match_case<  __x__ -> do {
			if Rpc.get_debug () then
				Printf.eprintf "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n" $str:name$ $str_of_id id$ (Rpc.to_string __x__) $str:expected$
			else ();
			raise (Rpc.Runtime_error ($str:expected$, __x__)) }
		>>

	let runtime_exn_error name id doing =
		let _loc = Loc.ghost in
		<:match_case< __x__ -> do {
			if Rpc.get_debug () then
				Printf.eprintf "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n" $str:name$ $str_of_id id$ (Printexc.to_string __x__) $str:doing$
			else () ;
			raise (Rpc.Runtime_exception ($str:doing$, Printexc.to_string __x__)) }		>>

	let product name build_one build_all id t =
		let _loc = loc_of_ctyp t in
		let nid, npid = new_id _loc in
		let fields = decompose_fields _loc t in
		let ids, pids = new_id_list _loc fields in
		let exprs = List.map2 (fun id (n, ctyp) -> build_one n id ctyp) ids fields in
		let bindings =
			List.map2 (fun pid (n, ctyp) ->
				if is_option ctyp then begin
					<:binding< $pid$ =
						if List.mem_assoc $str:n$ $nid$ then
							Rpc.Enum [List.assoc $str:n$ $nid$]
						else
							Rpc.Enum []
					>>
				end else
					<:binding< $pid$ = try List.assoc $str:n$ $nid$ with [ $runtime_exn_error name nid ("Looking for key "^n)$ ] >>
				) pids fields in
		<:expr< match $id$ with
			[ Rpc.Dict $npid$ -> let $biAnd_of_list bindings$ in $build_all exprs$ | $runtime_error name id "Dict"$ ]
		>>

	let rec create name id ctyp =
		let _loc = loc_of_ctyp ctyp in
		match ctyp with
		| <:ctyp< unit >>   -> <:expr< match $id$ with [ Rpc.Null -> () | $runtime_error name id "Null"$ ] >>

		| <:ctyp< int >>    ->
			<:expr< match $id$ with [
			  Rpc.Int x    -> Int64.to_int x
			| Rpc.String s -> int_of_string s
			| $runtime_error name id "Int(int)"$ ] >>

		| <:ctyp< int32 >>  ->
			<:expr< match $id$ with [
			  Rpc.Int x    -> Int64.to_int32 x
			| Rpc.String s -> Int32.of_string s
			| $runtime_error name id "Int(int32)"$ ] >>

		| <:ctyp< int64 >>  ->
			<:expr< match $id$ with [
			  Rpc.Int x    -> x
			| Rpc.String s -> Int64.of_string s
			| $runtime_error name id "Int(int64)"$ ] >>

		| <:ctyp< float >>  ->
			<:expr< match $id$ with [
			  Rpc.Float x  -> x
			| Rpc.String s -> float_of_string s
			| $runtime_error name id "Float"$ ] >>

		| <:ctyp< char >>   ->
			<:expr< match $id$ with [
			  Rpc.Int x    -> Char.chr (Int64.to_int x)
			| Rpc.String s -> Char.chr (int_of_string s)
			| $runtime_error name id "Int(char)"$ ] >>

		| <:ctyp< string >> -> <:expr< match $id$ with [ Rpc.String x -> x | $runtime_error name id "String(string)"$ ] >>
		| <:ctyp< bool >>   -> <:expr< match $id$ with [ Rpc.Bool x -> x | $runtime_error name id "Bool"$ ] >>

		| <:ctyp< list (string * $t$ ) >> ->
			let nid, pid = new_id _loc in
			<:expr< match $id$ with [
			  Rpc.Dict d -> List.map (fun (key, $pid$) -> (key, $create name nid t$)) d
			| $runtime_error name id "Dict"$ ] >>

		| <:ctyp< list ($lid:key$ * $t$) >> when not (is_base key) ->
			let nid, pid = new_id _loc in
			<:expr<
				let is_a_real_dict = $is_string _loc key$ in
				if is_a_real_dict then begin
					match $id$ with [
					  Rpc.Dict d -> List.map (fun (key, $pid$) -> ($lid:of_rpc key$ (Rpc.String key), $create name nid t$)) d
					| $runtime_error name id "Dict"$ ]
				end else begin
					match $id$ with [
					  Rpc.Enum e -> List.map (fun $pid$ -> $create name nid <:ctyp< ($lid:key$ * $t$) >>$) e
					| $runtime_error name id "Enum"$ ]
				end >>

		| <:ctyp< Hashtbl.t string $t$ >> ->
			let nid, pid = new_id _loc in
			<:expr< match $id$ with [
				  Rpc.Dict d ->
					let h = Hashtbl.create (List.length d) in
					do { List.iter (fun (key,$pid$) -> Hashtbl.add h key $create name nid t$) d; h}
				| $runtime_error name id "Dict"$ ] >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let ids, ctyps = decompose_variants _loc t in
			let pattern (n, t) ctyps =
				let ids, pids = new_id_list _loc ctyps in
				let patt =
					if ids = [] then
						<:patt< Rpc.String $str:n$ >>
					else
						<:patt< Rpc.Enum [ Rpc.String $str:n$ :: $patt_list_of_list _loc pids$ ] >> in
				let exprs = List.map2 (create name) ids ctyps in
				let body = List.fold_right
					(fun a b -> <:expr< $b$ $a$ >>)
					(List.rev exprs)
					(if t = `V then <:expr< $uid:n$ >> else <:expr< `$uid:n$ >>) in
				<:match_case< $patt$ -> $body$ >> in
			let fail_match = <:match_case< $runtime_error name id "Enum[String s;...]"$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps @ [ fail_match ]) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let nid, npid = new_id _loc in
			<:expr< match $id$ with [ Rpc.Enum [] -> None | Rpc.Enum [ $npid$ ] -> Some $create name nid t$ | $runtime_error name id "Enum[]/Enum[_]"$ ] >>

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 (create name) ids ctyps in
			<:expr< match $id$ with
				[ Rpc.Enum $patt_list_of_list _loc pids$ -> $expr_tuple_of_list _loc exprs$ | $runtime_error name id "List"$ ]
			>>

		| <:ctyp< list $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with
				[ Rpc.Enum $npid$ -> List.map (fun $npid2$ -> $create name nid2 t$) $nid$ | $runtime_error name id "List"$ ]
			>>

		| <:ctyp< array $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with
				[ Rpc.Enum $npid$ -> Array.of_list (List.map (fun $npid2$ -> $create name nid2 t$) $nid$) | $runtime_error name id "List"$ ]
			>>

		| <:ctyp< { $t$ } >> ->
			product name (fun n i ctyp -> <:rec_binding< $lid:n$ = $create name i ctyp$ >>) (fun es -> <:expr< { $rbSem_of_list es$ } >>) id t

		| <:ctyp< < $t$ > >> ->
			product name (fun n i ctyp -> <:class_str_item< method $lid:n$ = $create name i ctyp$ >>) (fun es -> <:expr< object $crSem_of_list es$ end >>) id t

		| <:ctyp< '$lid:a$ >>             -> <:expr< $lid:of_rpc_polyvar a$ $id$ >>

		| <:ctyp< $lid:t$ >>              -> <:expr< $lid:of_rpc t$ $id$ >>
		| <:ctyp< $id:m$ . $lid:t$ >>     -> <:expr< $id:m$ . $lid:of_rpc t$ $id$ >>

		| <:ctyp< $lid:t$ $a$ >>          -> apply _loc of_rpc of_rpc_i (create name) id None t a
		| <:ctyp< $id:m$ . $lid:t$ $a$ >> -> apply _loc of_rpc of_rpc_i (create name) id (Some m) t a

		| _ -> type_not_supported ctyp

	let gen_one (name, args, ctyp) =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $lid:of_rpc name$ = 
			$List.fold_left
				(fun accu arg -> <:expr< fun $lid:of_rpc_polyvar (name_of_polyvar _loc arg)$ -> $accu$ >>)
				(<:expr< fun $pid$ -> $create name id ctyp$ >>)
				args$
		>>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let bindings = List.map gen_one (list_of_ctyp_decl tds) in
		biAnd_of_list bindings
end


let gen tds =
	let _loc = loc_of_ctyp tds in
	<:str_item<
		value rec $Of_rpc.gen tds$;
		value rec $Rpc_of.gen tds$;
	>>

