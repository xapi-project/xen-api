(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* -pp camlp4orf *)

open Camlp4
open PreCast
open Ast
open Syntax

(* utils *)

let biList_to_expr _loc bindings final =
	List.fold_right 
		(fun b a -> <:expr< let $b$ in $a$ >>)
		bindings final

let function_with_label_args _loc ~fun_name ~final_ident ~function_body ~return_type opt_args =
   let opt_args = opt_args @ [ <:patt< $lid:final_ident$ >> ] in
   <:binding< $lid:fun_name$ = 
      $List.fold_right (fun b a ->
        <:expr<fun $b$ -> $a$ >>
       ) opt_args <:expr< ( $function_body$ : $return_type$ ) >>
      $ >>

let rec list_of_fields _loc fields =
	match fields with
	| <:ctyp< $t1$; $t2$ >> ->
		list_of_fields _loc t1 @ list_of_fields _loc t2
	| <:ctyp< $lid:field_name$: mutable $t$ >> | <:ctyp< $lid:field_name$: $t$ >> ->
		[ field_name, t ]
	| _ -> failwith "unexpected type while processing fields"

let record_of_fields _loc fields =
	let rec_bindings = List.map (fun (n,e) -> Ast.RbEq(_loc, <:ident< $lid:n$ >>, e)) fields in
	<:expr< { $rbSem_of_list rec_bindings$ } >>

let list_of_expr _loc exprs =
	match List.rev exprs with
	| []   -> <:expr< [ ] >>
	| h::t -> List.fold_left (fun accu x -> <:expr< [ $x$ :: $accu$ ] >>) <:expr< [ $h$ ] >> t 

let patt_list_of_expr _loc patts =
	match List.rev patts with
	| []   -> assert false
	| h::t -> List.fold_left (fun accu x -> <:patt< [ $x$ :: $accu$ ] >>) <:patt< [ $h$ ] >> t

let tuple_of_expr _loc exprs =
	match List.rev exprs with
	| []   -> assert false
	| h::t -> Ast.ExTup ( _loc, List.fold_left (fun accu x -> <:expr< $x$,$accu$ >>) h t)
(* BUG? <:expr< ( $exCom_of_list exprs$ ) doesn't work >> *)

let patt_tuple_of_expr _loc patts = 
	Ast.PaTup (_loc, paCom_of_list patts)
(* BUG?	<:patt< ( $paCom_of_list patts$ ) doesn't work >> *)

let decompose_variants _loc variant =
	let rec fn accu = function
	| <:ctyp< $t$ | $u$ >> -> fn (fn accu t) u
	| <:ctyp< $uid:id$ of $t$ >> -> (id, Some t) :: accu
	| <:ctyp< $uid:id$ >> -> (id, None) :: accu
	| _ -> failwith "decompose_variant"
	in fn [] variant

let count = ref 0
let new_id _loc =
	incr count;
	let new_id = Printf.sprintf "__x%i__" !count in
	<:expr< $lid:new_id$ >>, <:patt< $lid:new_id$ >>

(* conversion ML type -> Rpc.Value.t *)
module Rpc_of_ML = struct
	
	let rec value_of_ctyp _loc id = function
		| <:ctyp< unit >>    -> <:expr< Rpc.String "nil" >>
		| <:ctyp< int >>     -> <:expr< Rpc.Int $id$ >>
		| <:ctyp< int32 >>   -> <:expr< Rpc.String (Int32.to_string $id$) >>
		| <:ctyp< int64 >>   -> <:expr< Rpc.String (Int64.to_string $id$) >>
		| <:ctyp< float >>   -> <:expr< Rpc.Double $id$ >>
		| <:ctyp< char >>    -> <:expr< Rpc.String (sprintf "%c" $id$) >>
		| <:ctyp< string >>  -> <:expr< Rpc.String $id$ >>
		| <:ctyp< bool >>    -> <:expr< Rpc.Bool $id$ >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let decomp = decompose_variants _loc t in
			let patterns =
				List.map (fun (n, t) ->
					let new_id, new_pid = new_id _loc in
					match t with
					| None -> 
						<:match_case< $uid:n$ -> Rpc.Array [ Rpc.String $str:n$ ] >>
					| Some t ->
						<:match_case< $uid:n$ $new_pid$ -> Rpc.Array [ Rpc.String $str:n$; $value_of_ctyp _loc new_id t$ ] >>
					) decomp in
			let pattern = mcOr_of_list patterns in
			<:expr< match $id$ with [ $pattern$ ] >>

		| <:ctyp< option $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [
				  Some $new_pid$ -> Rpc.Array [ $value_of_ctyp _loc new_id t$ ]
				| None -> Rpc.Array []
			] >> 

		| <:ctyp< $tup:tp$ >> ->
			let tys = list_of_ctyp tp [] in
			let new_ids = List.map (fun t -> let new_id, new_pid = new_id _loc in (t,new_id, new_pid)) tys in
			let exprs = List.map (fun (t,new_id,_) -> value_of_ctyp _loc new_id t) new_ids in
			let new_ids_patt = List.map (fun (_,_,new_pid) -> new_pid) new_ids in
			<:expr<
				let $patt_tuple_of_expr _loc new_ids_patt$ = $id$ in
				Rpc.Array $list_of_expr _loc exprs$
			>>

		| <:ctyp< list $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< Rpc.Array (List.map (fun $new_pid$ -> $value_of_ctyp _loc new_id t$) $id$) >>

		| <:ctyp< array $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr<
				Rpc.Array (Array.to_list (Array.map (fun $new_pid$ -> $value_of_ctyp _loc new_id t$) $id$))
			>>

		| <:ctyp< { $t$ } >> ->
			let get_name_value (n,ctyp) = <:expr< ($str:n$, $value_of_ctyp _loc <:expr< $lid:n$ >> ctyp$) >> in

			let fields = list_of_fields _loc t in
			let bindings = List.map (fun (f,_) -> <:binding< $lid:f$ = $id$ . $lid:f$ >>) fields in
			let final_expr = <:expr< Rpc.Struct $list_of_expr _loc (List.map get_name_value fields)$ >> in
			biList_to_expr _loc bindings final_expr

		| <:ctyp< $lid:t$ >> -> <:expr< $lid:"rpc_of_"^t$ $id$ >>

		| _ -> failwith "Rpc_of_ML.value_of_ctyp: type not supported"

	let rpc_of _loc id ctyp =
		let id = <:expr< $lid:id$ >> in
		value_of_ctyp _loc id ctyp

	let process _loc id ctyp =
		function_with_label_args _loc
			~fun_name:("rpc_of_"^id)
			~final_ident:id
			~function_body:(rpc_of _loc id ctyp)
			~return_type:<:ctyp< Rpc.t >>
			[]

end

(* conversion Rpc.Value.t -> ML type *)
module ML_of_rpc = struct

	let arg = let _loc = Loc.ghost in <:expr< $lid:"__x__"$ >>
	let parg = let _loc = Loc.ghost in <:patt< $lid:"__x__"$ >>

	let parse_error expected got =
		let _loc = Loc.ghost in
		<:expr< raise (Parse_error( $str:expected^" expected"$, $got$)) >>

	let rec value_of_ctyp _loc id = function
		| <:ctyp< unit >>   ->
			<:expr< match $id$ with [ Rpc.String "nil" -> unit | $parg$ -> $parse_error "String(nil)" arg$ ] >>

		| <:ctyp< int >>    ->
			<:expr< match $id$ with [ Rpc.Int x -> x | $parg$ -> $parse_error "Int(int)" arg$ ] >>

		| <:ctyp< int32 >>  ->
			<:expr< match $id$ with [ Rpc.String x -> Int32.of_string x | $parg$ -> $parse_error "String(int32)" arg$ ] >>

		| <:ctyp< int64 >>  ->
			<:expr< match $id$ with [ Rpc.String x -> Int64.of_string x | $parg$ -> $parse_error "String(int64)" arg$ ] >>

		| <:ctyp< float >>  ->
			<:expr< match $id$ with [ Rpc.Double x -> x | $parg$ -> $parse_error "Double(flaot)" arg$ ] >>

		| <:ctyp< char >>   ->
			<:expr< match $id$ with [ Rpc.String x -> x.[0] | $parg$ -> $parse_error "Char(string)" arg$ ] >>

		| <:ctyp< string >> ->
			<:expr< match $id$ with [ Rpc.String x -> x | $parg$ -> $parse_error "String(string)" arg$ ] >>

		| <:ctyp< bool >>   ->
			<:expr< match $id$ with [ Rpc.Bool x -> x | $parg$ -> $parse_error "Bool(bool)" arg$ ] >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let decomp = decompose_variants _loc t in
			let patterns =
				List.map (fun (n,t) ->
					let new_id, new_pid = new_id _loc in
					match t with
					| None ->
						<:match_case< Rpc.Array [ Rpc.String $str:n$ ] ->  $uid:n$ >>
					| Some t ->
						<:match_case< Rpc.Array [ Rpc.String $str:n$; $new_pid$ ] -> $uid:n$ $value_of_ctyp _loc new_id t$ >>
					) decomp 
				@ [ <:match_case< $parg$ -> $parse_error "Array[string;_]" arg$ >> ] in
			let pattern = mcOr_of_list patterns in
			<:expr< match $id$ with [ $pattern$ ] >>

		| <:ctyp< option $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [
				  Rpc.Array [] -> None
				| Rpc.Array [$new_pid$] -> Some $value_of_ctyp _loc new_id t$
				| $parg$ -> $parse_error "Array[_]" arg$
			] >>

		| <:ctyp< $tup:tp$ >> ->
			let tys = list_of_ctyp tp [] in
			let new_ids = List.map (fun t -> let new_id, new_pid = new_id _loc in (t,new_id,new_pid)) tys in
			let exprs = List.map (fun (t,new_id,mew_pid) -> value_of_ctyp _loc new_id t) new_ids in
			let new_ids_patt = List.map (fun (_,_,new_pid) -> new_pid) new_ids in
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [
			  Rpc.Array $new_pid$ ->
				match $new_id$ with [
				  $patt_list_of_expr _loc new_ids_patt$ -> $tuple_of_expr _loc exprs$
				| $parg$ -> $parse_error (Printf.sprintf "list of size %i" (List.length tys)) <:expr< Rpc.Array $arg$ >>$ ]
			| $parg$ -> $parse_error "Array[_]" arg$
			] >>

		| <:ctyp< list $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [
			  Rpc.Array $new_pid$ -> 
				let __fn__ $parg$ = $value_of_ctyp _loc arg t$ in
				List.map __fn__ $new_id$
			| $parg$ -> $parse_error "Array[_]" arg$
			] >>

		| <:ctyp< array $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [
			  Rpc.Array $new_pid$ ->
				let __fn__ $parg$ = $value_of_ctyp _loc arg t$ in
				Array.of_list (List.map __fn__ $new_id$)
			| $parg$ -> $parse_error "Array[_]" arg$
			] >>

		| <:ctyp< { $t$ } >> ->
			let new_id, new_pid = new_id _loc in
			let fields = list_of_fields _loc t in
			let bindings =
				List.map (fun (n,ctyp) ->
					<:binding< $lid:n$ =
						let __f__ $parg$ = $value_of_ctyp _loc arg ctyp$ in 
						__f__ (try List.assoc $str:n$ $new_id$ with [ Not_found -> $parse_error ("key "^n) id$ ])
					>>)
					fields in
			let record_bindings = List.map (fun (n,_) -> (n,<:expr< $lid:n$ >>)) fields in
			let final_expr = record_of_fields _loc record_bindings in
			<:expr< match $id$ with [
			  Rpc.Struct $new_pid$ -> $biList_to_expr _loc bindings final_expr$
			| $parg$ -> $parse_error "Struct(_)" arg$
			] >>

		| <:ctyp< $lid:t$ >> -> <:expr< $lid:t^"_of_rpc"$ $id$ >>

		| _ -> failwith "ML_of_rpc.scalar_of_ctyp: unsuported type"

	let of_rpc _loc id ctyp =
		let id = <:expr< $lid:id$ >> in
		value_of_ctyp _loc id ctyp

	let process _loc id ctyp =
		function_with_label_args _loc
			~fun_name:(id^"_of_rpc")
			~final_ident:id
			~function_body:(of_rpc _loc id ctyp)
			~return_type:<:ctyp< $lid:id$ >>
			[]

end

let process_type_declaration _loc process ctyp =
	let rec fn ty accu = match ty with
	| Ast.TyAnd (_loc, tyl, tyr)      -> fn tyl (fn tyr accu)
	| Ast.TyDcl (_loc, id, _, ty, []) -> process _loc id ty :: accu
	| _                               -> accu in
	biAnd_of_list (fn ctyp [])

let () =
	Pa_type_conv.add_generator "rpc"
		(fun ctyp ->
			let _loc = loc_of_ctyp ctyp in
			<:str_item<
		 		exception Parse_error of (string * Rpc.t);
				value rec $process_type_declaration _loc Rpc_of_ML.process ctyp$;
		 		value rec $process_type_declaration _loc ML_of_rpc.process ctyp$
		 		>>)
