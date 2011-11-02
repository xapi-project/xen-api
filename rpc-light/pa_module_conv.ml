open Camlp4
open PreCast
open Ast
open Syntax

(* Map of "with_gen"-generators for modules signature *)
let generators = Hashtbl.create 0

(* Check that there is no argument for generators that do not expect arguments *)
let no_arg id e typ arg =
	if arg = None then
		e typ
	else
		failwith ("Pa_type_conv: generator '" ^ id ^ "' does not expect an argument")

(* Parse a list of tokens with the given grammar entry *)
let parse_with entry = function
	| Some tokens -> Some (Gram.parse_tokens_after_filter entry (Stream.of_list tokens))
	| None -> None

(* Entry which ignores its input *)
let ignore_tokens = Gram.Entry.of_parser "ignore_tokens" ignore

(* Add new generator, fail if already defined *)
let safe_add_gen gens id entry e =
	if Hashtbl.mem gens id then
		failwith ("Pa_type_conv: generator '" ^ id ^ "' defined multiple times")
	else Hashtbl.add gens id (fun typ arg -> e typ (parse_with entry arg))


(* Register a "with"-generator for types in structures *)
let add_generator_with_arg ?(is_exn = false) id entry e =
	safe_add_gen generators id entry e

let add_generator ?is_exn id e =
	add_generator_with_arg ?is_exn id ignore_tokens (no_arg id e)

(* Functions for interpreting derivation types *)
let generate tp (drv_id, drv_arg) =
	try Hashtbl.find generators drv_id tp drv_arg
	with Not_found ->
		failwith ("Pa_module_type_conv: '" ^ drv_id ^ "' is not a supported type generator.")

let gen_derived_defs _loc tp drvs =
	let coll drv der_sis = <:str_item< $der_sis$; $generate tp drv$ >> in
	List.fold_right coll drvs <:str_item< >>

let rec fetch_generator_arg paren_count strm =
	match Stream.next strm with
	| KEYWORD "(", _ -> fetch_generator_arg (paren_count + 1) strm
	| KEYWORD ")", token_info ->
		if paren_count = 1 then [(EOI, token_info)]
		else fetch_generator_arg (paren_count - 1) strm
	| EOI, token_info -> Loc.raise (Gram.token_location token_info) (Stream.Error "')' missing")
	| x -> x :: fetch_generator_arg paren_count strm

let generator_arg =
	Gram.Entry.of_parser "generator_arg"
		(fun strm ->
			match Stream.peek strm with
			| Some(KEYWORD "(", _) ->
				Stream.junk strm;
				Some (fetch_generator_arg 1 strm)

			| _ -> None)

EXTEND Gram

GLOBAL: str_item;

	generator: [[ id = LIDENT; arg = generator_arg -> (id, arg) ]];

	str_item: AFTER "top" [[
		"module"; "type"; name = a_UIDENT; "="; md = module_type; "with_gen"; drvs = LIST1 generator SEP ","  ->
			<:str_item<
				module type $uid:name$ = $md$;
				$gen_derived_defs _loc md drvs$
			>>
	]];
END


