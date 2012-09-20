open Types

type t =
	| Block of t list
	| Line of string

let rec lines_of_t t =
	let indent = String.make 4 ' ' in
	match t with
		| Line x -> [ x ]
		| Block xs ->
	let all = List.concat (List.map lines_of_t xs) in
	List.map (fun x -> indent ^ x) all

let string_of_ts ts = String.concat "\n" (List.concat (List.map lines_of_t ts))

open Printf

let rec typeof env =
	let open Type in function
		| Basic Int64 -> "int64"
		| Basic String -> "string"
		| Basic Double -> "float"
		| Basic Boolean -> "bool"
		| Struct (fst, rest) ->
			let member (name, ty, descr) = sprintf "%s: %s; (** %s *)" name (typeof env ty) descr in
			"{ " ^ (member fst) ^ (String.concat " " (List.map member rest)) ^ " }"
		| Variant (fst, rest) ->
			let member (name, ty, descr) = sprintf "| %s of %s (** %s *)" name (typeof env ty) descr in
			member fst ^ (String.concat " " (List.map member rest))
		| Array t -> typeof env t ^ " list"
		| Dict (basic, t) -> sprintf "(%s * %s) list" (typeof env (Basic basic)) (typeof env t)
		| Name x ->
			let ident =
				if not(List.mem_assoc x env)
				then failwith (Printf.sprintf "Unable to find ident: %s" x)
				else List.assoc x env in
			typeof env ident.Ident.ty
		| Unit -> "()"
		| Option t -> sprintf "%s option" (typeof env t)
		| Pair (a, b) -> sprintf "(%s * %s)" (typeof env a) (typeof env b)

let type_decl env t =
	[
		Line (sprintf "type %s = %s" t.TyDecl.name (typeof env t.TyDecl.ty));
		Line (sprintf "(** %s *)" t.TyDecl.description);
	]

let rec example_value_of env =
	let open Type in function
		| Basic Int64 -> "0L"
		| Basic String -> "\"string\""
		| Basic Double -> "1.1"
		| Basic Boolean -> "true"
		| Struct (hd, tl) ->
			let member (name, ty, descr) =
				sprintf "%s = %s" name (example_value_of env ty) in
			sprintf "{ %s }" (String.concat "; " (List.map member (hd :: tl)))
		| Variant ((first_name, first_t, _), tl) ->
			first_name ^ " " ^ (example_value_of env first_t)
		| Array t ->
			sprintf "[ %s; %s ]" (example_value_of env t) (example_value_of env t)
		| Dict (key, va) ->
			sprintf "(%s, %s)" (example_value_of env (Basic key)) (example_value_of env va)
		| Name x ->
			let ident =
				if not(List.mem_assoc x env)
				then failwith (Printf.sprintf "Unable to find ident: %s" x)
				else List.assoc x env in
			example_value_of env ident.Ident.ty
		| Unit ->
			"()"
		| Option t ->
			"Some " ^ (example_value_of env t)
		| Pair (a, b) ->
			Printf.sprintf "(%s, %s)" (example_value_of env a) (example_value_of env b)

let exn_decl env e =
	let open Printf in
	let rec unpair = function
		| Type.Pair(a, b) -> unpair a @ (unpair b)
		| Type.Name x -> unpair((List.assoc x env).Ident.ty)
		| t -> [ t ] in
	let args = unpair e.TyDecl.ty in
	[
		Line (sprintf "exception %s of %s" e.TyDecl.name (String.concat " * " (List.map Type.ocaml_of_t args)));
		Line (sprintf "(** %s *)" e.TyDecl.description);
	]

let skeleton_method unimplemented env i m =
	[
		Line (sprintf "let %s x = %s" m.Method.name
			(if unimplemented
			then (sprintf "raise (Unimplemented \"%s.%s\")" i.Interface.name m.Method.name)
			else "failwith \"need example method outputs\"")
		)
	]

let example_skeleton_user env i m =
    let open Printf in
    [
		Line "";
		Line (sprintf "module %s_myimplementation = struct" i.Interface.name);
		Block [
			Line (sprintf "include %s_skeleton" i.Interface.name);
			Line "...";
			Block (skeleton_method false env i m);
			Line "...";
		];
		Line "end"
    ]

let skeleton_of_interface unimplemented suffix env i =
	[
		Line (sprintf "module %s_%s = struct" i.Interface.name suffix);
		Block (List.concat (List.map (skeleton_method unimplemented env i) i.Interface.methods));
		Line "end";
	]



let test_impl_of_interface = skeleton_of_interface false "test"
let skeleton_of_interface = skeleton_of_interface true "skeleton"

let server_of_interface env i =
    let open Printf in
    let typecheck_method_wrapper m =
		let extract_input arg =
			[ Line (sprintf "if not(args.has_key('%s')):" arg.Arg.name);
			Block [ Line (sprintf "raise UnmarshalException('argument missing', '%s', '')" arg.Arg.name) ];
			Line (sprintf "%s = args[\"%s\"]" arg.Arg.name arg.Arg.name) ]
			(* @ (typecheck env arg.Arg.ty arg.Arg.name) *) in
		let check_output arg =
			(* The ocaml rpc-light doesn't actually support named results, instead we
			   have single anonymous results only. *)
			(* typecheck env arg.Arg.ty "results" *) [] in
		[
			Line (sprintf "def %s(self, args):" m.Method.name);
			Block ([
				Line "\"\"\"type-check inputs, call implementation, type-check outputs and return\"\"\"";
				Line "if type(args) <> type({}):";
				Block [
					Line "raise (UnmarshalException('arguments', 'dict', repr(args)))"
				]
			] @ (
				List.concat (List.map extract_input m.Method.inputs)
			) @ [
				Line (sprintf "results = self._impl.%s(%s)" m.Method.name (String.concat ", " (List.map (fun x -> x.Arg.name) m.Method.inputs)))
			] @ (
				List.concat (List.map check_output m.Method.outputs)
			) @ [
				Line "return results"
			])
		] in    
	let dispatch_method first m =
		[ Line (sprintf "%sif method == \"%s.%s\":" (if first then "" else "el") i.Interface.name m.Method.name);
		Block [ Line (sprintf "return success(self.%s(args))" m.Method.name) ]
		] in
	let first_is_special f xs = match xs with
		| [] -> []
		| x :: xs -> f true x :: (List.map (f false) xs) in
	[
		Line (sprintf "class %s_server_dispatcher:" i.Interface.name);
		Block ([
			Line (sprintf "\"\"\"%s\"\"\"" i.Interface.description);
			Line "def __init__(self, impl):";
			Block [
				Line "\"\"\"impl is a proxy object whose methods contain the implementation\"\"\"";
				Line "self._impl = impl";
			];
		] @ (List.concat (List.map typecheck_method_wrapper i.Interface.methods)
		) @ [
			Line "def _dispatch(self, method, params):";
			Block ([
				Line "\"\"\"type check inputs, call implementation, type check outputs and return\"\"\"";
				Line "args = params[0]";
			] @ (List.concat (first_is_special dispatch_method i.Interface.methods)))
		])
	]

let test_impl_of_interfaces env i =
	let open Printf in
	[
		Line (sprintf "class %s_server_test(%s_server_dispatcher):" i.Interfaces.name i.Interfaces.name);
		Block [
			Line "\"\"\"Create a server which will respond to all calls, returning arbitrary values. This is intended as a marshal/unmarshal test.\"\"\"";
			Line "def __init__(self):";
			Block [
				Line (sprintf "%s_server_dispatcher.__init__(self%s)" i.Interfaces.name (String.concat "" (List.map (fun i -> ", " ^ i.Interface.name ^ "_server_dispatcher(" ^ i.Interface.name ^ "_test())") i.Interfaces.interfaces)))
			]
		]
	]

let of_interfaces env i =
	let open Printf in
	[
		Line "from xcp import *";
		Line "import traceback";
	] @ (
		List.concat (List.map (type_decl env) i.Interfaces.type_decls)
	) @ (
		List.concat (List.map (exn_decl env) i.Interfaces.exn_decls)
	) @ (
		List.fold_left (fun acc i -> acc @
			(server_of_interface env i) @ (skeleton_of_interface env i) @ (test_impl_of_interface env i)
		) [] i.Interfaces.interfaces
	) @ [
		Line (sprintf "class %s_server_dispatcher:" i.Interfaces.name);
		Block ([
			Line "\"\"\"Demux calls to individual interface server_dispatchers\"\"\"";
			Line (sprintf "def __init__(self%s):" (String.concat "" (List.map (fun x -> ", " ^ x ^ " = None") (List.map (fun i -> i.Interface.name) i.Interfaces.interfaces))));
			Block (List.map (fun i -> Line (sprintf "self.%s = %s" i.Interface.name i.Interface.name)) i.Interfaces.interfaces);
			Line "def _dispatch(self, method, params):";
			Block [
				Line "try:";
				Block ([
					Line "log(\"method = %s params = %s\" % (method, repr(params)))";
				] @ (
					List.fold_left (fun (first, acc) i -> false, acc @ [
						Line (sprintf "%sif method.startswith(\"%s\") and self.%s:" (if first then "" else "el") i.Interface.name i.Interface.name);
						Block [ Line (sprintf "return self.%s._dispatch(method, params)" i.Interface.name) ];
					]) (true, []) i.Interfaces.interfaces |> snd
				) @ [
					Line "raise UnknownMethod(method)"
				]
				);
				Line "except Exception, e:";
				Block [
					Line "log(\"caught %s\" % e)";
					Line "traceback.print_exc()";
					Line "try:";
					Block [
						Line "# A declared (expected) failure will have a .failure() method";
						Line "log(\"returning %s\" % (repr(e.failure())))";
						Line "return e.failure()"
					];
					Line "except:";
					Block [
						Line "# An undeclared (unexpected) failure is wrapped as InternalError";
						Line "return (InternalError(str(e)).failure())"
					]
				]
			]
		])
	] @ (test_impl_of_interfaces env i)
