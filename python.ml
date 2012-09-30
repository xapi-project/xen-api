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

(** [typecheck ty v] returns a python fragment which checks 
	[v] has type [ty] *)
let rec typecheck env ty v =
	let open Type in
	let open Printf in
	let raise_type_error =
		Line (sprintf "raise (TypeError(\"%s\", repr(%s)))" (Type.ocaml_of_t ty) v) in
	match ty with
		| Basic b ->
			let python_of_basic = function
				| Int64   -> "0L"
				| String  -> "\"\""
				| Double  -> "1.1"
				| Boolean -> "True" in
			[
				Line (match b with
					| Int64 -> sprintf "if not(is_long(%s)):" v
					| b     -> sprintf "if type(%s) <> type(%s):"       v (python_of_basic b)
				);
				Block [ raise_type_error ]
			]
		| Struct (hd, tl) ->
			let member (name, ty, descr) =
				typecheck env ty (sprintf "%s['%s']" v name) in
			List.concat (List.map member (hd :: tl))
		| Variant (hd, tl) ->
			let member first (name, ty, descr) =
				[ Line (sprintf "%sif isinstance(%s, %s):" (if first then "" else "el") v name);
				Block (typecheck env ty (sprintf "%s.value" v))
				] in
			(member true hd) @ (List.concat (List.map (member false) tl))
		| Array t ->
			[
				Line (sprintf "if type(%s) <> type([]):" v);
				Block [ raise_type_error ];
				Line (sprintf "for x in %s:" v);
				Block (typecheck env t "x")
			]
		| Dict (key, va) ->
			[
				Line (sprintf "if type(%s) <> type({}):" v);
				Block [ raise_type_error ];
				Line (sprintf "for x in %s.keys():" v);
				Block (typecheck env (Basic key) "x");
				Line (sprintf "for x in %s.values():" v);
				Block (typecheck env va "x")
			]
		| Name x ->
			let ident =
				if not(List.mem_assoc x env)
				then failwith (Printf.sprintf "Unable to find ident: %s" x)
				else List.assoc x env in
			typecheck env ident.Ident.ty v
		| Unit ->
			[
				Line (sprintf "if type(%s) <> type(None):" v);
				Block [ raise_type_error ]
			]
		| Option t ->
			[
				Line (sprintf "if %s <> None:" v);
				Block (typecheck env t v)
			]
		| Pair (a, b) ->
			[
				Line "# Not sure how to typecheck pairs"
			]

let rec value_of env =
	let open Type in
	let open Printf in function
		| Basic Int64 -> "0L"
		| Basic String -> "\"string\""
		| Basic Double -> "1.1"
		| Basic Boolean -> "True"
		| Struct (hd, tl) ->
			let member (name, ty, descr) =
				sprintf "\"%s\": %s" name (value_of env ty) in
			sprintf "{ %s }" (String.concat ", " (List.map member (hd :: tl)))
		| Variant (hd, tl) ->
			"None"
		| Array t ->
			sprintf "[ %s, %s ]" (value_of env t) (value_of env t)
		| Dict (key, va) ->
			sprintf "{ %s: %s }" (value_of env (Basic key)) (value_of env va)
		| Name x ->
			let ident =
				if not(List.mem_assoc x env)
				then failwith (Printf.sprintf "Unable to find ident: %s" x)
				else List.assoc x env in
			value_of env ident.Ident.ty
		| Unit ->
			"None"
		| Option t ->
			"None"
		| Pair (a, b) ->
			"[]"

let exn_decl env e =
	let open Printf in
	let rec unpair = function
		| Type.Pair(a, b) -> unpair a @ (unpair b)
		| Type.Name x -> unpair((List.assoc x env).Ident.ty)
		| t -> [ t ] in
	let args = unpair e.TyDecl.ty in
	let names = List.fold_left (fun (i, acc) _ -> (i + 1, sprintf "arg_%d" i :: acc)) (0, []) args |> snd |> List.rev in
	[
		Line (sprintf "class %s(Rpc_light_failure):" e.TyDecl.name);
		Block ([
			Line (sprintf "def __init__(self%s):" (String.concat "" (List.map (fun x -> ", " ^ x) names)));
			Block (
				[ Line (sprintf "Rpc_light_failure.__init__(self, \"%s\", [ %s ])" e.TyDecl.name (String.concat ", " names))
				] @ (List.concat (List.map (fun (ty, v) -> typecheck env ty v) (List.combine args names))
				) @ (List.map (fun v -> Line (sprintf "self.%s = %s" v v)) names)
			)
		])
	]

let skeleton_method unimplemented env i m =
	let open Printf in
	[
		Line (sprintf "def %s(self%s):" m.Method.name (String.concat "" (List.map (fun x -> ", " ^ x) (List.map (fun x -> x.Arg.name) m.Method.inputs))));
		Block ([
			Line (sprintf "\"\"\"%s\"\"\"" i.Interface.description);
		] @ (
			if unimplemented
			then [ Line (sprintf "raise Unimplemented(\"%s.%s\")" i.Interface.name m.Method.name) ]
			else ([
				Line "result = {}";
			] @ (
				List.map (fun a -> Line (sprintf "result[\"%s\"] = %s" a.Arg.name (value_of env a.Arg.ty))) m.Method.outputs
			) @ [
				Line "return result"
			])
		))
	]

let example_skeleton_user env i m =
    let open Printf in
    [
		Line "";
		Line "import xmlrpclib";
		Line "import xcp";
		Line "from storage import *";
		Line "";
		Line (sprintf "class %s_myimplementation(%s_skeleton):" i.Interface.name i.Interface.name);
		Block ([
			Line "# by default each method will return a Not_implemented error";
			Line "# ..."
		] @ (skeleton_method false env i m
		) @ [
			Line "# ..."
		]);
    ]

let rec skeleton_of_interface unimplemented suffix env i =
	let open Printf in

	[
		Line (sprintf "class %s_%s:" i.Interface.name suffix);
		Block ([
			Line (sprintf "\"\"\"%s\"\"\"" i.Interface.description);
			Line "def __init__(self):";
			Block [
				Line "pass";
			];
		] @ (
			List.concat (List.map (skeleton_method unimplemented env i) i.Interface.methods)
		))
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
			@ (typecheck env arg.Arg.ty arg.Arg.name) in
		let check_output arg =
			(* The ocaml rpc-light doesn't actually support named results, instead we
			   have single anonymous results only. *)
			typecheck env arg.Arg.ty "results" in
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
