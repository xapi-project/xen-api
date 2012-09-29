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

let rec typeof ?(expand_aliases=false) env =
	let typeof env = typeof ~expand_aliases env in
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
		| Array t -> "(" ^ (typeof env t) ^ " list" ^ ")"
		| Dict (basic, t) -> sprintf "((%s * %s) list)" (typeof env (Basic basic)) (typeof env t)
		| Name x ->
			let ident =
				if not(List.mem_assoc x env)
				then failwith (Printf.sprintf "Unable to find ident: %s" x)
				else List.assoc x env in
			if expand_aliases
			then typeof env ident.Ident.ty
			else List.hd ident.Ident.name (* we assume names are all in scope *)
		| Unit -> "()"
		| Option t -> sprintf "(%s option)" (typeof env t)
		| Pair (a, b) -> sprintf "(%s * %s)" (typeof env a) (typeof env b)

let type_decl env t =
	[
		Line (sprintf "type %s = %s with rpc" t.TyDecl.name (typeof ~expand_aliases:true env t.TyDecl.ty));
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
				sprintf "Types.%s = %s" name (example_value_of env ty) in
			sprintf "{ %s }" (String.concat "; " (List.map member (hd :: tl)))
		| Variant ((first_name, first_t, _), tl) ->
			first_name ^ " " ^ (example_value_of env first_t)
		| Array t ->
			sprintf "[ %s; %s ]" (example_value_of env t) (example_value_of env t)
		| Dict (key, va) ->
			sprintf "[ (%s, %s) ]" (example_value_of env (Basic key)) (example_value_of env va)
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

let args_of_exn env e =
	let rec unpair = function
		| Type.Pair(a, b) -> unpair a @ (unpair b)
		| Type.Name x -> unpair((List.assoc x env).Ident.ty)
		| t -> [ t ] in
	unpair e.TyDecl.ty

let exn_decl env e =
	let open Printf in
	let args = args_of_exn env e in
	[
		Line (sprintf "type %s = %s with rpc" (String.lowercase e.TyDecl.name) (typeof ~expand_aliases:true env e.TyDecl.ty));
		Line (sprintf "exception %s of %s" e.TyDecl.name (String.concat " * " (List.map Type.ocaml_of_t args)));
		Line (sprintf "(** %s *)" e.TyDecl.description);
	]

let rpc_of_exns env es =
	let rpc_of_exn e =
		let args = args_of_exn env e in
		Line (Printf.sprintf "| %s(%s) as e -> Rpc.failure (Rpc.String (Printf.sprintf \"Failed to marshal: %%s\" (Printexc.to_string e)))" e.TyDecl.name (String.concat ", " (List.map (fun _ -> "_") args))) in
	[
		Line "let response_of_exn = function";
		Block (List.map rpc_of_exn es)
	]


let result_of_response env es =
	let result_of_exn e =
		let args = args_of_exn env e in
		let rec ints = function | 0 -> [] | n -> n :: (ints (n - 1)) in
		let is = List.map (fun x -> "arg_" ^ (string_of_int x)) (ints (List.length args)) in
		[
			Line (sprintf "| { Rpc.success = false; contents = Rpc.Enum [ Rpc.String \"%s\"; args ] } ->" e.TyDecl.name);
			Block [
				Line (sprintf "let %s = %s_of_rpc args in" (String.concat ", " is) (String.lowercase e.TyDecl.name));
				Line (sprintf "Result.fail (%s (%s))" e.TyDecl.name (String.concat ", " is));
			]
		] in
	[
		Line "let result_of_response = function";
		Block ([
			Line "| { Rpc.success = true; contents = t } -> Result.return t";
		] @ (List.concat (List.map result_of_exn es)
		) @ [
			Line "| { Rpc.success = false; contents = t } -> Result.fail (Xcp.Internal_error (Rpc.to_string t))";
		])
	]

let rpc_of_interfaces env is =
	let field_of_arg a = Line (sprintf "%s: %s;" a.Arg.name (typeof env a.Arg.ty)) in
	let of_method i m =
		[
			Line (sprintf "module %s = struct" (String.capitalize m.Method.name));
			Block ([
				Line "module In = struct";
				Block [
					Line "type t = {";
					Block (List.map field_of_arg m.Method.inputs);
					Line "} with rpc";
				];
				Line "end";
			]);
			Block [
				Line "module Out = struct";
				Block [
					match m.Method.outputs with
						| [ x ] ->
							Line (sprintf "type t = %s with rpc" (typeof env x.Arg.ty))
						| [] ->
							Line "type t = unit with rpc"
						| _ -> failwith (Printf.sprintf "%s.%s has output arity <> 0, 1: rpc-light can't cope" i.Interface.name m.Method.name)
				];
				Line "end";
			];
			Line "end";
		] in
	let of_call_method i m =
		[
			Line (sprintf "| \"%s.%s\", [ args ] ->" i.Interface.name m.Method.name);
			Block [
				Line (sprintf "Result.return (%s(%s.In.t_of_rpc args))" (String.capitalize m.Method.name) (String.capitalize m.Method.name));
			]
		] in
	let to_call_method i m =
		[
			Line (sprintf "| %s args -> Rpc.call \"%s.%s\" [ %s.In.rpc_of_t args ]" (String.capitalize m.Method.name) i.Interface.name m.Method.name (String.capitalize m.Method.name))
		] in
	let response_of_method i m =
		[
			Line (sprintf "| Result.Ok (%s x) ->" (String.capitalize m.Method.name));
			Block [
				Line (sprintf "Rpc.success (%s.Out.rpc_of_t x)" (String.capitalize m.Method.name))
			]
		] in
	let variant_of_interface direction env i =
		[
			Line (sprintf "module %s = struct" direction);
			Block ([
				Line "type t =";
				Block (List.map (fun m -> Line(sprintf "| %s of %s.%s.t" (String.capitalize m.Method.name) (String.capitalize m.Method.name) direction)) i.Interface.methods);
				Line "with rpc";

			] @ (
				if direction = "In" then [
					Line "let of_call (call: Rpc.call) : (t, 'b) Result.t =";
					Block [
						Line "match call.Rpc.name, call.Rpc.params with";
						Block ([
						] @ (List.concat (List.map (of_call_method i) i.Interface.methods)
						) @ [
							Line "| name, params -> Result.fail (Unknown_method name)"
						])
					];
					Line "let call_of = function";
					Block (List.concat (List.map (to_call_method i) i.Interface.methods))
				] else [
					Line "let response_of = function";
					Block ([
						Line "| Result.Error exn ->";
						Block [
							Line "Rpc.failure (Rpc.String (Printf.sprintf \"Internal_error: %s\" (Printexc.to_string exn)))";
						];
					] @ (List.concat (List.map (response_of_method i) i.Interface.methods))
					)
				]
			));
			Line "end";
		] in

	let rpc_of_interface env i =
		[
			Line (sprintf "module %s = struct" i.Interface.name);
			Block ([
			] @ (List.concat (List.map (type_decl env) i.Interface.type_decls)
			) @ (List.concat (List.map (of_method i) i.Interface.methods)
			) @ (variant_of_interface "In" env i
			) @ (variant_of_interface "Out" env i
			));
			Line "end"
		] in
	[
		Line "module Types = struct";
		Block ([
		] @ (List.concat (List.map (type_decl env) is.Interfaces.type_decls)
		) @ (List.concat (List.map (exn_decl env) is.Interfaces.exn_decls)
		) @ (List.concat (List.map (rpc_of_interface env) is.Interfaces.interfaces)
		));
		Line "end";
	]

let skeleton_method unimplemented env i m =
	let example_outputs =
		if m.Method.outputs = []
		then "return (Result.Ok ())"
		else sprintf "return (Result.Ok (Types.%s.%s.Out.(%s)))" i.Interface.name (String.capitalize m.Method.name)
		(String.concat "; " (List.map (fun a -> sprintf "%s" (example_value_of env a.Arg.ty)) m.Method.outputs)) in
	let unimplemented_error =
		sprintf "return (Result.Error (Unimplemented \"%s.%s\"))" i.Interface.name m.Method.name in
	[
		Line (sprintf "let %s x =" m.Method.name);
		Block [
			Line (if unimplemented then unimplemented_error else example_outputs)
		]
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
		Line (sprintf "module %s_%s = functor(M: Xcp.M) -> struct" i.Interface.name suffix);
		Block ([
			Line "include M";
		] @ (List.concat (List.map (skeleton_method unimplemented env i) i.Interface.methods))
		);
		Line "end";
	]

let signature_of_interface env i =
	let signature_of_method m =
		Line (sprintf "val %s: Types.%s.%s.In.t -> (Types.%s.%s.Out.t, exn) Result.t t"
			m.Method.name
			i.Interface.name (String.capitalize m.Method.name)
			i.Interface.name (String.capitalize m.Method.name)
		) in
	[
		Line (sprintf "module type %s = sig" i.Interface.name);
		Block ([
			Line "include Xcp.M";
		] @ (List.map signature_of_method i.Interface.methods
		));
		Line "end";
	]

let test_impl_of_interface = skeleton_of_interface false "test"
let skeleton_of_interface = skeleton_of_interface true "skeleton"

let server_of_interface env i =
	let dispatch_method m =
		[
			Line (sprintf "| Types.%s.In.%s x ->" i.Interface.name (String.capitalize m.Method.name));
			Block [
				Line (sprintf "Impl.%s x" m.Method.name);
				Line ">>= fun result ->";
				Line "return (Result.(>>=) result (fun ok ->";
				Block [
					Line (sprintf "Result.return (Types.%s.Out.%s ok)" i.Interface.name (String.capitalize m.Method.name));
				];
				Line "))";
			]
		] in
	[
		Line (sprintf "module %s_server_dispatcher = functor(Impl: %s) -> struct" i.Interface.name i.Interface.name);
		Block [
			Line "type 'a t = 'a Impl.t";
			Line "let (>>=) = Impl.(>>=)";
			Line "let return = Impl.return";
			Line (sprintf "let dispatch (request: Types.%s.In.t) : (Types.%s.Out.t, 'b) Result.t Impl.t = match request with" i.Interface.name i.Interface.name);
			Block (List.concat (List.map dispatch_method i.Interface.methods));
			Line "let rpc (call: Rpc.call) : Rpc.response Impl.t =";
			Block [
				Line (sprintf "match Types.%s.In.of_call call with" i.Interface.name);
				Block [
					Line "| Result.Ok x ->";
					Block [
						Line "dispatch x >>= fun result ->";
						Line (sprintf "return (Types.%s.Out.response_of result)" i.Interface.name);
					];
					Line "| Result.Error e ->";
					Block [
						Line "return (response_of_exn e)";
					];
				];
			]
		];
		Line "end"
	]

let client_of_interfaces env is =
	let client_of_method env i m =
		[
			Line (sprintf "let %s_r x =" m.Method.name);
			Block [
				Line (sprintf "let call = Types.%s.In.call_of (Types.%s.In.%s x) in" i.Interface.name i.Interface.name (String.capitalize m.Method.name));
				Line "RPC.rpc call >>= fun response ->";
				Line (sprintf "let result = Result.(>>=) (result_of_response response) (fun x -> Result.return (Types.%s.%s.Out.t_of_rpc x)) in" i.Interface.name (String.capitalize m.Method.name));
				Line "return result";
			];
			Line (sprintf "let %s %s =" m.Method.name (String.concat " " (List.map (fun i -> sprintf "~%s" i.Arg.name) m.Method.inputs)));
			Block [
				Line (sprintf "let r = Types.%s.%s.In.({ %s }) in" i.Interface.name (String.capitalize m.Method.name) (String.concat "; " (List.map (fun i -> sprintf "%s = %s" i.Arg.name i.Arg.name) m.Method.inputs)));
				Line (sprintf "%s_r r" m.Method.name);
			]
		] in
	let client_of_interface env i =
		[
			Line (sprintf "module %s_client = functor(RPC: RPC) -> struct" i.Interface.name);
			Block ([
				Line "open RPC";
			] @ (List.concat (List.map (client_of_method env i) i.Interface.methods))
			);
			Line "end";
		] in
	List.concat (List.map (client_of_interface env) is.Interfaces.interfaces)

let of_interfaces env i =
	let open Printf in
	[
		Line "(* Automatically generated code - DO NOT MODIFY *)";
		Line "";
		Line "open Xcp";
		Line "";
	] @ (
		List.concat (List.map (exn_decl env) i.Interfaces.exn_decls)
	) @ (
		rpc_of_exns env i.Interfaces.exn_decls
	) @ (
		result_of_response env i.Interfaces.exn_decls
	) @ (
		rpc_of_interfaces env i
	) @ (
		List.concat (List.map (signature_of_interface env) i.Interfaces.interfaces)
	) @ (
		List.fold_left (fun acc i -> acc @
			(server_of_interface env i) @ (skeleton_of_interface env i) @ (test_impl_of_interface env i)
		) [] i.Interfaces.interfaces
	) @ (
		client_of_interfaces env i
	)
