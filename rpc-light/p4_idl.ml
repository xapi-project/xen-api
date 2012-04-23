(* Written in normal syntax, quotations in revised syntax *)

module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
	open AstFilters

	module MyRpcLight = P4_rpc.RpcLight(Ast) 

	(* We build up a list of the 'external' functions in the ml file *)    
	(* For each one, we construct the following record: *)
	type rpc = { 
		(* Location of the original definition *)
		loc : Ast.Loc.t;

		(* Capitalized module path (e.g. ["Sr"; "Foo"] for an external defined as Sr.Foo.funct *)
		namespace : string list;
 
		(* Name of the function itself, e.g. funct *)
		fname : string;
 
		(* Name of the wire call, e.g. sr_foo_name *)
		name : string;
 
		(* Arguments *)
		args : MyRpcLight.arg list;
 
		(* Return type *)
		rtype : Ast.ctyp;
		}    

	(* As well as keeping track of the namespace within the rpc record *)
	(* we keep a hierarchical data structure so that we can reconstruct *)
	(* the module tree as required later *) 
	type module_item = 
		| Rpc of rpc 
		| Namespace of string * (module_item list)

	let rec contains_rpc module_item =
	  match module_item with
	    | Rpc _ -> true
	    | Namespace (_,mis) -> List.fold_left (fun a b -> a || b) false (List.map contains_rpc mis)

	let rpc_name_of_fname_and_namespace fname namespace =
		String.concat "." (List.rev (fname::namespace))
 
	let rec return_type ctyp =
		match ctyp with 
		| <:ctyp< $x$ -> $y$ >> ->
			return_type y
		| t -> t

	let find_version cur si =
		match si with 
			| <:str_item< value version = $version$ >> ->
				let version = match version with
					| Ast.ExStr (_,version) -> version
					| _ -> failwith "Cannot parse version"
				in
				Some version
			| _ -> cur

	let add_version_rpc rpcs version =
		let _loc = Ast.Loc.ghost in
		match version with
			| None -> rpcs
			| Some _ -> 
				Rpc { loc = _loc;
				namespace = [];
				fname = "get_version";
				name = "get_version";
				args = [ { MyRpcLight.kind=`Anonymous; ctyp = <:ctyp< unit >>} ];
				rtype = <:ctyp< string >> }::rpcs

	(* Find rpcs - any 'external' definitions in the module *)
	let rec find_rpcs (cur,namespace) si = 
		match si with
		| <:str_item< module $name$ = struct $sis$ end >> ->
			let sis = Ast.list_of_str_item sis [] in
			let (rpcs,_) = List.fold_left find_rpcs ([],name::namespace) sis in
			((Namespace (name,List.rev rpcs))::cur,namespace)
		| <:str_item@loc< external $fname$ : $ctyp$ = $override$ >> ->
			let override_name = 
				match override with
				| Ast.LCons (name,Ast.LNil) -> name
				| _ -> failwith "Cannot parse external definition"
			in
			let args = MyRpcLight.args_of_ctyp (MyRpcLight.decompose_arrows ctyp) in

			let name = 
				if override_name="" 
				then rpc_name_of_fname_and_namespace fname namespace
				else override_name 
			in

			let rtype = return_type ctyp in

			let rpc = {
				loc = loc;
				namespace = List.rev namespace;
				fname = fname;
				name = name;
				args = args;
				rtype = rtype; }
			in 
			((Rpc rpc)::cur,namespace)
		| _ -> (cur,namespace)
			
	let find_exns exns si =
		match si with
			| <:str_item< exception $ctyp$ >> ->
			    begin
					match ctyp with
						| <:ctyp< $uid:uid$ of $ty$ >> ->
							(uid,Some ty)::exns
						| <:ctyp< $uid:uid$ >> ->
							(uid,None)::exns
				end
			| _ -> 
				exns

	(* Create the Args module - this contains the of_rpc and to_rpc functions
	   used to convert the arguments of the function call to Rpc.t type. It 
	   also contains converters for the return type, and a function to 
	   construct a Rpc.call record *)
	let make_args rpcs = 
		let _loc = Ast.Loc.ghost in

		let gen_call_module rpc =
			MyRpcLight.Args.gen_one 
			  (rpc.loc, rpc.namespace, rpc.fname, rpc.name, rpc.args, rpc.rtype)
		in

		let rec gen_str_items items = List.map 
			(fun item -> match item with  
	 			| Rpc r -> gen_call_module r
	 			| Namespace (n,sub_rpcs) ->
				    if contains_rpc item then 
	 				<:str_item< 
	 					module $uid:n$ = struct
	 					$list:gen_str_items sub_rpcs$
	 				end >>
				    else <:str_item< >>) items
		in	

		<:str_item< module Args = struct $list: gen_str_items rpcs$ end >>


	(* Make the client module - the signature of which is very similar to that of 
	   the original idl module, except each RPC function takes as first argument
	   an rpc function of type 'Rpc.call -> Rpc.response' *)
	let make_client rpcs = 
		let _loc = Ast.Loc.ghost in

		let create_call rpc =
			let cap_name = String.capitalize rpc.fname in
			let arg_path = MyRpcLight.arg_path rpc.loc (rpc.namespace @ [cap_name]) in
			let _loc = rpc.loc in
			<:expr< $MyRpcLight.list_foldi 
				(fun accu arg i ->
	 				let lid = MyRpcLight.argi i in
	 				match arg.MyRpcLight.kind with 
	 				| `Optional s -> <:expr< $accu$ ? $lid:s$ >>
	 				| `Named s -> <:expr< $accu$ ~ $lid:s$ >>
	 				| `Anonymous -> <:expr< $accu$ $lid:lid$ >>)
				<:expr< Args.$arg_path$.$lid:MyRpcLight.call_of rpc.fname$ >>
				rpc.args$
			>>	  
		in

		let create rpc =
			let cap_name = String.capitalize rpc.fname in
			let arg_path = MyRpcLight.arg_path rpc.loc (rpc.namespace @ [cap_name]) in
			<:expr<
				let call = $create_call rpc$ in
				let response = R.rpc call in
				if response.Rpc.success
				then Args.$arg_path$.response_of_rpc response.Rpc.contents
				else
					let e = exn_of_exnty (Exception.exnty_of_rpc response.Rpc.contents) in
					raise e >>
		in

		let gen_client_fun rpc =
			let n = List.length rpc.args - 1 in
			<:str_item< value $lid:rpc.fname$ =
				$MyRpcLight.list_foldi (fun accu arg i ->
	 				let lid = MyRpcLight.argi (n-i) in
	 				match arg.MyRpcLight.kind with 
	 				| `Optional s -> <:expr< fun ? $lid:s$ -> $accu$ >>
	 				| `Named s -> <:expr< fun ~ $lid:s$ -> $accu$ >>
	 				| `Anonymous -> <:expr< fun $lid:lid$ -> $accu$ >>)
					(create rpc)
					(List.rev rpc.args)$
			>>
		in  

		let rec gen_str_items items = List.map 
			(fun item -> match item with  
	 		| Rpc r -> gen_client_fun r
	 		| Namespace (n,sub_rpcs) -> 
			    if contains_rpc item then 
			      <:str_item< 
	 			module $uid:n$ = struct
	 				$list:gen_str_items sub_rpcs$
	 			end >> 
			    else <:str_item< >>) items
		in
		<:str_item< module Client = functor(R: RPC) -> struct $list: gen_str_items rpcs$ end >>

	(* Generates a signature for a server module - again, very similar to the original
	   idl module, but in this one a 'context' is defined - a way of passing in any
	   extra info to the functions called. This extra info is passed into the 'process'
	   function in the module generated by the functor below *)
	let make_server_sig rpcs =
		let _loc = Ast.Loc.ghost in

		let gen_server_sig r =
			let _loc = r.loc in
			<:sig_item< 
				value $lid:r.fname$ : context -> $List.fold_left 
					(fun accu arg ->
 						match arg.MyRpcLight.kind with 
 						| `Optional s -> <:ctyp< ? $lid:s$ : $arg.MyRpcLight.ctyp$ -> $accu$ >>
 						| `Named s -> <:ctyp< ~ $lid:s$ : $arg.MyRpcLight.ctyp$ -> $accu$ >>
 						| `Anonymous -> <:ctyp< $arg.MyRpcLight.ctyp$ -> $accu$ >>
					)
					r.rtype
					(List.rev r.args)$; >>
		in 
	
		let rec gen_sig_items items = List.map 
			(fun item -> match item with  
 				| Rpc r -> gen_server_sig r
 				| Namespace (n,sub_rpcs) ->
				    if contains_rpc item then 
 					<:sig_item< 
 						module $uid:n$ : sig 
 							$list:gen_sig_items sub_rpcs$
 						end >>
				    else <:sig_item< >>) items
		in

		<:str_item< module type Server_impl = sig 
			type context; 
			$list: gen_sig_items rpcs$; end >>

	(* Make the functor that generates server modules. The generated module will
	   contain a single function - 'process' - which takes an Rpc.call and 
	   unmarshals the arguments and passes them to the implementation module *)
	let make_server_functor version rpcs =
		let gen_match_case rpc =
			let _loc = rpc.loc in
			let cap_name = String.capitalize rpc.fname in
			let arg_path = Ast.ExId(_loc,MyRpcLight.arg_path rpc.loc ("Args"::rpc.namespace @ [cap_name])) in
			let impl_path = Ast.ExId(_loc,MyRpcLight.arg_path rpc.loc ("Impl"::rpc.namespace)) in

			let has_names = MyRpcLight.contains_names rpc.args in

			let pattern_list = List.rev (MyRpcLight.list_foldi
				(fun accu e i ->
			 		let lid = MyRpcLight.argi (i + 1) in
			 		match e.MyRpcLight.kind with 
			 		| `Optional s 
			 		| `Named s -> accu
			 		| `Anonymous -> <:patt< $lid:lid$ >> :: accu )
					(if has_names then [ <:patt< arg >> ] else [])
					rpc.args)
			in

			let inner = 
				let apply = MyRpcLight.list_foldi 
					(fun accu e i ->
	 					match e.MyRpcLight.kind with
	 						| `Optional s -> <:expr< $accu$ ? $lid:s$ : params.$arg_path$.$lid:s$ >>
	 						| `Named s -> <:expr< $accu$ ~ $lid:s$ : params.$arg_path$.$lid:s$ >>
	 						| `Anonymous -> <:expr< $accu$ ($arg_path$.$lid:MyRpcLight.of_rpc 
								(MyRpcLight.argi (i+1))$ $lid:MyRpcLight.argi (i+1)$) >>)
					<:expr< $impl_path$.$lid:rpc.fname$ x >> 
					rpc.args
				in
				let default = <:expr< $arg_path$.rpc_of_response ($apply$) >> in
				
				if rpc.fname = "get_version" 
				then match version with Some v -> <:expr< $arg_path$.rpc_of_response ($str:v$) >> | None -> default
				else default
				
			in
				
			let outer = 
				if has_names 
				then <:expr< let params = $arg_path$.request_of_rpc arg in $inner$ >> 
				else inner 
			in

			<:match_case< 
				  ($str:rpc.name$,$MyRpcLight.patt_list_of_list rpc.loc pattern_list$) -> 
					$outer$ 
				| ($str:rpc.name$,_) -> 
					raise (Message_param_count_mismatch ($str:rpc.name$,$`int:List.length pattern_list$,List.length call.Rpc.params))
			>>
		in
	
		let mcs = List.map gen_match_case rpcs in
		let _loc = Ast.Loc.ghost in

		<:str_item<
			module Server = functor (Impl : Server_impl) -> struct
				value process x call =
					try
						let contents = match (call.Rpc.name, call.Rpc.params) with
							[ $Ast.mcOr_of_list mcs$ 
							| (x,_) -> raise (Unknown_RPC x)]

						in { Rpc.success = True;
						     Rpc.contents = contents; }
					with
						[ e -> 
							{ Rpc.success = False;
							  Rpc.contents = Exception.rpc_of_exnty (exnty_of_exn e); } ];
			end >>


	let rec filter_types si =
		match si with
		| <:str_item< type $lid:lid$ = $body$ >> ->
		      add_rpcs si lid body 
		| <:str_item@_loc< module $foo$ = struct $sis$ end >> ->
		  <:str_item< module $foo$ = struct $list:List.map filter_types (Ast.list_of_str_item sis [])$ end>>
		| <:str_item@_loc< external $fname$ : $ctyp$ = $override$ >> ->
		  <:str_item< >>
		| <:str_item@_loc< exception $_$ >> ->
		  <:str_item< >>
		| _ -> si

	and add_rpcs si lid body = 
		let _loc = Ast.loc_of_str_item si in
		<:str_item<
			$si$;
			value $MyRpcLight.Rpc_of.gen_one (lid,[],body)$;
			value $MyRpcLight.Of_rpc.gen_one (lid,[],body)$;
		>>;;

	AstFilters.register_str_item_filter begin fun si ->
		let _loc = Ast.loc_of_str_item si in
		let version = List.fold_left find_version None (Ast.list_of_str_item si []) in

		let (orig_rev_rpcs,_) = List.fold_left find_rpcs ([],[]) (Ast.list_of_str_item si []) in

		let rev_rpcs = add_version_rpc orig_rev_rpcs version in

		let rev_exns = List.fold_left find_exns [] (Ast.list_of_str_item si []) in
		let rev_exns = 
			("Internal_error",Some <:ctyp< string >>) ::
				("Message_param_count_mismatch",Some <:ctyp< (string * int * int) >>) :: 
				("Unknown_RPC",Some <:ctyp< string >>) :: rev_exns in
		let rpcs = List.rev rev_rpcs in
		let orig_rpcs = List.rev orig_rev_rpcs in

		let rec flatten_rpcs rpcs = 
			List.flatten (List.map (function 
				| Rpc r -> [r] 
				| Namespace (rs,rpcs) -> flatten_rpcs rpcs) rpcs)
		in 
		let (tydecls,_) = List.fold_left
			(fun (decls,i) (uid,ty) ->
				let tyname = Printf.sprintf "__exn_ty%d" i in
				match ty with
					| Some ty -> 
						(<:str_item< type $lid:tyname$ = $ty$; >> :: decls , i+1)
					| None -> 
						(decls,i+1)) ([],0) rev_exns in

		let (exceptions,_) = List.fold_left
			(fun (exns,i) (uid,ty) ->
				let tyname = Printf.sprintf "__exn_ty%d" i in
				match ty with
					| Some ty ->
						(<:str_item< exception $uid:uid$ of $lid:tyname$ >> :: exns, i+1)
					| None ->
						(<:str_item< exception $uid:uid$ >> :: exns, i+1)) ([],0) rev_exns in

		let exnty = <:ctyp< [ $Ast.tyOr_of_list 
			(List.map (fun (uid,ty) -> 
				match ty with 
					| Some ty -> <:ctyp< $uid:uid$ of $ty$ >>
					| None -> <:ctyp< $uid:uid$ >>) rev_exns)$ ] >> in

		let exns = <:str_item< module Exception = struct type exnty = $exnty$ ;
			value $MyRpcLight.Rpc_of.gen_one ("exnty",[],exnty)$;
			value $MyRpcLight.Of_rpc.gen_one ("exnty",[],exnty)$; 			
		end >> in

		let exnty_of_exn = 
			let gen_mc = function | (uid,Some _) -> <:match_case< $uid:uid$ x -> Exception.$uid:uid$ x >> 
				| (uid,None) -> <:match_case< $uid:uid$ -> Exception.$uid:uid$ >> in
			let generic = <:match_case<
				e -> Exception.Internal_error (Printexc.to_string e) >> in
			let ors = List.map gen_mc rev_exns in
			<:str_item<
			value exnty_of_exn x = match x with [ $Ast.mcOr_of_list (ors @ [generic])$ ] >>
		in
		
		let exn_of_exnty = 
			let gen_mc = function | (uid,Some _) -> <:match_case< Exception.$uid:uid$ x -> $uid:uid$ x >> 
				| (uid,None) -> <:match_case< Exception.$uid:uid$ -> $uid:uid$ >> in
			let ors = List.map gen_mc rev_exns in
			<:str_item<
			value exn_of_exnty x = match x with [ $Ast.mcOr_of_list ors$ ] >>
		in
		
		let rpc_type =
			let t = <:ctyp< Rpc.call -> Rpc.response >> in
			let s = <:sig_item< value rpc: $typ:t$ >> in
			<:str_item< module type RPC = sig $sigi:s$ end >>
		in
		let flat_rpcs = flatten_rpcs rpcs in
		let sis = Ast.list_of_str_item si [] in
		<:str_item< $list: (List.map filter_types sis) @ 
			  tydecls @
			  exceptions @
			[ exns; 
			  exnty_of_exn;
			  exn_of_exnty;
			  make_args rpcs; 
			  rpc_type;
			  make_client rpcs;  
			  make_server_sig orig_rpcs;  
			  make_server_functor version flat_rpcs ] $ >>
		end

end

module Id = struct 
	let name = "idl"
	let version = "0.1"
end

let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
