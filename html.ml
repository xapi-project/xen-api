open Files
open Types

type page = {
	name: string;
	title: string;
	filename: string;
	path: string;
	description: string;
	api: Interfaces.t;
}

let to_string env x =
	let open Xmlm in
    let buffer = Buffer.create 128 in
    let output = Xmlm.make_output ~nl:true ~indent:(Some 4) (`Buffer buffer) in
    Xmlm.output output (`Dtd None);
    Buffer.reset buffer;
    let wrap ?id name body =
		let attrs = match id with
			| None -> []
			| Some id -> [ ("", "id"), id ] in
		Xmlm.output output (`El_start (("", name), attrs));
		Xmlm.output output (`Data body);
		Xmlm.output output (`El_end) in
    let h1 ?id = wrap ?id "h1" in
    let h2 ?id = wrap ?id "h2" in
    let h3 ?id = wrap ?id "h3" in
    let td = wrap "td" in
    let pre ?lang txt =
		let cls = match lang with
			| None -> "prettyprint"
			| Some x -> Printf.sprintf "prettyprint lang-%s" x in
		Xmlm.output output (`El_start (("", "pre"), [ ("", "class"), cls ]));
		Xmlm.output output (`Data txt);
		Xmlm.output output (`El_end) in
    let code ?id = wrap ?id "code" in
    let wrapf ?cls name items =
		let attrs = match cls with
			| None -> []
			| Some cls -> [ ("", "class"), cls ] in
		Xmlm.output output (`El_start (("", name), attrs));
		items ();
		Xmlm.output output (`El_end) in
    let tdcode x = wrapf "td" (fun () -> code x) in
    let th = wrapf "th" in
    let tr = wrapf "tr" in
    let ul ?cls = wrapf ?cls "ul" in
    let li ?cls = wrapf ?cls "li" in
    let p = wrap "p" in
    let a_href ?toggle ?icon link txt =
		let toggle = match toggle with
			| None -> []
			| Some x -> [ ("", "data-toggle"), x ] in
		Xmlm.output output (`El_start (("", "a"), [ ("", "href"), link ] @ toggle));
		begin match icon with | None -> () | Some icon ->
			Xmlm.output output (`El_start (("", "i"), [ ("", "class"), icon ]));
			Xmlm.output output (`Data "");
			Xmlm.output output (`El_end)
		end;
	
		Xmlm.output output (`Data txt);
		Xmlm.output output (`El_end) in

    let rec html_of_t =
		let open Type in
		let print txt = Xmlm.output output (`Data txt) in
		function
			| Basic b -> print (ocaml_of_basic b)
			| Struct (_, _) -> print ("struct  { ... }")
			| Variant (_, _) -> print ("variant { ... }")
			| Array t -> html_of_t t; print " list"
			| Dict (key, v) -> print (Printf.sprintf "(%s * " (ocaml_of_basic key)); html_of_t v; print ") list";
			| Name x ->
				let ident =
					if not(List.mem_assoc x env)
					then failwith (Printf.sprintf "Unable to find ident: %s" x)
					else List.assoc x env in
				a_href (Printf.sprintf "#a-%s" x) (String.concat "/" ident.Ident.name)
			| Unit -> print "unit"
			| Option x -> html_of_t x; print " option"
			| Pair(a, b) -> html_of_t a; print " * "; html_of_t b in

    let of_args args =
		Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "alert alert-info" ]));
		Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped" ]));
		th (fun () -> td "Direction"; td "Type"; td "Description");
		List.iter
			(fun (is_in, arg) ->
				tr (fun () ->
					tdcode arg.Arg.name;
					td (if is_in then "in" else "out");
					wrapf "td"
						(fun () ->
							wrapf "code"
								(fun () ->
									html_of_t arg.Arg.ty;
								)
						);
					td arg.Arg.description);
			) args;
		Xmlm.output output (`El_end);
		Xmlm.output output (`El_end) in

    Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "container-fluid" ]));
    Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "row-fluid" ]));

    (* Side bar *)
    Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "span3" ]));
    Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "well sidebar-nav" ]));
    ul ~cls:"nav nav-list" (fun () ->
		List.iter (fun t ->
			let ident = ident_of_type_decl env t in
			li (fun () ->
				a_href (* ~toggle:"tab" *) ~icon:"icon-pencil" (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
			)
		) x.Interfaces.type_decls;
		List.iter (fun i ->
			li ~cls:"nav-header" (fun () ->
				a_href (* ~toggle:"tab" *) ~icon:"icon-book" (Printf.sprintf "#a-%s" i.Interface.name) i.Interface.name
			);
			List.iter (fun t ->
				let ident = ident_of_type_decl env t in
				li (fun () ->
					a_href (* ~toggle:"tab" *) ~icon:"icon-pencil" (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
				)
			) i.Interface.type_decls;
			List.iter (fun m ->
				li (fun () ->
					a_href (* ~toggle:"tab" *) ~icon:"icon-cogs" (Printf.sprintf "#a-%s" m.Method.name) m.Method.name
				)
			) i.Interface.methods
		) x.Interfaces.interfaces;
		li ~cls:"nav-header" (fun () ->
			a_href "#a-exceptions" "Exceptions";
		);
(*
	List.iter (fun t ->
	  li (fun () ->
	    let ident = ident_of_type_decl env t in
	    a_href (Printf.sprintf "#a-%s" ident.Ident.id) t.TyDecl.name
	  )
	) x.Interfaces.exn_decls;
*)
    );
	Xmlm.output output (`El_end);
	Xmlm.output output (`El_end);

	let of_struct_variant_fields all =
		Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped table-condensed" ]));
		th (fun () -> td "Type"; td "Description");
		List.iter
			(fun (name, ty, descr) ->
				tr (fun () ->
					tdcode name;
					tdcode (Type.ocaml_of_t ty);
					td descr
				)
			) all;
		Xmlm.output output (`El_end) in
    let of_type_decl t =
		let ident = ident_of_type_decl env t in
		h2 ~id:(Printf.sprintf "a-%s" ident.Ident.id) (Printf.sprintf "type %s = %s" t.TyDecl.name (Type.string_of_t ident.Ident.ty));
		p t.TyDecl.description;
		match ident.Ident.original_ty with
			| Type.Struct(hd, tl) ->
				p "Members:";
				of_struct_variant_fields (hd :: tl)
			| Type.Variant(hd, tl) ->
				p "Constructors:";
				of_struct_variant_fields (hd :: tl)
			| _ -> () in

    (* Main content *)
    Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "span9" ]));
    h1 ~id:(Printf.sprintf "a-%s" x.Interfaces.name) (Printf.sprintf "%s: %s" x.Interfaces.name x.Interfaces.title);
    p x.Interfaces.description;
    List.iter of_type_decl x.Interfaces.type_decls;
    List.iter
		(fun i ->
			h2 ~id:(Printf.sprintf "a-%s" i.Interface.name) i.Interface.name;
			p i.Interface.description;
			List.iter of_type_decl i.Interface.type_decls;
			List.iter
				(fun m ->
					h3 ~id:(Printf.sprintf "a-%s" m.Method.name) m.Method.name;
					p m.Method.description;
					Buffer.add_string buffer
						(Printf.sprintf "
          <ul id=\"tab\" class=\"nav nav-tabs\">
            <li><a href=\"#defn-%s\" data-toggle=\"tab\">Definition</a></li>
            <li class=\"active\"><a href=\"#ocaml-%s\" data-toggle=\"tab\">ocaml client</a></li>
            <li><a href=\"#ocaml-server-%s\" data-toggle=\"tab\">ocaml server</a></li>
            <li><a href=\"#python-client-%s\" data-toggle=\"tab\">python client</a></li>
            <li><a href=\"#python-server-%s\" data-toggle=\"tab\">python server</a></li>
            <li><a href=\"#dbus-%s\" data-toggle=\"tab\">DBUS XML</a></li>
          </ul>
          <div id=\"myTabContent\" class=\"tab-content\">
            <div class=\"tab-pane fade\" id=\"defn-%s\">
" m.Method.name m.Method.name m.Method.name m.Method.name m.Method.name m.Method.name m.Method.name);
					of_args (List.map (fun m -> true, m) m.Method.inputs @ (List.map (fun m -> false, m) m.Method.outputs));
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"dbus-%s\">
" m.Method.name);
					pre (with_xmlm (To_dbus.of_method env m));
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade in active\" id=\"ocaml-%s\">
" m.Method.name);
					Buffer.add_string buffer (Ocaml.caml2html (Ocaml.example_stub env x i m |> Ocaml.string_of_ts));
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"ocaml-server-%s\">
" m.Method.name);
					Buffer.add_string buffer (Ocaml.caml2html (Ocaml.example_skeleton_user env x i m |> Ocaml.string_of_ts));
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"python-client-%s\">
" m.Method.name);
					pre ~lang:"python" (Python.example_stub_user env i m |> Python.string_of_ts);
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"python-server-%s\">
" m.Method.name);
					pre ~lang:"python" (Python.example_skeleton_user env i m |> Python.string_of_ts);
					Buffer.add_string buffer
(Printf.sprintf "
            </div>
          </div>
");
				) i.Interface.methods;
		) x.Interfaces.interfaces;

    h1 ~id:"a-exceptions" "Exceptions";

    Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped table-condensed" ]));
    th (fun () -> td "Parameter Types"; td "Description");
    List.iter
		(fun t ->
			let ident = ident_of_type_decl env t in
			tr (fun () ->
				tdcode (String.concat "/" ident.Ident.name);
				tdcode (Type.ocaml_of_t ident.Ident.original_ty);
				td ident.Ident.description
			)
		) x.Interfaces.exn_decls;
    Xmlm.output output (`El_end);

    Xmlm.output output (`El_end);
    Xmlm.output output (`El_end);
    Xmlm.output output (`El_end);
    Buffer.contents buffer


module Html = struct

	let div cls = `El_start (("", "div"), [ ("", "class"), cls ])
	let a cls toggle target = `El_start (("", "a"), [ ("", "class"), cls; ("", "data-toggle"), toggle; ("", "data-target"), target ])
	let span cls = `El_start (("", "span"), [ ("", "class"), cls ])
	let ul cls = `El_start (("", "ul"), [ ("", "class"), cls ])
	let li cls = `El_start (("", "li"), match cls with None -> [] | Some x -> [ ("", "class"), x ])
	let h1 = `El_start (("", "h1"), [])
	let h2 = `El_start (("", "h2"), [])
	let p = `El_start (("", "p"), [])
	let endtag = `El_end
end

let html_navbar oc pages this_page =
	let open Html in
	let txt = Types.with_xmlm
		(fun xmlm ->
			xmlm (div "navbar navbar-fixed-top");
			xmlm (div "navbar-inner");
			xmlm (div "container");
			xmlm (a "btn btn-navbar" "collapse" ".nav-collapse");
			xmlm (span "icon-bar"); xmlm endtag;
			xmlm (span "icon-bar"); xmlm endtag;
			xmlm (span "icon-bar"); xmlm endtag;
			xmlm endtag;
			xmlm (`El_start (("", "a"), [ ("", "class"), "brand"; ("", "href"), "index.html"]));
			xmlm (`Data "XCP Host APIs");
			xmlm endtag;
			xmlm (div "nav-collapse");
			xmlm (ul "nav");
			List.iter
				(fun page ->
					xmlm (li (if Some page = this_page then Some "active" else None));
					xmlm (`El_start (("", "a"), [ ("", "href"), page.filename ]));
					xmlm (`Data page.name);
					xmlm endtag;
					xmlm endtag
				) pages;
			xmlm endtag;
			xmlm endtag;
			xmlm endtag;
			xmlm endtag;
			xmlm endtag
		) in
	output_string oc txt

let index_html oc pages =
	let open Html in
	let txt = Types.with_xmlm
		(fun xmlm ->
			xmlm (div "container");
			xmlm (div "hero-unit");
			xmlm h1;
			xmlm (`Data "XCP Host APIs");
			xmlm endtag;
			xmlm p;
			xmlm (`Data "This site contains prototype API definitions and example code fragments for interoperating with services running on an XCP host.");
			xmlm endtag; (* p *)
			xmlm (`El_start (("", "a"), [("", "class"), "btn btn-primary btn-large"; ("", "href"), "https://github.com/xen-org"]));
			xmlm (`El_start (("", "i"), [("", "class"), "icon-github-sign icon-large"]));
			xmlm (`Data "");
			xmlm endtag; (* i *)
			xmlm (`Data "View Project on GitHub");
			xmlm endtag; (* a *)
			xmlm endtag; (* hero-unit *)
			(* Make rows of 3 elements each *)
			let rec make_rows = function
				| a :: b :: c :: rest -> [a; b; c] :: (make_rows rest)
				| a :: b :: [] -> [[a; b]]
				| a :: [] -> [[a]]
				| [] -> [] in
			List.iter
				(fun row ->
					xmlm (div "row");
					List.iter
						(fun page ->
							xmlm (div "span4");
							xmlm h2;
							xmlm (`Data page.title);
							xmlm endtag;
							xmlm p;
							xmlm (`Data page.description);
							xmlm endtag;
							xmlm p;
							xmlm (`El_start (("", "a"), [("", "class"), "btn btn-info"; ("", "href"), page.filename]));
							xmlm (`Data "View details");
							xmlm endtag;
							xmlm endtag;
							xmlm endtag;
						) row;
					xmlm endtag;
				) (make_rows pages);
			xmlm endtag (* container *)
		) in
	print_file_to oc ("doc/header.html");
	html_navbar oc pages None;
	output_string oc txt;
	print_file_to oc ("doc/footer.html")

let page_of_api api = {
    name = api.Interfaces.name;
	title = api.Interfaces.title;
	path = "doc/" ^ api.Interfaces.name ^ ".html";
	filename = api.Interfaces.name ^ ".html";
	description = api.Interfaces.description;
	api = api;
}

let write apis =
	let pages = List.map page_of_api apis in

	List.iter
		(fun page ->
			with_output_file page.path
				(fun oc ->
					print_file_to oc ("doc/header.html");
(*					html_navbar oc pages (Some page); *)
					let idents, api = Types.resolve_refs_in_api page.api in
					output_string oc (to_string idents api);
					print_file_to oc ("doc/footer.html")
				);
		) pages;
	with_output_file "doc/index.html"
		(fun oc ->
			index_html oc pages
		)
