
let ( |> ) f g = g f

module Type = struct
    (** Subset of dbus types which we'll use *)

  type basic =
    | Int64
    | String
    | Double
    | Boolean
  let basic = [
    Int64, "x";
    String, "s";
    Double, "d";
    Boolean, "b";
  ]
  let string_of_basic x = List.assoc x basic
  let basic_of_string x =
    let basic' = List.map (fun (x, y) -> y, x) basic in
    if List.mem_assoc x basic'
    then Some (List.assoc x basic')
    else None
  let ocaml_of_basic = function
    | Int64 -> "int64"
    | String -> "string"
    | Double -> "float"
    | Boolean -> "bool"

  type t =
    | Basic of basic
    | Struct of (string * t) * ((string * t) list)
    | Array of t
    | Dict of basic * t
    | Name of string

  type env = (string * t) list

  let rec string_of_t env = function
    | Basic b -> string_of_basic b
    | Struct ((_, h), tl) -> Printf.sprintf "(%s%s)" (string_of_t env h) (String.concat "" (List.map (string_of_t env) (List.map snd tl)))
    | Array x -> Printf.sprintf "a%s" (string_of_t env x)
    | Dict (k, v) -> Printf.sprintf "a{%s%s}" (string_of_basic k) (string_of_t env v)
    | Name x ->
      if not(List.mem_assoc x env)
      then failwith (Printf.sprintf "Unknown type: %s" x)
      else string_of_t env (List.assoc x env)

  let rec ocaml_of_t = function
    | Basic b -> ocaml_of_basic b
    | Struct (hd, tl) ->
      "{ " ^ (String.concat ";" (List.map (fun (name, ty) -> Printf.sprintf "%s: %s" name (ocaml_of_t ty)) (hd :: tl))) ^ " }"
    | Array t -> ocaml_of_t t ^ " list"
    | Dict (key, v) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic key) (ocaml_of_t v)
    | Name x -> x

  type ts = t list

end

module Arg = struct
  type t = {
    name: string;
    description: string;
    ty: Type.t;
  }
end

module Method = struct
  type t = {
    name: string;
    description: string;
    inputs: Arg.t list;
    outputs: Arg.t list;
  }    
end

module TyDecl = struct
  type t = {
    name: string;
    description: string;
    ty: Type.t;
  }
end

module Interface = struct
  type t = {
    name: string;
    description: string;
    type_decls: TyDecl.t list;
    methods: Method.t list;
  }
end

module Interfaces = struct
  type t = {
    name: string;
    description: string;
    type_decls: TyDecl.t list;
    interfaces: Interface.t list;
  }
end

let with_buffer f =
  let buffer = Buffer.create 128 in
  f (Buffer.add_string buffer);
  Buffer.contents buffer

module To_rpclight = struct
  let of_method m add =
    let open Format in
    let of_args name args =
      add (sprintf "@[type %s = {@." name);
      List.iter
	(fun { Arg.name = name; ty = ty } ->
	  add (sprintf "@[%s@ :@ %s;@.@]" name (Type.ocaml_of_t ty))
	) args;
      add (sprintf "@.}@.@]") in
    of_args (m.Method.name ^ "_inputs") m.Method.inputs;
    of_args (m.Method.name ^ "_outputs") m.Method.outputs;
    add (sprintf "@[external %s: %s_inputs -> %s_outputs = \"\"@.@]" m.Method.name m.Method.name m.Method.name);
    add (sprintf "@[(** %s *)@.@]" m.Method.description)

  let of_interface i add =
    let open Format in
    add (sprintf "@[module %s = struct@." i.Interface.name);
    add (sprintf "(* %s *)@." i.Interface.description);
    List.iter (fun j -> of_method j add) i.Interface.methods;
    add (sprintf "end@.@]")

  let of_interfaces i add =
    let open Format in
    add (sprintf "@[(* %s *)@." i.Interfaces.description);
    List.iter (fun j -> of_interface j add) i.Interfaces.interfaces;
    add (sprintf "@.@]")
end

let to_json x =
  let of_arg_list args =
    `Assoc (List.map (fun arg -> arg.Arg.name, `String (Type.string_of_t [] arg.Arg.ty)) args) in
  let of_interface i =
    `Assoc [
      "name", `String i.Interface.name;
      "description", `String i.Interface.description;
      "methods", 
      `List (List.map
	       (fun m ->
		 `Assoc [
		   "name", `String m.Method.name;
		   "inputs", of_arg_list m.Method.inputs;
		   "outputs", of_arg_list m.Method.outputs;
		 ]
	       ) i.Interface.methods)
    ] in
  let of_interfaces i =
    `Assoc [
      "name", `String i.Interfaces.name;
      "description", `String i.Interfaces.description;
      "interfaces", `List (List.map of_interface i.Interfaces.interfaces)
    ] in
  let json = of_interfaces x in
  Yojson.Basic.to_string json

module To_dbus = struct
  let of_arg is_in arg add =
    add (`El_start (("", "arg"), [ ("", "type"), Type.string_of_t [] arg.Arg.ty; ("", "name"), arg.Arg.name; ("", "direction"), if is_in then "in" else "out" ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data arg.Arg.description);
    add (`El_end);
    add (`El_end)

  let of_method m add =
    add (`El_start (("", "method"), [ ("", "name"), m.Method.name ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data m.Method.description);
    add (`El_end);
    List.iter
      (fun arg ->
	of_arg true arg add
      ) m.Method.inputs;
    List.iter
      (fun arg ->
	of_arg false arg add
      ) m.Method.outputs;
    add (`El_end)

  let of_interface i add =
    add (`El_start (("", "interface"), [ ("", "name"), "org.xen.xcp." ^ i.Interface.name ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data i.Interface.description);
    add (`El_end);
    List.iter
      (fun m ->
	of_method m add
      ) i.Interface.methods;
    add (`El_end)

  let of_interfaces x add =
      add (`El_start (("", "node"), [ ("", "name"), "/org/xen/xcp/" ^ x.Interfaces.name ]));
      add (`El_start (("", "tp:docstring"), []));
      add (`Data x.Interfaces.description);
      add (`El_end);
      List.iter
	(fun i ->
	  of_interface i add
	) x.Interfaces.interfaces;
      add (`El_end)

end

let with_xmlm f =
  let buffer = Buffer.create 128 in
  let output = Xmlm.make_output ~nl:true ~indent:(Some 4) (`Buffer buffer) in
  Xmlm.output output (`Dtd None);
  f (Xmlm.output output);
  Buffer.contents buffer

let to_dbus_xml x = with_xmlm (To_dbus.of_interfaces x)

let to_html x =
  let open Xmlm in
      let buffer = Buffer.create 128 in
      let output = Xmlm.make_output ~nl:true ~indent:(Some 4) (`Buffer buffer) in
      Xmlm.output output (`Dtd None);
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
      let wrapf ?cls name items =
	let attrs = match cls with
	  | None -> []
	  | Some cls -> [ ("", "class"), cls ] in
	Xmlm.output output (`El_start (("", name), attrs));
	items ();
	Xmlm.output output (`El_end) in
      let th = wrapf "th" in
      let tr = wrapf "tr" in
      let ul ?cls = wrapf ?cls "ul" in
      let li ?cls = wrapf ?cls "li" in
      let p = wrap "p" in
      let a_href link txt =
	Xmlm.output output (`El_start (("", "a"), [ ("", "href"), link ]));
	Xmlm.output output (`Data txt);
	Xmlm.output output (`El_end) in

      let of_args args =
	Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "alert alert-info" ]));
	Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped" ]));
	th (fun () -> td "Type"; td "Description");
	List.iter
	  (fun arg ->
	    tr (fun () -> td arg.Arg.name; td (Type.ocaml_of_t arg.Arg.ty); td arg.Arg.description);
	  ) args;
	Xmlm.output output (`El_end);
	Xmlm.output output (`El_end) in

      Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "container-fluid" ]));
      Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "row-fluid" ]));

      (* Side bar *)
      Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "span2" ]));
      ul ~cls:"nav nav-list" (fun () ->
	List.iter (fun t ->
	  li (fun () ->
	    a_href (Printf.sprintf "#a-%s" t.TyDecl.name) t.TyDecl.name
	  )
	) x.Interfaces.type_decls;
	List.iter (fun i ->
	  li ~cls:"nav-header" (fun () ->
	    a_href (Printf.sprintf "#a-%s" i.Interface.name) i.Interface.name
	  );
	  List.iter (fun m ->
	    li (fun () ->
	      a_href (Printf.sprintf "#a-%s" m.Method.name) m.Method.name
	    )
	  ) i.Interface.methods
	) x.Interfaces.interfaces
      );
    Xmlm.output output (`El_end);

      (* Main content *)
      Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "span10" ]));
      h1 ~id:(Printf.sprintf "a-%s" x.Interfaces.name) x.Interfaces.name;
      p x.Interfaces.description;
      List.iter
	(fun t ->
	  h2 ~id:(Printf.sprintf "a-%s" t.TyDecl.name) (Printf.sprintf "type %s" t.TyDecl.name);
	  p t.TyDecl.description;
	  pre ~lang:"ml" (Printf.sprintf "type %s = %s" t.TyDecl.name (Type.ocaml_of_t t.TyDecl.ty));
	  match t.TyDecl.ty with
	    | Type.Struct(hd, tl) ->
	      Xmlm.output output (`El_start (("", "div"), [ ("", "class"), "alert alert-info" ]));
	      Xmlm.output output (`El_start (("", "table"), [ ("", "class"), "table table-striped" ]));
	      th (fun () -> td "Type"; td "Description");
	      List.iter
		(fun (name, ty) ->
		  tr (fun () -> td name; td (Type.ocaml_of_t ty); td "foo");
		) (hd :: tl);
		Xmlm.output output (`El_end);
	      Xmlm.output output (`El_end)
	    | _ -> ()
	) x.Interfaces.type_decls;
      List.iter
	(fun i ->
	  h2 ~id:(Printf.sprintf "a-%s" i.Interface.name) i.Interface.name;
	  p i.Interface.description;
	  List.iter
	    (fun m ->
	      h3 ~id:(Printf.sprintf "a-%s" m.Method.name) m.Method.name;
	      p m.Method.description;
	      Buffer.add_string buffer
(Printf.sprintf "
          <ul id=\"tab\" class=\"nav nav-tabs\">
            <li class=\"active\"><a href=\"#defn-%s\" data-toggle=\"tab\">Definition</a></li>
            <li><a href=\"#dbus-%s\" data-toggle=\"tab\">DBUS XML</a></li>
            <li><a href=\"#ocaml-%s\" data-toggle=\"tab\">ocaml</a></li>
          </ul>
          <div id=\"myTabContent\" class=\"tab-content\">
            <div class=\"tab-pane fade in active\" id=\"defn-%s\">
" m.Method.name m.Method.name m.Method.name m.Method.name);
	      p "inputs:";
	      of_args m.Method.inputs;
	      p "outputs:";
	      of_args m.Method.outputs;
	      Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"dbus-%s\">
" m.Method.name);
	      pre (with_xmlm (To_dbus.of_method m));
	      Buffer.add_string buffer
(Printf.sprintf "
            </div>
            <div class=\"tab-pane fade\" id=\"ocaml-%s\">
" m.Method.name);
	      pre ~lang:"ml" (with_buffer (To_rpclight.of_method m));
	      Buffer.add_string buffer
(Printf.sprintf "
            </div>
          </div>
");
	    ) i.Interface.methods;
	) x.Interfaces.interfaces;
      Xmlm.output output (`El_end);
      Xmlm.output output (`El_end);
      Xmlm.output output (`El_end);
      Buffer.contents buffer


