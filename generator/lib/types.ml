
let ( |> ) f g = g f

(* Avoid linking in a whole other stdlib just for one function *)
let rec split ?limit:(limit=(-1)) c s =
  let i = try String.index s c with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i
    and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: (split ~limit: nlimit c b)

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
    | Struct of (string * t * string) * ((string * t * string) list)
    | Variant of (string * t * string) * ((string * t * string) list)
    | Array of t
    | Dict of basic * t
    | Name of string
    | Unit
    | Option of t
    | Pair of t * t
    | Custom of string

  type env = (string * t) list

  let rec dbus_of_t env = function
    | Basic b -> string_of_basic b
    | Struct (hd, tl) ->
      let member (_, h, _) = dbus_of_t env h in
      Printf.sprintf "(%s%s)" (member hd) (String.concat "" (List.map member tl))
    | Variant (hd, tl) ->
      let member (_, h, _) = dbus_of_t env h in
      Printf.sprintf "(i(%s%s))" (member hd) (String.concat "" (List.map member tl))
    | Option t -> Printf.sprintf "(b%s)" (dbus_of_t env t)
    | Array x -> Printf.sprintf "a%s" (dbus_of_t env x)
    | Dict (k, v) -> Printf.sprintf "a{%s%s}" (string_of_basic k) (dbus_of_t env v)
    | Name x ->
      if not(List.mem_assoc x env)
      then failwith (Printf.sprintf "Unknown type: %s" x)
      else dbus_of_t env (List.assoc x env)
    | Unit -> ""
    | Pair (a, b) -> dbus_of_t env a ^ (dbus_of_t env b)
    | Custom _ -> dbus_of_t env (Dict (String, Basic String))

  let rec string_of_t = function
    | Basic b -> ocaml_of_basic b
    | Struct (_, _) -> "struct  { ... }"
    | Variant (_, _) -> "variant { ... }"
    | Array t -> string_of_t t ^ " list"
    | Dict (key, v) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic key) (string_of_t v)
    | Name x -> x
    | Unit -> "unit"
    | Option x -> string_of_t x ^ " option"
    | Pair(a, b) -> string_of_t a ^ " * " ^ (string_of_t b)
    | Custom x -> x

  let rec ocaml_of_t = function
    | Basic b -> ocaml_of_basic b
    | Struct (hd, tl) ->
      "{ " ^ (String.concat ";" (List.map (fun (name, ty, descr) -> Printf.sprintf "%s: %s (** %s *)" name (ocaml_of_t ty) descr) (hd :: tl))) ^ " }"
    | Variant (hd, tl) ->
      let member (name, ty, descr) =
        Printf.sprintf "| %s (%s) (** %s *)" name (ocaml_of_t ty) descr in
      String.concat "\n" (List.map member (hd :: tl))
    | Array t -> ocaml_of_t t ^ " list"
    | Dict (key, v) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic key) (ocaml_of_t v)
    | Name x -> x
    | Unit -> "unit"
    | Option x -> string_of_t x ^ " option"
    | Pair(a, b) -> ocaml_of_t a ^ " * " ^ (ocaml_of_t b)
    | Custom x -> (String.capitalize_ascii x) ^ ".t"

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

module Ident = struct
  type t = {
    id: string; (* unique ID *)
    name: string list; (* full scope of name*)
    description: string;
    ty: Type.t;
    original_ty: Type.t; (* without names resolved to idents *)
  }
  let make =
    let counter = ref 0 in
    let next_id () =
      let result = !counter in
      incr counter;
      string_of_int result in
    fun scope name description ty original_ty ->
      { id = next_id ();
        name = name :: scope;
        description = description;
        ty = ty;
        original_ty = original_ty }

  let resolve_type idents scope =
    let open Type in
    let table = List.map (fun (_, i) -> i.name, i) idents in
    let rec aux = function
      | Basic x -> Basic x
      | Struct (hd, tl) ->
        let member (a, t, b) = a, aux t, b in
        Struct(member hd, List.map member tl)
      | Variant (hd, tl) ->
        let member (a, t, b) = a, aux t, b in
        Variant(member hd, List.map member tl)
      | Array x -> Array (aux x)
      | Dict (b, t) -> Dict (b, aux t)
      | Name x ->
        let lookhere scope =
          let x' = split '/' x in
          let name = x' @ scope in
          if List.mem_assoc name table
          then
            let ident = List.assoc name table in
            Some (ident.id)
          else
            None in
        (* XXX: this is a bit of a hack *)
        begin match lookhere scope with
          | Some x -> Type.Name x
          | None -> begin match lookhere [] with
              | Some x -> Type.Name x
              | None ->
                failwith (Printf.sprintf "Failed to resolve type: %s" (String.concat "/" (x :: scope)))
            end
        end
      | Unit -> Unit
      | Option x -> Option (aux x)
      | Pair(a, b) -> Pair (aux a, aux b)
      | Custom x -> Custom x in
    aux


end

module TyDecl = struct
  type t = {
    name: string;
    description: string;
    ty: Type.t;
  }
  let to_env x = x.name, x.ty

  let to_ident idents scope x = Ident.make scope x.name x.description (Ident.resolve_type idents scope x.ty) x.ty
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
    title: string;
    description: string;
    type_decls: TyDecl.t list;
    exn_decls: TyDecl.t list;
    interfaces: Interface.t list;
  }
end

let prepend_dbg i =
  let debug_info = {
    TyDecl.name = "debug_info";
    description = "Debug context from the caller";
    ty = Type.(Basic String)
  } in
  let dbg = {
    Arg.name = "dbg";
    description = "Debug context from the caller";
    ty = Type.(Name "debug_info")
  } in
  let of_interface i =
    let of_method m =
      {m with Method.inputs = dbg :: m.Method.inputs } in
    {i with Interface.methods = List.map of_method i.Interface.methods} in
  {i with
   Interfaces.interfaces = List.map of_interface i.Interfaces.interfaces;
   type_decls = debug_info :: i.Interfaces.type_decls
  }

(* Construct a global list of Ident.t, and convert all type_decls into
   Name <ident>. Once we have a global view of all types, we can resolve
   references in a second pass. *)
let lift_type_decls i =
  (* Indirect all defined type names via the ident table *)
  let of_type_decls scope idents type_decls =
    let idents, type_decls =
      List.fold_left
        (fun (idents, type_decls) type_decl ->
           let ident = TyDecl.to_ident idents scope type_decl in
           let type_decl = { type_decl with TyDecl.ty = Type.Name ident.Ident.id } in
           (ident.Ident.id, ident) :: idents, type_decl :: type_decls
        ) (idents, []) type_decls in
    idents, List.rev type_decls in
  (* Global scope *)
  let idents, type_decls = of_type_decls [] [] i.Interfaces.type_decls in
  let idents, exn_decls = of_type_decls [] idents i.Interfaces.exn_decls in
  let of_interface idents i =
    let idents, type_decls = of_type_decls [ i.Interface.name ] idents i.Interface.type_decls in
    idents, { i with Interface.type_decls = type_decls } in
  let idents, interfaces =
    List.fold_left
      (fun (idents, interfaces) i ->
         let idents, i' = of_interface idents i in
         (idents, i' :: interfaces)
      ) (idents, []) i.Interfaces.interfaces in
  idents, { i with Interfaces.type_decls = type_decls; exn_decls = exn_decls; interfaces = List.rev interfaces }

let dump_ident_mappings idents =
  List.iter
    (fun (_, i) ->
       Printf.printf "%10s %20s %s\n" i.Ident.id (String.concat "/" i.Ident.name) (Type.string_of_t i.Ident.ty)) idents

let resolve_references (idents: (string * Ident.t) list) i =
  let open Type in

  let of_interface i =
    let scope = [ i.Interface.name ] in
    let of_method m =
      let of_arg arg = { arg with Arg.ty = Ident.resolve_type idents scope arg.Arg.ty } in
      { m with Method.inputs = List.map of_arg m.Method.inputs;
               Method.outputs = List.map of_arg m.Method.outputs } in
    { i with Interface.methods = List.map of_method i.Interface.methods } in

  { i with Interfaces.interfaces = List.map of_interface i.Interfaces.interfaces }

let resolve_refs_in_api api =
  let idents, api = lift_type_decls api in
  dump_ident_mappings idents;
  idents, (resolve_references idents api)


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

(*
let to_json x =
  let of_arg_list args =
    `Assoc (List.map (fun arg -> arg.Arg.name, `String (Type.dbus_of_t [] arg.Arg.ty)) args) in
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
*)

module To_dbus = struct
  let of_arg env is_in arg add =
    let env = List.map (fun (x, id) -> x, id.Ident.ty) env in
    add (`El_start (("", "arg"), [ ("", "type"), Type.dbus_of_t env arg.Arg.ty; ("", "name"), arg.Arg.name; ("", "direction"), if is_in then "in" else "out" ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data arg.Arg.description);
    add (`El_end);
    add (`El_end)

  let of_method env m add =
    add (`El_start (("", "method"), [ ("", "name"), m.Method.name ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data m.Method.description);
    add (`El_end);
    List.iter
      (fun arg ->
         of_arg env true arg add
      ) m.Method.inputs;
    List.iter
      (fun arg ->
         of_arg env false arg add
      ) m.Method.outputs;
    add (`El_end)

  let of_interface env i add =
    add (`El_start (("", "interface"), [ ("", "name"), "org.xen.xcp." ^ i.Interface.name ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data i.Interface.description);
    add (`El_end);
    List.iter
      (fun m ->
         of_method env m add
      ) i.Interface.methods;
    add (`El_end)

  let of_interfaces env x add =
    add (`El_start (("", "node"), [ ("", "name"), "/org/xen/xcp/" ^ x.Interfaces.name ]));
    add (`El_start (("", "tp:docstring"), []));
    add (`Data x.Interfaces.description);
    add (`El_end);
    List.iter
      (fun i ->
         of_interface env i add
      ) x.Interfaces.interfaces;
    add (`El_end)

end

let with_xmlm f =
  let buffer = Buffer.create 128 in
  let output = Xmlm.make_output ~nl:true ~indent:(Some 4) (`Buffer buffer) in
  Xmlm.output output (`Dtd None);
  Buffer.reset buffer;
  f (Xmlm.output output);
  Buffer.contents buffer

let to_dbus_xml env x = with_xmlm (To_dbus.of_interfaces env x)

let ident_of_type_decl env t =
  (* These should have all been lifted into Idents *)
  match t.TyDecl.ty with
  | Type.Name x ->
    if not(List.mem_assoc x env)
    then failwith (Printf.sprintf "Unable to find ident: %s" x)
    else List.assoc x env
  | x ->
    failwith (Printf.sprintf "Type declaration wasn't converted into an ident: %s" (Type.string_of_t x))


