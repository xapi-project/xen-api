open Rpc.Types
open Idl
open Codegen

let get_description desc =
  let escape =
    let translate = function
      (* Escape special XML (and HTML) chars that might become tags *)
      | '<' -> Some "&lt;"
      | '>' -> Some "&gt;"
      | '&' -> Some "&amp;"
      (* Escape some special markdown chars - these are not part of ocamldoc markup language *)
      | '*' | '_' | '[' | ']' | '(' | ')' | '#' | '!' as c ->
        Some ("\\" ^ (String.make 1 c))
      | _ -> None
    in
    Internals.encode translate
  in
  String.concat " " desc |> escape

let rec string_of_t : type a.a typ -> string list =
  let of_basic : type b.b basic -> string = function
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Bool -> "bool"
    | Float -> "float"
    | String -> "string"
    | Char -> "char"
  in
  let print txt = [ txt ] in
  function
  | Basic b -> print (of_basic b)
  | DateTime -> print (of_basic String)
  | Struct { sname; _ } -> print (sname)
  | Variant { vname; _ } -> print (vname)
  | Array t -> string_of_t t @ (print " list")
  | List t -> string_of_t t @ (print " list")
  | Dict (key, v) -> print (Printf.sprintf "(%s * " (of_basic key)) @ (string_of_t v) @ (print ") list");
  | Unit -> print "unit"
  | Option x -> string_of_t x @ (print " option")
  | Tuple (a, b) -> string_of_t a @ (print " * ") @ (string_of_t b)
  | Abstract a -> print "<abstract>"

let definition_of_t : type a.a typ -> string list = function
  | Struct _ -> ["struct { ... }"]
  | Variant _ -> ["variant { ... }"]
  | ty -> string_of_t ty

let rec ocaml_patt_of_t : type a. a typ -> string = fun ty ->
  let of_basic : type b.b basic -> string = function
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Bool -> "bool"
    | Float -> "float"
    | String -> "str"
    | Char -> "char"
  in
  match ty with
  | Basic b -> of_basic b
  | DateTime -> "datetime"
  | Struct s -> s.Rpc.Types.sname
  | Variant v -> "v"
  | Array t -> Printf.sprintf "%s_list" (ocaml_patt_of_t t)
  | List t -> Printf.sprintf "%s_list" (ocaml_patt_of_t t)
  | Dict (key, v) -> "dict"
  | Unit -> "()"
  | Option x -> Printf.sprintf "%s_opt" (ocaml_patt_of_t x)
  | Tuple (a, b) -> Printf.sprintf "(%s,%s)" (ocaml_patt_of_t a) (ocaml_patt_of_t b)
  | Abstract _ -> "abstract"

let rec rpc_of : type a. a typ -> string -> Rpc.t = fun ty hint ->
  Rpcmarshal.marshal ty (Rpc_genfake.gen_nice ty hint)

let table headings rows =
  (* Slightly more convenient to have columns sometimes. This
     also ensures each row has the correct number of entries. *)
  let transpose mat =
    let rec inner r =
      let safe_hd = function | hd::tl -> hd | _ -> "" in
      let safe_tl = function | hd::tl -> tl | _ -> [] in
      match r with
      | ([])::_ -> []
      | _ -> List.(map safe_hd r :: inner (map safe_tl r))
    in
    inner mat
  in

  let columns = transpose (headings::rows) in
  let all_rows = transpose columns in

  let col_widths =
    let col_width col = List.fold_left max 0 col in
    let widths = List.map String.length in
    List.map (fun col -> col_width (widths col)) columns
  in

  let pad c n s = Printf.sprintf "%s%s" s (String.make (n - String.length s) c) in
  let padfns = List.map (pad ' ') col_widths in

  let pad_row row = List.map2 (fun fn x -> fn x) padfns row in
  let padded = List.map pad_row all_rows in

  let row_to_string row = Printf.sprintf " %s " (String.concat " | " (row)) in
  let separator = Printf.sprintf "-%s-" (String.concat "-|-" (List.map (fun width -> pad '-' width "") col_widths)) in
  row_to_string (List.hd padded) :: separator :: (List.map row_to_string (List.tl padded))

let link uri text =
  Printf.sprintf "[%s](%s)" text uri

let h1 txt = [ Printf.sprintf "# %s" txt ] (*txt; String.make (String.length txt) '=' ] *)
let h2 txt = [ Printf.sprintf "## %s" txt ] (*txt; String.make (String.length txt) '-' ] *)
let h3 txt = [ Printf.sprintf "### %s" txt ]
let h4 txt = [ Printf.sprintf "#### %s" txt ]
let h5 txt = [ Printf.sprintf "##### %s" txt ]
let h6 txt = [ Printf.sprintf "###### %s" txt ]
let hrule = [ "---" ]

(* Function inputs and outputs in a table *)
let of_args args =
  let row_of_arg (is_in, Param.Boxed arg) =
    match is_in, arg.Param.typedef.ty with
    | false, Unit -> []
    | _ ->
      let name = match arg.Param.name with Some s -> s | None -> "unnamed" in
      let direction = if is_in then "in" else "out" in
      let ty = arg.Param.typedef.name in
      let description = get_description arg.Param.description in
      [name; direction; ty; description]
  in
  table ["Name"; "Direction"; "Type"; "Description"] (List.filter (fun l -> List.length l > 0) (List.map row_of_arg args))

let of_struct_fields : 'a boxed_field list -> string list = fun all ->
  let of_row (BoxedField f) =
    let ty = string_of_t f.field in
    [f.fname; String.concat "" ty; get_description f.fdescription]
  in
  table ["Name"; "Type"; "Description"] (List.map of_row all)

let of_variant_tags : 'a boxed_tag list -> string list = fun all ->
  let of_row (BoxedTag t) =
    let ty = string_of_t t.tcontents in
    [t.tname; String.concat "" ty; get_description t.tdescription]
  in
  table ["Name"; "Type"; "Description"] (List.map of_row all)

let of_type_decl i_opt ((BoxedDef t) as t') =
  if List.mem t' default_types then [] else
    let name = t.name in
    let header = [ Printf.sprintf "### %s" name ]
    in
    let example_tys = Rpc_genfake.genall 0 name t.ty in
    let marshalled = List.map (fun example -> Rpcmarshal.marshal t.ty example) example_tys in
    let example = "```json" :: (List.map (fun x -> Jsonrpc.to_string x |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string) marshalled) @  ["```"] in
    let definition =
      let defn = String.concat "" (definition_of_t t.ty) in
      let description = get_description t.description in
      [ Printf.sprintf "type `%s` = `%s`" name defn
      ; description
      ]
    in
    let rest = match t.ty with
      | Struct structure -> h4 "Members" @ of_struct_fields structure.fields
      | Variant variant -> h4 "Constructors" @ of_variant_tags variant.variants
      | _ -> [] in
    header @ example @ definition @ rest

let json_of_method namespace is i (Codegen.BoxedFunction m) =
  let inputs = Codegen.Method.find_inputs (m.Codegen.Method.ty) in
  let Idl.Param.Boxed output = Codegen.Method.find_output (m.Codegen.Method.ty) in
  let (named,unnamed) = List.fold_left (fun (named, unnamed) bp ->
      match bp with
      | Idl.Param.Boxed p -> begin
          let rpc = rpc_of p.Idl.Param.typedef.Rpc.Types.ty (match p.Idl.Param.name with | Some n -> n | None -> p.Idl.Param.typedef.Rpc.Types.name) in
          match p.Idl.Param.name with
          | Some n ->
            ((n, rpc)::named, unnamed)
          | None ->
            (named, rpc::unnamed)
        end) ([],[]) inputs
  in
  let get_wire_name name =
    match namespace with
    | Some ns -> Printf.sprintf "%s.%s" ns name
    | None -> name
  in
  let wire_name = get_wire_name m.Codegen.Method.name in
  let args =
    match named with
    | [] -> List.rev unnamed
    | _ -> (Rpc.Dict named) :: List.rev unnamed
  in
  let call = Rpc.call wire_name args in
  let input = Jsonrpc.string_of_call call |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  let example_ty = Rpc_genfake.gen_nice output.Idl.Param.typedef.Rpc.Types.ty (match output.Idl.Param.name with | Some n -> n | None -> output.Idl.Param.typedef.Rpc.Types.name) in
  let marshalled = Rpcmarshal.marshal output.Idl.Param.typedef.Rpc.Types.ty example_ty in
  let output = Jsonrpc.to_string marshalled |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string in
  (input,output)

let ocaml_of_method (Codegen.BoxedFunction m) =
  let inputs = Codegen.Method.find_inputs (m.Codegen.Method.ty) in
  let (Idl.Param.Boxed output) = Codegen.Method.find_output (m.Codegen.Method.ty) in
  let (Rpc.Types.BoxedDef error) = Codegen.Method.find_errors (m.Codegen.Method.ty) in
  let patt_of_var = function
    | Rpc.Types.BoxedTag t ->
      Printf.sprintf "%s%s" t.Rpc.Types.tname (match t.Rpc.Types.tcontents with | Unit -> "" | t -> Printf.sprintf " %s" (ocaml_patt_of_t t))
  in
  let err_pre, err_post =
    match error.Rpc.Types.ty with
    | Variant v ->
      let pre = "try\n    " in
      let post = Printf.sprintf "with %s"
          (String.concat "\n| "
             (List.map (fun v -> Printf.sprintf "Exn (%s) -> ..." (patt_of_var v)) v.Rpc.Types.variants)) in
      (pre,post)
    | _ ->
      let pre = "try\n    " in
      let post = "with _ -> ..." in
      (pre,post)
  in
  let gen_arg p =
    match p with
    | Idl.Param.Boxed p ->
      match p.Idl.Param.name with
      | Some n -> n
      | None -> p.Idl.Param.typedef.Rpc.Types.name
  in
  let result_patt =
    match output.Idl.Param.typedef.Rpc.Types.ty with
    | Unit -> "()"
    | _ ->
      match output.Idl.Param.name with
      | Some n -> n
      | None -> output.Idl.Param.typedef.Rpc.Types.name
  in
  Printf.sprintf "%slet %s = Client.%s %s in\n    ...\n%s\n"
    err_pre
    result_patt
    m.Codegen.Method.name
    (String.concat " "
       (List.map gen_arg inputs))
    err_post

(*let ocaml_server_of_method is i (Codegen.BoxedFunction m) = [
    Printf.sprintf "module S=%s(Idl.GenServerExn ())" (String.capitalize i.Interface.name);
    "";
    Printf.sprintf "let %s_impl %s =" (m.Method.name) args;
    "   let result = %s in";
    "   result";
    "";
    "let bind () =";
    "   S.%s %s_impl"
  ]*)

let tabs_of namespace is i m =
  let (json,json_response) = json_of_method namespace is i m in
  let ocaml = ocaml_of_method m in
  let python = Pythongen.example_stub_user i m |> Pythongen.string_of_ts in
  let python_server = Pythongen.example_skeleton_user i m |> Pythongen.string_of_ts in
  [ "> Client"; "";
    Printf.sprintf "```json\n%s\n```" json; "";
    Printf.sprintf "```ocaml\n%s\n```" ocaml; "";
    Printf.sprintf "```python\n%s\n```" python; "";
    "> Server"; "";
    Printf.sprintf "```json\n%s\n```" json_response; "";
    Printf.sprintf "```ocaml\n%s\n```" ocaml; "";
    Printf.sprintf "```python\n%s\n```" python_server; "";
  ]

let of_method namespace is i (Codegen.BoxedFunction m) =
  let name = m.Method.name in
  let description = get_description m.Method.description in
  h2 (Printf.sprintf "Method: `%s`" name) @
  [ description ] @ [""] @ (tabs_of namespace is i (Codegen.BoxedFunction m)) @ [""] @
  (of_args (
      List.map (fun p -> (true,p)) Method.(find_inputs m.ty) @
      [ (false, Method.(find_output m.ty)) ]))

let all_errors i =
  let errors = List.map (function (BoxedFunction m) -> Codegen.Method.find_errors m.Codegen.Method.ty) i.Interface.methods in
  let rec uniq acc errors =
    match errors with
    | e::es -> if List.mem e acc then uniq acc es else uniq (e::acc) es
    | [] -> List.rev acc
  in uniq [] errors

(** We also document the nested types that contain useful documentation *)
let expand_types is =
  (* These are the types that are helpful to document in the markdown *)
  let doc = function
    | Struct { sname; _ } as ty -> Some { name = sname; description = []; ty }
    | Variant { vname; _ } as ty -> Some { name = vname; description = []; ty }
    | _ -> None
  in
  let rec expand : type a. bool -> a typ -> boxed_def list = fun documented ty ->
    let expand ty = expand false ty in
    let defs = match ty with
      | Array ty -> expand ty
      | List ty -> expand ty
      | Dict (_, ty) -> expand ty
      | Option ty -> expand ty
      | Tuple (ty1, ty2) -> (expand ty1) @ (expand ty2)
      | Struct { fields; _ } -> List.map (function BoxedField field -> expand field.field) fields |> List.flatten
      | Variant { variants; _ } -> List.map (function BoxedTag tag -> expand tag.tcontents) variants |> List.flatten
      | _ -> []
    in
    match documented, doc ty with
    | false, Some def -> defs @ [BoxedDef def]
    | _ -> defs
  in
  let same (BoxedDef def) (BoxedDef def') = def'.name = def.name in
  (* The expanded types will be grouped together before the parameter they were
     expanded from, with later ones referencing earlier ones. The ones
     already documented earlier won't be repeated. *)
  List.fold_left
    (fun documented_defs (BoxedDef { ty; _ } as def) ->
       let expanded =
         (* Each function parameter we expand is already documented *)
         expand true ty |> List.filter (fun d -> not (same d def))
       in
       let not_documented d = not (List.exists (same d) documented_defs) in
       documented_defs @ (List.filter not_documented (expanded @ [def]))
    )
    []
    is.Interfaces.type_decls


let of_interface is i =
  let name = i.Interface.details.Idl.Interface.name in
  let namespace = i.Interface.details.Idl.Interface.namespace in
  let description = get_description i.Interface.details.Idl.Interface.description in
  h2 (Printf.sprintf "Interface: `%s`" name) @ [ description ] @ List.concat (List.map (of_method namespace is i) i.Interface.methods)


let of_interfaces x =
  let name = x.Interfaces.name in
  let description = get_description x.Interfaces.description in
  h1 name @
  [ description ] @
  h2 "Type definitions" @
  List.concat (List.map (of_type_decl None) (expand_types x)) @
  List.concat (List.map (of_interface x) x.Interfaces.interfaces) @
  h2 "Errors" @
  List.concat (List.map (of_type_decl None) (List.flatten (List.map all_errors x.Interfaces.interfaces)))

let to_string x = String.concat "\n" (of_interfaces x)
