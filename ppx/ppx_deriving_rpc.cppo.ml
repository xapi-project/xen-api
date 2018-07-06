open Longident
open Asttypes
open Parsetree
open Location
open Ast_helper
open Ast_convenience

let deriver = "rpc"

let argn = Printf.sprintf "a%d"

(* For these types we have convertors in rpc.ml *)
let core_types = List.map (fun (s, y) -> (Lident s, y))
    ["unit", [%expr Unit];
     "int", [%expr Basic Int];
     "int32", [%expr Basic Int32];
     "int64", [%expr Basic Int64];
     "string", [%expr Basic String];
     "float", [%expr Basic Float];
     "bool", [%expr Basic Bool]]

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs |> Ppx_deriving.Arg.(get_attr ~deriver expr)

(* [is_option typ] returns true if the type 'typ' is an option type.
   This is required because of the slightly odd way we serialise records containing optional fields. *)
let is_option typ =
  match typ with
  | [%type: [%t? typ] option] -> true
  | _ -> false


(* When marshalling (foo * bar) lists we check to see whether it can be better represented by a
   dictionary - we do this by checking (possibly at run time) whether the 'foo' can be unmarshalled from
   a string - this following function, given the type 'foo', returns the run time check *)
let is_string typ =
  match typ with
  | [%type: string] -> [%expr true]
  | [%type: int] -> [%expr false]
  | [%type: bool] -> [%expr false]
  | { ptyp_desc = Ptyp_constr ( { txt = lid }, [] ) } ->
    [%expr let open Rpc in try (let _ = [%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~fixpoint:"" (`Suffix "of_rpc") lid)) ] (Rpc.String "") in true) with _ -> false]
  | _ -> [%expr false]

(* Retrieve a string attribute from the annotation. For example: given the type declaration:
 *
 *      type x = {
 *        f5: int [@key "type"];
 *      }
 *
 *  calling 'attr_string 'key' default attributes' will return 'type'
 *)
let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> x
  | None   -> default

(* This is for renaming fields where there's a keyword clash with ocaml *)
let attr_key  = attr_string "key"

(* This is for naming variants where there's a keyword clash with ocaml *)
let attr_name  = attr_string "name"

(* Documentation for variants / record members *)
let attr_doc = attr_string "doc"

let is_dict attr = match Ppx_deriving.attr ~deriver "dict" attr with Some _ -> [%expr true] | None -> [%expr false]

module Of_rpc = struct

  let rec expr_of_typ typ =
    match typ with
    | { ptyp_desc = Ptyp_constr ( { txt = Lident id as lid }, args ) } when
        List.mem_assoc lid core_types ->
      Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~fixpoint:"" (`Suffix "of_rpc") (Ldot (Lident "Rpc", id))))

    | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
      [%expr Rpc.char_of_rpc ]

    (* Tuple lists might be representable by a dictionary, if the first type in the tuple is string-like *)
    | { ptyp_desc = Ptyp_constr ({txt = Lident "list"}, [{ptyp_desc = Ptyp_tuple [typ1; typ2]}]); ptyp_attributes } -> [%expr
      if [%e is_dict ptyp_attributes] || [%e is_string typ1]
      then
        function
        | Rpc.Dict l -> Rpcmarshal.tailrec_map (fun (k,v) -> ([%e expr_of_typ typ1] (Rpc.String k),[%e expr_of_typ typ2] v)) l
        | y -> failwith (Printf.sprintf "Expecting Rpc.Dict, but found '%s'" (Rpc.to_string y))
      else
        function
        | Rpc.Enum l -> Rpcmarshal.tailrec_map
          (function | Rpc.Enum [k;v] -> ([%e expr_of_typ typ1] k,[%e expr_of_typ typ2] v)
                    | y -> failwith (Printf.sprintf "Expecting Rpc.Enum (within an Enum), but found '%s'" (Rpc.to_string y))) l
        | y -> failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y)) ]

    | [%type: [%t? typ] list] -> [%expr
      function
      | Rpc.Enum l -> Rpcmarshal.tailrec_map [%e expr_of_typ typ] l
      | y -> failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y)) ]

    | [%type: [%t? typ] array] -> [%expr
      function
      | Rpc.Enum l -> Rpcmarshal.tailrec_map [%e expr_of_typ typ] l |> Array.of_list
      | y -> failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y)) ]

    | {ptyp_desc = Ptyp_tuple typs } ->
      let pattern = List.mapi (fun i _ -> pvar (argn i)) typs in
      let exprs = List.mapi (fun i typ -> [%expr [%e expr_of_typ typ] [%e evar (argn i) ] ] ) typs in
      [%expr
        function
        | Rpc.Enum [%p plist pattern] -> [%e tuple exprs]
        | y -> failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]

    | [%type: [%t? typ] option] ->
      let e = expr_of_typ typ in
      [%expr
        function
        | Rpc.Enum [] -> None
        | Rpc.Enum [y] -> Some ([%e e] y)
        | y -> failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]

    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
      let args = List.map expr_of_typ args in
      let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~fixpoint:"" (`Suffix "of_rpc") lid)) in
      app f args

    | { ptyp_desc = Ptyp_var name } ->
      [%expr [%e evar ("poly_"^name)]]

    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; ptyp_attributes } ->
      let inherits, tags = List.partition (function Rinherit _ -> true | _ -> false) fields in
      let bad = [%expr failwith "Unknown tag/contents"] in
      let default_expr =
        match attr_default ptyp_attributes with
        | None -> bad
        | Some expr -> [%expr match rpc' with | String _ | Enum ((String _) :: _) -> [%e expr ] | _ -> [%e bad]] in
      let tag_cases =
        tags |> List.map (fun field ->
            match field with
            | Rtag (label, attrs, true, []) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              let label' = String.lowercase_ascii label in
              Exp.case
                [%pat? Rpc.String [%p pstr (attr_name label' attrs)]]
                (Exp.variant label None)
            | Rtag (label, attrs, false, [ { ptyp_desc = Ptyp_tuple typs }]) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              let label' = String.lowercase_ascii label in
              let exprs = List.mapi (fun i typ -> [%expr [%e expr_of_typ typ] [%e evar (argn i) ] ] ) typs in
              Exp.case
                [%pat? Rpc.Enum [Rpc.String [%p pstr (attr_name label' attrs)];
                                 Rpc.Enum [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)]]]
                (Exp.variant label (Some (tuple exprs)))
            | Rtag (label, attrs, false, [typ]) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              let label' = String.lowercase_ascii label in
              Exp.case
                [%pat? Rpc.Enum [Rpc.String [%p pstr (attr_name label' attrs)]; y]]
                [%expr [%e expr_of_typ typ] y |> fun x ->
                       [%e Exp.variant label (Some [%expr x])]]
            | _ ->
              raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                deriver (Ppx_deriving.string_of_core_type typ))
      and inherits_case =
        (*let toplevel_typ = typ in*)
        inherits |>
        List.map (function Rinherit typ -> typ | _ -> assert false) |>
        List.fold_left (fun expr typ ->
            [%expr
              try [%e expr_of_typ typ] rpc (*  :> [%t toplevel_typ]*)
              with _ -> [%e expr]]) default_expr |>
        Exp.case [%pat? _]
      in
      [%expr fun (rpc : Rpc.t) ->
             let rpc' = match rpc with
               | Rpc.Enum ((Rpc.String x)::xs) -> Rpc.Enum ((Rpc.String (String.lowercase_ascii x))::xs)
               | Rpc.String x -> Rpc.String (String.lowercase_ascii x)
               | y -> y in
             [%e Exp.match_ [%expr rpc'] (tag_cases @ [inherits_case])]]

    | { ptyp_desc = Ptyp_any } ->
      failwith "Ptyp_any not handled"

    | { ptyp_desc = Ptyp_poly (_, _) } ->
      failwith "Ptyp_poly not handled"

    | { ptyp_desc = Ptyp_extension _ } ->
      failwith "Ptyp_extension not handled"

    | { ptyp_desc = Ptyp_arrow (_, _, _) } ->
      failwith "Ptyp_arrow not handled"

    | { ptyp_desc = Ptyp_object (_, _) } ->
      failwith "Ptyp_object not handled"

    | { ptyp_desc = Ptyp_alias (_, _) } ->
      failwith "Ptyp_alias not handled"

    | { ptyp_desc = Ptyp_class (_, _) } ->
      failwith "Ptyp_class not handled"

    | { ptyp_desc = Ptyp_package _ } ->
      failwith "Ptyp_package not handled"

  let str_of_type ~options ~path type_decl =
    let of_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        expr_of_typ manifest
      | Ptype_record labels, _ ->
        let record =
          List.fold_left (fun expr (i,label) ->
              let { pld_name = { txt = name }; pld_attributes } = label in
              let key = String.lowercase_ascii (attr_key name pld_attributes) in
              [%expr let [%p pvar (argn i)] = match [%e evar (argn i)] with | Some x -> x | None -> failwith (Printf.sprintf "Undefined field: Expecting '%s'" [%e str key]) in [%e expr]])
            [%expr [%e Exp.record (labels |> List.mapi (fun i { pld_name = { txt = name } } ->
                mknoloc (Lident name), evar (argn i))) None]]
            (labels |> List.mapi (fun i label -> (i,label))) in
        let wrap_opt pld_type x =
          if is_option pld_type then [%expr (Rpc.Enum [[%e x]])] else x in
        let cases =
          (labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
               let thunks = labels |> List.mapi (fun j _ ->
                   if i = j
                   then [%expr Some [%e app (expr_of_typ pld_type) [(wrap_opt pld_type (evar "x"))]]]
                   else evar (argn j)) in
               Exp.case [%pat? ([%p pstr (String.lowercase_ascii (attr_key name pld_attributes))], x) :: xs]
                 [%expr loop xs [%e tuple thunks]])) @
          [Exp.case [%pat? []] record;
           Exp.case [%pat? _ :: xs] [%expr loop xs _state]]
        and thunks =
          labels |> List.map (fun { pld_name = { txt = name }; pld_type; pld_attributes } ->
              if is_option pld_type
              then [%expr Some None]
              else [%expr None])
        in
        [%expr fun x ->
          match x with
          | Rpc.Dict dict ->
            let d' = List.map (fun (k,v) -> (String.lowercase_ascii k, v)) dict in
            let rec loop xs ([%p ptuple (List.mapi (fun i _ -> pvar (argn i)) labels)] as _state) =
              [%e Exp.match_ [%expr xs] cases]
            in loop d' [%e tuple thunks]
          | y -> failwith (Printf.sprintf "Expecting Rpc.Dict, but found '%s'" (Rpc.to_string y))]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              match pcd_args with
              | Pcstr_tuple(typs) ->
                let subpattern = List.mapi (fun i _ -> pvar (argn i)) typs |> plist in
                let exprs = List.mapi (fun i typ -> [%expr [%e expr_of_typ typ] [%e evar (argn i) ] ] ) typs in
                let rpc_of = constr name exprs in
                let main = [%pat? Rpc.String [%p pstr (String.lowercase_ascii (attr_name name pcd_attributes))]] in
                let pattern = match typs with
                  | [] -> main
                  | _ -> [%pat? Rpc.Enum ([%p main] :: [%p subpattern])]
                in
                Exp.case pattern rpc_of
              | Pcstr_record _ ->
                raise_errorf "%s: record variants are not supported" deriver
          ) in
        let default =
          Exp.case
            [%pat? y]
            [%expr failwith
                (Printf.sprintf "Unhandled pattern when unmarshalling variant type: found '%s'"
                   (Rpc.to_string y))]
        in
        [%expr fun rpc ->
          let rpc' = Rpc.lowerfn rpc in
          [%e Exp.function_ (cases@[default]) ] rpc' ]
    in of_rpc
end


module Rpc_of = struct


  let rec expr_of_typ typ =
    match typ with
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } when
        List.mem_assoc lid core_types ->
      [%expr Rpc.([%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~fixpoint:"" (`Prefix "rpc_of") lid))])]
    | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
      [%expr Rpc.(function c -> Rpc.Int (Int64.of_int (Char.code c)))]

    (* Tuple lists might be representable by a dictionary, if the first type in the tuple is string-like *)
    | { ptyp_desc = Ptyp_constr ({txt = Lident "list"}, [{ptyp_desc = Ptyp_tuple [typ; typ2]}]); ptyp_attributes } -> [%expr
      if [%e is_dict ptyp_attributes] || [%e is_string typ]
      then fun l -> Rpc.Dict (List.map (fun (k,v) -> (Rpc.string_of_rpc ([%e expr_of_typ typ] k),[%e expr_of_typ typ2] v)) l)
      else fun l -> Rpc.Enum (List.map (fun (a,b) -> Rpc.Enum [[%e expr_of_typ typ] a; [%e expr_of_typ typ2] b]) l)]

    | [%type: [%t? typ] list] ->
      [%expr fun l -> Rpc.Enum (Rpcmarshal.tailrec_map [%e expr_of_typ typ] l)]
    | [%type: [%t? typ] array] -> [%expr fun l -> Rpc.Enum (Rpcmarshal.tailrec_map [%e expr_of_typ  typ] (Array.to_list l))]
    | {ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ  typ) [evar (argn i)]) typs in
      [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
             Rpc.Enum [%e list args]]
    | [%type: [%t? typ] option] ->
      let e = expr_of_typ  typ in
      [%expr fun x -> match x with None -> Rpc.Enum [] | Some y -> Rpc.Enum [ [%e e] y ] ]
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
      let args = List.map (expr_of_typ ) args in
      let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid ~fixpoint:"" (`Prefix "rpc_of") lid)) in
      app f args
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
            match field with
            | Rtag (label, attrs, true, []) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              Exp.case
                (Pat.variant label None)
                [%expr Rpc.String [%e str (attr_name label attrs)]]
            | Rtag (label, attrs, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              let l = list (List.mapi (fun i typ -> app (expr_of_typ  typ) [evar (argn i)]) typs) in
              Exp.case
                (Pat.variant label (Some (ptuple (List.mapi (fun i _ -> pvar (argn i)) typs))))
                [%expr Rpc.Enum ( Rpc.String ([%e str (attr_name label attrs)]) ::
                                  [Rpc.Enum [%e l]])]
            | Rtag (label, attrs, false, [typ]) ->
#if OCAML_VERSION > (4, 05, 0)
              let label = label.txt in
#endif
              Exp.case
                (Pat.variant label (Some [%pat? x]))
                [%expr Rpc.Enum ( (Rpc.String ([%e str (attr_name label attrs)])) :: [ [%e expr_of_typ  typ] x])]
            | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
              Exp.case
                [%pat? [%p Pat.type_ tname] as x]
                [%expr [%e expr_of_typ  typ] x]
            | _ ->
              raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                deriver (Ppx_deriving.string_of_core_type typ))
      in
      Exp.function_ cases

    | { ptyp_desc = Ptyp_any } ->
      failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_var name } ->
      [%expr [%e evar ("poly_"^name)]]
    | { ptyp_desc = Ptyp_poly (_, _) } ->
      failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _ } ->
      failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _) } ->
      failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _) } ->
      failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _) } ->
      failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _) } ->
      failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _ } ->
      failwith "Ptyp_package not handled"
  (*  | _ -> failwith "Error"*)


  let str_of_type ~options ~path type_decl =
    let to_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        expr_of_typ  manifest
      | Ptype_record labels, _ ->
        let fields =
          labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
              let rpc_name = attr_key name pld_attributes in
              if is_option pld_type
              then
                [%expr let rpc = [%e (expr_of_typ  pld_type)] [%e Exp.field (evar "x") (mknoloc (Lident name))] in
                       match rpc with
                       | Rpc.Enum [x] -> Some ([%e str rpc_name], x)
                       | Rpc.Enum [] -> None
                       | _ -> failwith (Printf.sprintf "Programmer error when marshalling %s.%s" [%e str type_decl.ptype_name.txt] [%e str name]) (* Should never happen *)
                ]
              else
                [%expr Some ([%e str rpc_name],
                             [%e (expr_of_typ  pld_type)] [%e Exp.field (evar "x") (mknoloc (Lident name))])]) in

        [%expr fun x -> Rpc.Dict (List.fold_right (fun x acc -> match x with | Some x -> x::acc | None -> acc) [%e list fields] []) ]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              match pcd_args with
              | Pcstr_tuple(typs) ->
                let args = List.mapi (fun i typ -> [%expr [%e expr_of_typ  typ] [%e evar (argn i)]]) typs in
                let argsl = list args in
                let pattern = List.mapi (fun i _ -> pvar (argn i)) typs in
                let rpc_of = match args with
                  | [] -> [%expr Rpc.String [%e str (attr_name name pcd_attributes)]]
                  | args -> [%expr Rpc.Enum ((Rpc.String [%e str (attr_name name pcd_attributes)]) :: [%e argsl])]
                in
                Exp.case (pconstr name pattern) rpc_of
              | Pcstr_record _ ->
                raise_errorf "%s: record variants are not supported" deriver
          ) in
        Exp.function_ cases
    in
    to_rpc


end



let rpc_strs_of_type ~options ~path type_decl =
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let rpc_of = Ppx_deriving.mangle_type_decl ~fixpoint:"" (`Prefix "rpc_of") type_decl in
  let of_rpc = Ppx_deriving.mangle_type_decl ~fixpoint:"" (`Suffix "of_rpc") type_decl in
  [
    Vb.mk (pvar rpc_of)
      (Exp.fun_ Label.nolabel None (pvar ("__x__")) [%expr [%e (polymorphize (Rpc_of.str_of_type ~options ~path type_decl))] __x__]);
    Vb.mk (pvar of_rpc)
      (Exp.fun_ Label.nolabel None (pvar ("__x__")) [%expr [%e (polymorphize (Of_rpc.str_of_type ~options ~path type_decl))] __x__]);
  ]




let () =
  let open Ppx_deriving in
  register
    (create deriver
       ~core_type: (Rpc_of.expr_of_typ)
       ~type_decl_str:(fun ~options ~path type_decls ->
           [Str.value Recursive
              (List.concat (List.map (rpc_strs_of_type ~options ~path) type_decls))])
       ());
