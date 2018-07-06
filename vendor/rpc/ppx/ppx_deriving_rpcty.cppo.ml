open Longident
open Asttypes
open Parsetree
open Location
open Ast_helper
open Ast_convenience

let deriver = "rpcty"

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> x
  | None   -> default

(* This is for renaming fields where there's a keyword clash with ocaml *)
let attr_key  = attr_string "key"

(* This is for naming variants where there's a keyword clash with ocaml *)
let attr_name  = attr_string "name"

#if OCAML_VERSION < (4, 04, 0)
let rec split s =
  try
    let i = String.index s '\n' in
    (String.sub s 0 i) :: split (String.sub s (i+1) (String.length s - i - 1))
  with Not_found -> [s]
#else
let split = String.split_on_char '\n'
#endif

let convert_doc x = split x |> List.map (String.trim)

(* Documentation for variants / record members *)
let attr_doc attrs =
  let rpcdoc =
    Ppx_deriving.attr ~deriver "doc" attrs |>
    Ppx_deriving.Arg.(get_attr ~deriver expr) in
  let ocamldoc =
    Ppx_deriving.attr ~deriver "ocaml.doc" attrs |>
    Ppx_deriving.Arg.(get_attr ~deriver string) in
  match rpcdoc, ocamldoc with
  | Some e, _ -> e
  | _, Some s -> list (convert_doc s |> List.map str)
  | _, _ -> list []

(* Version information *)
let attr_version attrs =
  Ppx_deriving.attr ~deriver "version" attrs |> Ppx_deriving.Arg.(get_attr ~deriver expr)

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs |> Ppx_deriving.Arg.(get_attr ~deriver expr)

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

module Typ_of = struct

  let wrap_runtime decls =
    [%expr let open! Rpc.Types in [%e decls]]

  let rec expr_of_typ  typ =
    let expr =
      match typ with
      | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } when
          List.mem_assoc lid core_types -> List.assoc lid core_types
      | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
        [%expr Basic Char]
      | [%type: (string * [%t? typ]) list] ->
        [%expr Dict (String, [%e expr_of_typ typ])]
      | [%type: [%t? typ] list] ->
        [%expr List [%e expr_of_typ  typ]]
      | [%type: [%t? typ] array] ->
        [%expr Array [%e expr_of_typ  typ]]
      | {ptyp_desc = Ptyp_tuple typs } ->
        let typs = List.rev typs in
        List.fold_right (fun t acc -> [%expr Tuple ([%e expr_of_typ  t], [%e acc])]) (List.rev (List.tl typs)) [%expr [%e (expr_of_typ  (List.hd typs))] ]
      | [%type: [%t? typ] option] ->
        [%expr Option [%e expr_of_typ typ]]
      | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
        [%expr [%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "typ_of") lid))]]
      | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
        failwith "Ptyp_variant not handled"
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
    in
    wrap_runtime expr

  (*  | _ -> failwith "Error"*)

  let str_of_type ~options ~path type_decl =
    let name = type_decl.ptype_name.txt in
    let mytype = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
    let typ_of_lid = Ppx_deriving.mangle_type_decl (`Prefix "typ_of") type_decl in
    let typ_of =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        [ Vb.mk (pvar typ_of_lid) (polymorphize (expr_of_typ manifest))]
      | Ptype_record labels, _ ->
        let fields =
          let one_field = match labels with [_] -> true | _ -> false in
          labels |> List.map (fun { pld_name = { txt = fname }; pld_type; pld_attributes } ->
              let rpc_name = attr_key fname pld_attributes in
              let default = attr_default pld_attributes in
              let field_name = String.concat "_" [name; fname] in
              let fget = [%expr fun _r -> [%e Exp.field (evar "_r") (mknoloc (Lident fname)) ] ] in
              let fset = [%expr fun v _s -> [%e record [fname, [%expr v]] ?over:(if one_field then None else Some ([%expr _s]))]] in
              (fname,
               rpc_name,
               field_name,
               pld_type,
               [%expr let open Rpc.Types in
                 [%e record ["fname", str rpc_name;
                             "field", expr_of_typ pld_type;
                             "fdefault", (match default with None -> [%expr None] | Some d -> [%expr Some [%e d]]);
                             "fdescription", attr_doc pld_attributes;
                             "fversion", (match attr_version pld_attributes with | Some v -> [%expr Some [%e v]] | None -> [%expr None]);
                             "fget", fget;
                             "fset", fset;
                            ] ] ], default ))
        in
        let field_name_bindings = List.map (fun (fname, _, field_name, typ, record, _) ->
            Vb.mk (Pat.constraint_ (pvar field_name)
                     ([%type: (_, [%t mytype]) Rpc.Types.field]))
              record) fields in
        let boxed_fields = list (List.map (fun (_,_,field_name,_,_,_) ->
            [%expr BoxedField ([%e Exp.ident (lid field_name)])]) fields) in
        let construct_record = List.fold_left (fun expr (fname,rpc_name,field_name,pld_type,_,def) ->
            match def with
            | Some d ->
              [%expr (match getter.Rpc.Types.fget
                              [%e str rpc_name] [%e expr_of_typ pld_type] with
                     | Result.Ok _ as y -> y
                     | Result.Error _ -> Result.Ok [%e d])>>= fun [%p pvar field_name] -> [%e expr]]
            | None ->
              [%expr getter.Rpc.Types.fget [%e str rpc_name] [%e expr_of_typ pld_type] >>= fun [%p pvar field_name] -> [%e expr]]
          )
            [%expr return [%e Exp.record (List.map (fun (fname, _, field_name, _, _, _) ->
                mknoloc (Lident fname), evar field_name) fields) None]]
            fields in
        field_name_bindings @
        [ Vb.mk (pvar typ_of_lid)
            (polymorphize
               (wrap_runtime
                  [%expr Struct ({
                      fields=[%e boxed_fields ];
                      sname=[%e str name];
                      version=[%e match attr_version type_decl.ptype_attributes with | Some v -> [%expr Some [%e v]] | None -> [%expr None];];
                      constructor = fun getter -> let open Rresult.R in [%e construct_record]
                    } : [%t mytype ] Rpc.Types.structure)])) ]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let default_case = attr_default type_decl.ptype_attributes in
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              let rpc_name = attr_name name pcd_attributes in
              let lower_rpc_name = String.lowercase_ascii rpc_name in
              let typs = match pcd_args with
              | Pcstr_tuple(typs) -> typs
              | Pcstr_record _ ->
                raise_errorf "%s: record variants are not supported" deriver
              in
              let contents = match typs with
                | [] -> [%expr Unit]
                | typs_hd::typs_tl -> List.fold_right (fun t acc ->
                    [%expr Tuple ([%e expr_of_typ  t], [%e acc])])
                    typs_tl
                    [%expr [%e (expr_of_typ  typs_hd)]]
              in
              let args = List.mapi (fun i typ -> evar (argn i)) typs in
              let pattern = List.mapi (fun i _ -> pvar (argn i)) typs in
              let vpreview_default = if List.length constrs = 1 then [] else [Exp.case (Pat.any ()) [%expr None]] in
              let vpreview = Exp.function_ ([
                  Exp.case (pconstr name pattern) [%expr Some [%e tuple args ]];
                ] @ vpreview_default)
              in
              let vreview = Exp.function_ [Exp.case (ptuple pattern) (constr name args)] in
              let variant = [%expr BoxedTag [%e record [
                  "tname", str rpc_name;
                  "tcontents", contents;
                  "tversion", (match attr_version pcd_attributes with | Some v -> [%expr Some [%e v]] | None -> [%expr None]);
                  "tdescription", attr_doc pcd_attributes;
                  "tpreview", vpreview;
                  "treview", vreview]]] in
              let vconstructor_case = Exp.case (Pat.constant (Pconst_string (lower_rpc_name,None))) [%expr Rresult.R.bind (t.tget [%e contents]) ([%e Exp.function_ [Exp.case (ptuple pattern) [%expr Rresult.R.ok [%e (constr name args)]]]])] in
              (variant, vconstructor_case))
        in
        let default = [Exp.case (Pat.any ())
                         (match default_case with
                         | None -> [%expr Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s)]
                         | Some d -> [%expr Result.Ok [%e d]])] in
        let vconstructor = [%expr fun s' t -> let s = String.lowercase_ascii s' in [%e Exp.match_ (evar "s") ((List.map snd cases) @ default)]] in
        [ Vb.mk (pvar typ_of_lid) (wrap_runtime (polymorphize (
              [%expr Variant ({
                  vname=[%e str name ];
                  variants=([%e list (List.map fst cases)]);
                  vdefault=[%e match default_case with None -> [%expr None] | Some d -> [%expr Some [%e d]]];
                  vversion=[%e match attr_version type_decl.ptype_attributes with | Some v -> [%expr Some [%e v]] | None -> [%expr None];];
                  vconstructor=[%e vconstructor] } : [%t mytype ] variant) ]))) ]
    in
    let doc = attr_doc type_decl.ptype_attributes in
    let name = type_decl.ptype_name.txt in
    typ_of @ [Vb.mk (pvar name) (wrap_runtime (polymorphize (record ["name", str name; "description", doc; "ty", Ppx_deriving.poly_apply_of_type_decl type_decl (Exp.ident (lid typ_of_lid))])))]

end

let rpcty_strs_of_type ~options ~path type_decl =
  Typ_of.str_of_type ~options ~path type_decl

let () =
  let open Ppx_deriving in
  register
    (create deriver
       ~type_decl_str:(fun ~options ~path type_decls ->
           [Str.value Recursive
              (List.concat (List.map (rpcty_strs_of_type ~options ~path) type_decls))])
       ());
