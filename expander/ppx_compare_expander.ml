(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)


(* Note: I am introducing a few unnecessary explicit closures, (not all of them some are
   unnecessary due to the value restriction).
*)

open Base
open Ppxlib
open Ast_builder.Default

module Attrs = struct
  let ignore =
    Attribute.declare "compare.ignore"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
end

let str_attributes = [
  Attribute.T Attrs.ignore
]

let with_tuple loc ~value ~tys f =
  (* generate
     let id_1, id_2, id_3, ... id_n = value in expr
     where expr is the result of (f [id_1, ty_1 ; id_2, ty_2; ...])
  *)
  let names_types = List.map tys
                      ~f:(fun t -> gen_symbol ~prefix:"t" (), t) in
  let pattern =
    let l = List.map names_types ~f:(fun (n, _) -> pvar ~loc n) in
    ppat_tuple ~loc l
  in
  let e = f (List.map names_types ~f:(fun (n,t) -> (evar ~loc n, t))) in
  let binding  = value_binding ~loc ~pat:pattern ~expr:value in
  pexp_let ~loc Nonrecursive [binding] e

let phys_equal_first a b cmp =
  let loc = cmp.pexp_loc in
  [%expr
    if Ppx_compare_lib.phys_equal [%e a] [%e b] then 0 else [%e cmp]
  ]

let rec chain_if ~loc = function
  | [] -> [%expr 0]
  | [x] -> x
  | x :: xs ->
    let loc = x.pexp_loc in
    [%expr
      match [%e x] with
      | 0 -> [%e chain_if ~loc xs]
      | n -> n
    ]

let tp_name n = Printf.sprintf "_cmp__%s" n

let compare_type ~loc ty =
  [%type: [%t ty] -> [%t ty] -> int]

let equal_type ~loc ty =
  [%type: [%t ty] -> [%t ty] -> bool]

let function_name = function
  | "t" -> "compare"
  | s -> "compare_" ^ s

let rec compare_applied ~constructor ~args value1 value2 =
  let args = List.map args ~f:(compare_of_ty_fun ~type_constraint:false) @ [value1; value2] in
  type_constr_conv ~loc:(Located.loc constructor) constructor args
    ~f:function_name

and compare_of_tuple loc tys value1 value2 =
  with_tuple loc ~value:value1 ~tys (fun elems1 ->
    with_tuple loc ~value:value2 ~tys (fun elems2 ->
      let exprs = List.map2_exn elems1 elems2 ~f:(fun (v1, t) (v2, _) ->
        compare_of_ty t v1 v2)
      in
      chain_if ~loc exprs))

and compare_variant loc row_fields value1 value2 =
  let map = function
    | Rtag ({ txt = cnstr; _ }, _attrs, true, _) | Rtag ({ txt = cnstr; _ }, _attrs, _, []) ->
      case ~guard:None
        ~lhs:(ppat_tuple ~loc
                [ppat_variant ~loc cnstr None; ppat_variant ~loc cnstr None])
        ~rhs:(eint ~loc 0)
    | Rtag ({ txt = cnstr; _ }, _attrs, false, tp :: _) ->
      let v1 = gen_symbol ~prefix:"_left" ()
      and v2 = gen_symbol ~prefix:"_right" () in
      let body = compare_of_ty tp (evar ~loc v1) (evar ~loc v2) in
      case ~guard:None
        ~lhs:(ppat_tuple ~loc [ ppat_variant ~loc cnstr (Some (pvar ~loc v1))
                              ; ppat_variant ~loc cnstr (Some (pvar ~loc v2))
                              ])
        ~rhs:body
    | Rinherit { ptyp_desc = Ptyp_constr (id, args); _ } ->
      (* quite sadly, this code doesn't handle:
         type 'a id = 'a with compare
         type t = [ `a | [ `b ] id ] with compare
         because it will generate a pattern #id, when id is not even a polymorphic
         variant in the first place.
         The culprit is caml though, since it only allows #id but not #([`b] id)
      *)
      let v1 = gen_symbol ~prefix:"_left" ()
      and v2 = gen_symbol ~prefix:"_right" () in
      case ~guard:None
        ~lhs:(ppat_tuple ~loc [ ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v1)
                              ; ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v2)
                              ])
        ~rhs:(compare_applied ~constructor:id ~args (evar ~loc v1) (evar ~loc v2))
    | Rinherit ty ->
      Location.raise_errorf ~loc:ty.ptyp_loc "Ppx_compare.compare_variant: unknown type"
  in
  let e =
    let matched = pexp_tuple ~loc [value1; value2] in
    match List.map ~f:map row_fields with
    | [v] -> pexp_match ~loc matched [v]
    | l     ->
      pexp_match ~loc matched
        (l @
         (* Providing we didn't screw up badly we now know that the tags of the variants
            are different. We let pervasive do its magic. *)
         [ case ~guard:None ~lhs:[%pat? (x, y)]
             ~rhs:[%expr Ppx_compare_lib.polymorphic_compare x y] ])
  in
  phys_equal_first value1 value2 e

and branches_of_sum cds =
  let rightmost_index = (List.length cds - 1) in
  List.concat
    (List.mapi cds ~f:(fun i cd ->
       let rightmost = i = rightmost_index in
       let loc = cd.pcd_loc in
       if Option.is_some cd.pcd_res then
         Location.raise_errorf ~loc "GADTs are not supported by comparelib";
       match cd.pcd_args with
       | Pcstr_record lds ->
         let value1 = gen_symbol ~prefix:"_a" () in
         let value2 = gen_symbol ~prefix:"_b" () in
         let res =
           case ~guard:None
             ~lhs:(ppat_tuple ~loc [ pconstruct cd (Some (pvar ~loc value1))
                                   ; pconstruct cd (Some (pvar ~loc value2))
                                   ])
             ~rhs:(compare_of_record_no_phys_equal loc lds (evar ~loc value1) (evar ~loc value2))
         in
         if rightmost then
           [ res ]
         else
           let pany = ppat_any ~loc in
           let pcnstr = pconstruct cd (Some pany) in
           let case l r n =
             case ~guard:None ~lhs:(ppat_tuple ~loc [l; r]) ~rhs:(eint ~loc n)
           in
           [ res
           ; case pcnstr pany   (-1)
           ; case pany   pcnstr 1
           ]
       | Pcstr_tuple pcd_args ->
         match pcd_args with
         | [] ->
           let pcnstr = pconstruct cd None in
           let pany = ppat_any ~loc in
           let case l r n =
             case ~guard:None ~lhs:(ppat_tuple ~loc [l; r]) ~rhs:(eint ~loc n)
           in
           if rightmost then
             [ case pcnstr pcnstr 0 ]
           else
             [ case pcnstr pcnstr 0
             ; case pcnstr pany   (-1)
             ; case pany pcnstr   1
             ]
         | tps ->
           let ids_ty =
             List.map tps
               ~f:(fun ty ->
                 let a = gen_symbol ~prefix:"_a" () in
                 let b = gen_symbol ~prefix:"_b" () in
                 (a, b, ty))
           in
           let lpatt = List.map ids_ty ~f:(fun (l,_r,_ty) -> pvar ~loc l) |> ppat_tuple ~loc
           and rpatt = List.map ids_ty ~f:(fun (_l,r,_ty) -> pvar ~loc r) |> ppat_tuple ~loc
           and body =
             List.map ids_ty ~f:(fun (l,r,ty) ->
               compare_of_ty ty (evar ~loc l) (evar ~loc r))
             |> chain_if ~loc
           in
           let res =
             case ~guard:None
               ~lhs:(ppat_tuple ~loc [ pconstruct cd (Some lpatt)
                                     ; pconstruct cd (Some rpatt)
                                     ])
               ~rhs:body
           in
           if rightmost then
             [ res ]
           else
             let pany = ppat_any ~loc in
             let pcnstr = pconstruct cd (Some pany) in
             let case l r n =
               case ~guard:None ~lhs:(ppat_tuple ~loc [l; r]) ~rhs:(eint ~loc n)
             in
             [ res
             ; case pcnstr pany   (-1)
             ; case pany   pcnstr 1
             ]))

and compare_sum loc cds value1 value2 =
  let mcs = branches_of_sum cds in
  let e = pexp_match ~loc (pexp_tuple ~loc [value1; value2]) mcs in
  phys_equal_first value1 value2 e

and compare_of_ty ty value1 value2 =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr (constructor, args) -> compare_applied ~constructor ~args value1 value2
  | Ptyp_tuple tys -> compare_of_tuple loc tys value1 value2
  | Ptyp_var name -> eapply ~loc (evar ~loc (tp_name name)) [value1; value2]
  | Ptyp_arrow _ ->
    Location.raise_errorf ~loc
      "ppx_compare: Functions can not be compared."
  | Ptyp_variant (row_fields, Closed, None) ->
    compare_variant loc row_fields value1 value2
  | Ptyp_any -> [%expr let _ = [%e value1] and _ = [%e value2] in 0]
  | _ ->
    Location.raise_errorf ~loc "ppx_compare: unknown type"

and compare_of_ty_fun ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let a = gen_symbol ~prefix:"a" () in
  let b = gen_symbol ~prefix:"b" () in
  let e_a = evar ~loc a in
  let e_b = evar ~loc b in
  let mk_pat x =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc x) ty
    else
      pvar ~loc x
  in
  eta_reduce_if_possible
    [%expr fun [%p mk_pat a] [%p mk_pat b] -> [%e compare_of_ty ty e_a e_b] ]

and compare_of_record_no_phys_equal loc lds value1 value2 =
  let is_evar = function
    | { pexp_desc = Pexp_ident _; _ } -> true
    | _                               -> false
  in
  assert (is_evar value1);
  assert (is_evar value2);
  List.filter lds ~f:(fun ld ->
    match Attribute.get Attrs.ignore ld with
    | None -> true
    | Some () -> false)
  |> List.map ~f:(fun ld ->
    let loc = ld.pld_loc in
    let label = Located.map lident ld.pld_name in
    compare_of_ty ld.pld_type
      (pexp_field ~loc value1 label)
      (pexp_field ~loc value2 label))
  |> chain_if ~loc


let compare_of_record loc lds value1 value2 =
  compare_of_record_no_phys_equal loc lds value1 value2
  |> phys_equal_first value1 value2

let compare_abstract loc type_name v_a v_b =
  [%expr
    Ppx_compare_lib.compare_abstract ~type_name:[%e estring ~loc type_name]
      [%e v_a] [%e v_b]
  ]

let scheme_of_td td =
  let loc = td.ptype_loc in
  let type_ = combinator_type_of_type_declaration td ~f:compare_type in
  match td.ptype_params with
  | [] -> type_
  | l ->
    let vars = List.map l ~f:get_type_param_name in
    ptyp_poly ~loc vars type_

let compare_of_td td ~rec_flag =
  let loc = td.ptype_loc in
  let a = gen_symbol ~prefix:"a" () in
  let b = gen_symbol ~prefix:"b" () in
  let v_a = evar ~loc a in
  let v_b = evar ~loc b in
  let function_body =
    match td.ptype_kind with
    | Ptype_variant cds -> compare_sum       loc cds v_a v_b
    | Ptype_record  lds -> compare_of_record loc lds v_a v_b
    | Ptype_open ->
      Location.raise_errorf ~loc
        "ppx_compare: open types are not yet supported"
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> compare_abstract loc td.ptype_name.txt v_a v_b
      | Some ty ->
        match ty.ptyp_desc with
        | Ptyp_variant (_, Open, _) | Ptyp_variant (_, Closed, Some (_ :: _)) ->
          Location.raise_errorf ~loc:ty.ptyp_loc
            "ppx_compare: cannot compare open polymorphic variant types"
        | Ptyp_variant (row_fields, _, _) ->
          compare_variant loc row_fields v_a v_b
        | _ ->
          compare_of_ty ty v_a v_b
  in
  let extra_names =
    List.map td.ptype_params
      ~f:(fun p -> tp_name (get_type_param_name p).txt)
  in
  let patts = List.map (extra_names @ [a; b]) ~f:(pvar ~loc)
  and bnd = pvar ~loc (function_name td.ptype_name.txt) in
  let poly_scheme = (match extra_names with [] -> false | _::_ -> true) in
  let body = eta_reduce_if_possible_and_nonrec ~rec_flag
               (eabstract ~loc patts function_body) in
  if poly_scheme
  then value_binding ~loc ~pat:(ppat_constraint ~loc bnd (scheme_of_td td)) ~expr:body
  else value_binding ~loc ~pat:bnd ~expr:(pexp_constraint ~loc body (scheme_of_td td))

let str_type_decl ~loc ~path:_ (rec_flag, tds) =
  let rec_flag =
    (object
      inherit type_is_recursive rec_flag tds as super

      method! label_declaration ld =
        match Attribute.get Attrs.ignore ld with
        | None -> super#label_declaration ld
        | Some () -> ()

    end)#go ()
  in
  let bindings = List.map tds ~f:(compare_of_td ~rec_flag) in
  [ pstr_value ~loc rec_flag bindings ]

let sig_type_decl ~loc:_ ~path:_ (_rec_flag, tds) =
  List.map tds ~f:(fun td ->
    let compare_of = combinator_type_of_type_declaration td ~f:compare_type in
    let name = function_name td.ptype_name.txt in
    let loc = td.ptype_loc in
    psig_value ~loc (value_description ~loc ~name:{ td.ptype_name with txt = name }
                       ~type_:compare_of ~prim:[]))

let compare_core_type ty = compare_of_ty_fun ~type_constraint:true ty

let equal_core_type ty =
  let loc = ty.ptyp_loc in
  let arg1 = gen_symbol () in
  let arg2 = gen_symbol () in
  [%expr
    (fun [%p pvar ~loc arg1] [%p pvar ~loc arg2] ->
       match [%e compare_core_type ty] [%e evar ~loc arg1] [%e evar ~loc arg2] with
       | 0 -> true
       | _ -> false
    )
  ]
