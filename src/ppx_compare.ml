open Ppxlib

let str_type_decl =
  Deriving.Generator.make_noarg Ppx_compare_expander.str_type_decl
    ~attributes:Ppx_compare_expander.str_attributes

let sig_type_decl =
  Deriving.Generator.make_noarg Ppx_compare_expander.sig_type_decl

let replace_underscores_by_variables =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_any -> Ptyp_var (gen_symbol ~prefix:"a" ())
      | t -> super#core_type_desc t
  end in
  map#core_type

let name = "compare"

let compare =
  Deriving.add name
    ~str_type_decl
    ~sig_type_decl
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.compare_core_type ty)
;;

let () =
  Driver.register_transformation name
    ~rules:[ Context_free.Rule.extension
               (Extension.declare name
                  Core_type Ast_pattern.(ptyp __)
                  (fun ~loc ~path:_ ty ->
                     Ppx_compare_expander.compare_type ~loc
                       (replace_underscores_by_variables ty))) ]
;;

let () =
  let name = "@compare.equal" in
  Deriving.add name
    ~str_type_decl
    ~sig_type_decl
    ~extension:(fun ~loc:_ ~path:_ ty -> Ppx_compare_expander.equal_core_type ty)
  |> Deriving.ignore;

  Driver.register_transformation name
    ~rules:[ Context_free.Rule.extension
               (Extension.declare name
                  Core_type Ast_pattern.(ptyp __)
                  (fun ~loc ~path:_ ty ->
                     Ppx_compare_expander.equal_type ~loc
                       (replace_underscores_by_variables ty))) ]
;;

let add_warning e msg =
  let attr = attribute_of_warning e.pexp_loc msg in
  { e with pexp_attributes = attr :: e.pexp_attributes }
;;

let () =
  let deprecated_name = "equal" in
  Deriving.add deprecated_name
    ~extension:(fun ~loc:_ ~path:_ ty ->
      add_warning
        (Ppx_compare_expander.equal_core_type ty)
        "equal is deprecated, use compare.equal instead")
  |> Deriving.ignore
;;
