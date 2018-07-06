open! Import
open Ast_builder.Default

module Buffer = Caml.Buffer

module Format = Caml.Format

let lident x = Longident.Lident x

let core_type_of_type_declaration td =
  let loc = td.ptype_name.loc in
  ptyp_constr ~loc
    (Located.map lident td.ptype_name)
    (List.map td.ptype_params ~f:fst)
;;

let gen_symbol =
  let cnt = ref 0 in
  fun ?(prefix = "_x") () ->
    cnt := !cnt + 1;
    Printf.sprintf "%s__%03i_" prefix !cnt
;;

let name_type_params_in_td (td : type_declaration) : type_declaration =
  let name_param (tp, variance) =
    let ptyp_desc =
      match tp.ptyp_desc with
      | Ptyp_any -> Ptyp_var ("v" ^ gen_symbol ())
      | Ptyp_var _ as v -> v
      | _ -> Location.raise_errorf ~loc:tp.ptyp_loc "not a type parameter"
    in
    ({ tp with ptyp_desc }, variance)
  in
  { td with ptype_params = List.map td.ptype_params ~f:name_param }
;;

let combinator_type_of_type_declaration td ~f =
  let td = name_type_params_in_td td in
  let result_type = f ~loc:td.ptype_name.loc (core_type_of_type_declaration td) in
  List.fold_right td.ptype_params ~init:result_type ~f:(fun (tp, _variance) acc ->
    let loc = tp.ptyp_loc in
    ptyp_arrow ~loc Nolabel (f ~loc tp) acc)
;;

let string_of_core_type ct =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Pprintast.core_type ppf ct;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
;;

let get_type_param_name (ty, _) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_var name -> Located.mk ~loc name
  | _ -> Location.raise_errorf ~loc "not a type parameter"


exception Type_is_recursive

class type_is_recursive rec_flag tds = object(self)
  inherit Ast_traverse.iter as super

  val type_names : string list = List.map tds ~f:(fun td -> td.ptype_name.txt)

  method return_true () = Exn.raise_without_backtrace Type_is_recursive

  method! core_type ctype =
    match ctype.ptyp_desc with
    | Ptyp_arrow _ -> ()
    | Ptyp_constr ({ txt = Longident.Lident id; _ }, _)
      when List.mem ~equal:String.equal type_names id ->
      self#return_true ()
    | _ -> super#core_type ctype

  method! constructor_declaration cd =
    (* Don't recurse through cd.pcd_res *)
    match cd.pcd_args with
    | Pcstr_tuple args -> List.iter args ~f:self#core_type
    | Pcstr_record fields -> List.iter fields ~f:self#label_declaration

  method go () =
    match rec_flag with
    | Nonrecursive -> Nonrecursive
    | Recursive    ->
      match List.iter tds ~f:self#type_declaration with
      | exception Type_is_recursive -> Recursive
      | () -> Nonrecursive

end

let really_recursive rec_flag tds = (new type_is_recursive rec_flag tds)#go ()

let rec last x l =
  match l with
  | [] -> x
  | x :: l -> last x l
;;

let loc_of_payload (name, payload) =
  match payload with
  | PStr []          -> name.loc
  | PStr (x :: l)    -> { x.pstr_loc with loc_end = (last x l).pstr_loc.loc_end }
  | PSig []          -> name.loc
  | PSig (x :: l)    -> { x.psig_loc with loc_end = (last x l).psig_loc.loc_end }
  | PTyp t           -> t.ptyp_loc
  | PPat (x, None)   -> x.ppat_loc
  | PPat (x, Some e) -> { x.ppat_loc with loc_end = e.pexp_loc.loc_end }
;;

let loc_of_attribute ((name, _) as attr) =
  (* TODO: fix this in the compiler *)
  (* "ocaml.doc" attributes are generated with [Location.none], which is not helpful for
     error messages. *)
  if Polymorphic_compare.(=) name.loc Location.none then
    loc_of_payload attr
  else
    { name.loc with loc_end = (loc_of_payload attr).loc_end }
;;

let curry_applications expr =
  let open Ast_builder_generated.M in
  match expr.pexp_desc with
  | Pexp_apply (f,orig_forward_args) ->
    let loc = expr.pexp_loc in
    let rec loop = function
      | [] -> f
      | last_arg::rev_front_args -> pexp_apply ~loc (loop rev_front_args) [last_arg]
    in
    loop (List.rev orig_forward_args)
  | _ -> expr
;;

let rec assert_no_attributes = function
  | [] -> ()
  (* for the moment we allow merlin attributes everywhere, and it's ok
     if they are just dropped. *)
  | (name, _) :: rest when Name.comes_from_merlin name.Location.txt ->
    assert_no_attributes rest
  | attr :: _ ->
    let loc = loc_of_attribute attr in
    Location.raise_errorf ~loc "Attributes not allowed here"

let assert_no_attributes_in = object
  inherit Ast_traverse.iter

  method! attribute a = assert_no_attributes [a]
end

let attribute_of_warning loc s =
  ({ loc; txt = "ocaml.ppwarning" },
   PStr ([pstr_eval ~loc (estring ~loc s) []]))
