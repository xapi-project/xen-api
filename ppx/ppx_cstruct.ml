(*
 * Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Migrate_parsetree
open Printf

open Ast_404
open Longident
open Asttypes
open Parsetree
open Ast_helper
open Ast_mapper
module Loc = Location
module Ast = Ast_convenience_404

type mode = Big_endian | Little_endian | Host_endian

type prim =
  | Char
  | UInt8
  | UInt16
  | UInt32
  | UInt64

type ty =
  | Prim of prim
  | Buffer of prim * int

type field = {
  field: string;
  ty: ty;
  off: int;
}

type t = {
  name: string;
  fields: field list;
  len: int;
  endian: mode;
}

let ty_of_string =
  function
  |"char_t"  |"char" -> Some Char
  |"uint8_t" |"uint8" |"int8" |"int8_t"  -> Some UInt8
  |"uint16_t"|"uint16"|"int16"|"int16_t" -> Some UInt16
  |"uint32_t"|"uint32"|"int32"|"int32_t" -> Some UInt32
  |"uint64_t"|"uint64"|"int64"|"int64_t" -> Some UInt64
  |_ -> None

let width_of_field f =
  let rec width = function
    |Prim Char -> 1
    |Prim UInt8 -> 1
    |Prim UInt16 -> 2
    |Prim UInt32 -> 4
    |Prim UInt64 -> 8
    |Buffer (prim, len) -> (width (Prim prim)) * len
  in
  width f.ty

let field_to_string f =
  let rec string = function
    |Prim Char -> "char_t"
    |Prim UInt8 -> "uint8_t"
    |Prim UInt16 -> "uint16_t"
    |Prim UInt32 -> "uint32_t"
    |Prim UInt64 -> "uint64_t"
    |Buffer (prim, len) -> sprintf "%s[%d]" (string (Prim prim)) len
  in
  sprintf "%s %s" (string f.ty) f.field

let to_string t =
  sprintf "cstruct[%d] %s { %s }" t.len t.name
    (String.concat "; " (List.map field_to_string t.fields))

let loc_err loc fmt = Location.raise_errorf ~loc ("ppx_cstruct error: " ^^ fmt)

let parse_field loc field field_type sz =
  match ty_of_string field_type with
  |None -> loc_err loc "Unknown type %s" field_type
  |Some ty -> begin
    let ty = match ty,sz with
      |_,None -> Prim ty
      |prim,Some sz -> Buffer (prim, sz)
    in
    let off = -1 in
    { field; ty; off }
  end

let create_struct loc endian name fields =
  let endian = match endian with
    |"little_endian" -> Little_endian
    |"big_endian" -> Big_endian
    |"host_endian" -> Host_endian
    |_ -> loc_err loc "unknown endian %s, should be little_endian, big_endian or host_endian" endian
  in
  let len, fields =
    List.fold_left (fun (off,acc) field ->
      let field = {field with off=off} in
      let off = width_of_field field + off in
      let acc = acc @ [field] in
      (off, acc)
    ) (0,[]) fields
  in
  { fields; name = name.txt; len; endian }

let ($.) l x = Longident.Ldot (l, x)
let cstruct_id = Longident.Lident "Cstruct"
let mode_mod s = function
  |Big_endian -> cstruct_id$."BE"$.s
  |Little_endian -> cstruct_id$."LE"$.s
  |Host_endian -> cstruct_id$."HE"$.s

let mode_mod loc x s =
  Exp.ident ~loc {loc ; txt = mode_mod s x}

let getter_name s f = sprintf "get_%s_%s" s.name f.field
let setter_name s f = sprintf "set_%s_%s" s.name f.field
let op_name op s f = sprintf "%s_%s_%s" op s.name f.field

let output_get _loc s f =
  let m = mode_mod _loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_, _) ->
    let len = width_of_field f in
    [
      [%stri
        let [%p Ast.pvar (op_name "get" s f)] =
          fun src -> Cstruct.sub src [%e num f.off] [%e num len]];
      [%stri
        let [%p Ast.pvar (op_name "copy" s f)] =
          fun src -> Cstruct.copy src [%e num f.off] [%e num len]]
    ]
  |Prim prim ->
    [
      [%stri
        let [%p Ast.pvar (getter_name s f)] = fun v ->
          [%e match prim with
              |Char -> [%expr Cstruct.get_char v [%e num f.off]]
              |UInt8 -> [%expr Cstruct.get_uint8 v [%e num f.off]]
              |UInt16 -> [%expr [%e m "get_uint16"] v [%e num f.off]]
              |UInt32 -> [%expr [%e m "get_uint32"] v [%e num f.off]]
              |UInt64 -> [%expr [%e m "get_uint64"] v [%e num f.off]]]]
    ]

let output_get loc s f =
  (output_get loc s f) [@metaloc loc]

let type_of_int_field = function
  |Char -> [%type: char]
  |UInt8 -> [%type: Cstruct.uint8]
  |UInt16 -> [%type: Cstruct.uint16]
  |UInt32 -> [%type: Cstruct.uint32]
  |UInt64 -> [%type: Cstruct.uint64]

let type_of_int_field _loc x =
  type_of_int_field x [@metaloc loc]

let output_get_sig _loc s f =
  match f.ty with
  |Buffer (_,_) ->
    [
      Sig.value (Val.mk (Loc.mknoloc (op_name "get" s f)) [%type: Cstruct.t -> Cstruct.t]);
      Sig.value (Val.mk (Loc.mknoloc (op_name "copy" s f)) [%type: Cstruct.t -> string])
    ]
  |Prim prim ->
    let retf = type_of_int_field _loc prim in
    [
      Sig.value (Val.mk (Loc.mknoloc (getter_name s f)) [%type: Cstruct.t -> [%t retf]])
    ]

let output_get_sig _loc s f =
  output_get_sig _loc s f [@metaloc _loc]

let output_set _loc s f =
  let m = mode_mod _loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_,_) ->
    let len = width_of_field f in
    [
      [%stri
        let [%p Ast.pvar (setter_name s f)] = fun src srcoff dst ->
          Cstruct.blit_from_string src srcoff dst [%e num f.off] [%e num len]];
      [%stri
        let [%p Ast.pvar (op_name "blit" s f)] = fun src srcoff dst ->
          Cstruct.blit src srcoff dst [%e num f.off] [%e num len]]
    ]
  |Prim prim ->
    [
      [%stri
        let [%p Ast.pvar (setter_name s f)] = fun v x ->
          [%e match prim with
              |Char -> [%expr Cstruct.set_char v [%e num f.off] x]
              |UInt8 -> [%expr Cstruct.set_uint8 v [%e num f.off] x]
              |UInt16 -> [%expr [%e m "set_uint16"] v [%e num f.off] x]
              |UInt32 -> [%expr [%e m "set_uint32"] v [%e num f.off] x]
              |UInt64 -> [%expr [%e m "set_uint64"] v [%e num f.off] x]]]
    ]

let output_set _loc s f =
  output_set _loc s f [@metaloc _loc]

let output_set_sig _loc s f =
  match f.ty with
  |Buffer (_,_) ->
    [
      Sig.value (Val.mk (Loc.mkloc (setter_name s f) _loc)
                   [%type: string -> int -> Cstruct.t -> unit]);
      Sig.value (Val.mk (Loc.mkloc (op_name "blit" s f) _loc)
                   [%type: Cstruct.t -> int -> Cstruct.t -> unit])
    ] [@metaloc _loc]
  |Prim prim ->
    let retf = type_of_int_field _loc prim in
    [
      Sig.value (Val.mk (Loc.mkloc (setter_name s f) _loc) [%type: Cstruct.t -> [%t retf] -> unit])
    ] [@metaloc _loc]

let output_sizeof _loc s =
  [%stri
    let [%p Ast.pvar ("sizeof_"^s.name)] = [%e Ast.int s.len]] [@metaloc _loc]

let output_sizeof_sig _loc s =
  Sig.value (Val.mk (Loc.mknoloc ("sizeof_"^s.name)) [%type: int]) [@metaloc _loc]

let output_hexdump _loc s =
  let hexdump =
    List.fold_left (fun a f ->
        [%expr
          [%e a]; Buffer.add_string _buf [%e Ast.str ("  "^f.field^" = ")];
          [%e match f.ty with
              |Prim Char ->
                [%expr Printf.bprintf _buf "%c\n" ([%e Ast.evar (getter_name s f)] v)]
              |Prim (UInt8|UInt16) ->
                [%expr Printf.bprintf _buf "0x%x\n" ([%e Ast.evar (getter_name s f)] v)]
              |Prim UInt32 ->
                [%expr Printf.bprintf _buf "0x%lx\n" ([%e Ast.evar (getter_name s f)] v)]
              |Prim UInt64 ->
                [%expr Printf.bprintf _buf "0x%Lx\n" ([%e Ast.evar (getter_name s f)] v)]
              |Buffer (_,_) ->
                [%expr Printf.bprintf _buf "<buffer %s>"
                         [%e Ast.str (field_to_string f)];
                         Cstruct.hexdump_to_buffer _buf ([%e Ast.evar (getter_name s f)] v)]
          ]]
      ) (Ast.unit ()) s.fields
  in
  [
    [%stri
      let [%p Ast.pvar ("hexdump_"^s.name^"_to_buffer")] = fun _buf v ->
        [%e hexdump]];
    [%stri
      let [%p Ast.pvar ("hexdump_"^s.name)] = fun v ->
        let _buf = Buffer.create 128 in
        Buffer.add_string _buf [%e Ast.str (s.name ^ " = {\n")];
        [%e Ast.evar ("hexdump_"^s.name^"_to_buffer")] _buf v;
        print_endline (Buffer.contents _buf);
        print_endline "}"
    ]
  ] [@metaloc _loc]

let output_hexdump_sig _loc s =
  [
    Sig.value
      (Val.mk (Loc.mkloc ("hexdump_"^s.name^"_to_buffer") _loc)
         [%type: Buffer.t -> Cstruct.t -> unit]);
    Sig.value
      (Val.mk (Loc.mkloc ("hexdump_"^s.name) _loc) [%type: Cstruct.t -> unit])
  ] [@metaloc _loc]

let output_struct _loc s =
  (* Generate functions of the form {get/set}_<struct>_<field> *)
  let expr = List.fold_left (fun a f ->
      a @ output_get _loc s f @ output_set _loc s f
    ) [output_sizeof _loc s] s.fields
  in expr @ output_hexdump _loc s

let output_struct_sig _loc s =
  (* Generate signaturs of the form {get/set}_<struct>_<field> *)
  let expr = List.fold_left (fun a f ->
      a @ output_get_sig _loc s f @ output_set_sig _loc s f
    ) [output_sizeof_sig _loc s] s.fields
  in expr @ output_hexdump_sig _loc s

let output_enum _loc name fields width ~sexp =
  let intfn,pattfn = match ty_of_string width with
    |None -> loc_err _loc "enum: unknown width specifier %s" width
    |Some Char ->
      (fun i -> Exp.constant (Pconst_char (Char.chr @@ Int64.to_int i))),
      (fun i -> Pat.constant (Pconst_char (Char.chr @@ Int64.to_int i)))
    |Some (UInt8 | UInt16) ->
      (fun i -> Exp.constant (Pconst_integer(Int64.to_string i, None))),
      (fun i -> Pat.constant (Pconst_integer(Int64.to_string i, None)))
    |Some UInt32 ->
      (fun i -> Exp.constant (Pconst_integer (Int32.to_string (Int64.to_int32 i), Some 'l'))),
      (fun i -> Pat.constant (Pconst_integer (Int32.to_string (Int64.to_int32 i), Some 'l')))
    |Some UInt64 ->
      (fun i -> Exp.constant (Pconst_integer (Int64.to_string i, Some 'L'))),
      (fun i -> Pat.constant (Pconst_integer (Int64.to_string i, Some 'L')))
  in
  let decls = List.map (fun (f,_) -> Type.constructor f) fields in
  let getters = (List.map (fun ({txt = f; _},i) ->
      {pc_lhs = pattfn i; pc_guard = None; pc_rhs = Ast.constr "Some" [Ast.constr f []]}
    ) fields) @ [{pc_lhs = Pat.any (); pc_guard = None; pc_rhs = Ast.constr "None" []}] in
  let setters = List.map (fun ({txt = f; _},i) ->
      {pc_lhs = Ast.pconstr f []; pc_guard = None; pc_rhs = intfn i}
    ) fields in
  let printers = List.map (fun ({txt = f; _},_) ->
      {pc_lhs = Ast.pconstr f []; pc_guard = None; pc_rhs = Ast.str f}) fields in
  let parsers = List.map (fun ({txt = f; _},_) ->
      {pc_lhs = Ast.pstr f; pc_guard = None; pc_rhs = Ast.constr "Some" [Ast.constr f []]}) fields in
  let getter {txt = x; _} = sprintf "int_to_%s" x in
  let setter {txt = x; _} = sprintf "%s_to_int" x in
  let printer {txt = x; _} = sprintf "%s_to_string" x in
  let parse {txt = x; _} = sprintf "string_to_%s" x in
  let of_sexp {txt = x; _} = sprintf "%s_of_sexp" x in
  let to_sexp {txt = x; _} = sprintf "sexp_of_%s" x in
  let output_sexp_struct =
    [
      [%stri
        let [%p Ast.pvar (to_sexp name)] = fun x ->
          Sexplib.Sexp.Atom ([%e Ast.evar (printer name)] x)];
      [%stri
        let [%p Ast.pvar (of_sexp name)] = fun x ->
          match x with
          | Sexplib.Sexp.List _ ->
            raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "expected Atom, got List", x))
          | Sexplib.Sexp.Atom v ->
            match [%e Ast.evar (parse name)] v with
            | None ->
              raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "unable to parse enum string", x))
            | Some r -> r
          ]
      ] in
  Str.type_ Recursive [Type.mk ~kind:(Ptype_variant decls) name] ::
  [%stri
    let [%p Ast.pvar (getter name)] = fun x -> [%e Exp.match_ [%expr x] getters]] ::
  [%stri
    let [%p Ast.pvar (setter name)] = fun x -> [%e Exp.match_ [%expr x] setters]] ::
  [%stri
    let [%p Ast.pvar (printer name)] = fun x -> [%e Exp.match_ [%expr x] printers]] ::
  [%stri
    let [%p Ast.pvar (parse name)] = fun x ->
      [%e Exp.match_ [%expr x]
            (parsers @ [{pc_lhs = Pat.any (); pc_guard = None; pc_rhs = Ast.constr "None" []}])]] ::
  if sexp then output_sexp_struct else []

let output_enum_sig _loc name fields width ~sexp =
  let oty = match ty_of_string width with
    |None -> loc_err _loc "enum: unknown width specifier %s" width
    |Some Char -> [%type: char]
    |Some (UInt8|UInt16) -> [%type: int]
    |Some UInt32 -> [%type: int32]
    |Some UInt64 -> [%type: int64]
  in
  let decls = List.map (fun (f,_) -> Type.constructor f) fields in
  let getter {txt = x; _}  = sprintf "int_to_%s" x in
  let setter {txt = x; _}  = sprintf "%s_to_int" x in
  let printer {txt = x; _} = sprintf "%s_to_string" x in
  let parse {txt = x; _}   = sprintf "string_to_%s" x in
  let of_sexp {txt = x; _} = sprintf "%s_of_sexp" x in
  let to_sexp {txt = x; _} = sprintf "sexp_of_%s" x in
  let ctyo = [%type: [%t Ast.tconstr name.txt []] option] in
  let cty = Ast.tconstr name.txt [] in
  let output_sexp_sig =
    [
      Sig.value (Val.mk (Loc.mkloc (to_sexp name) _loc) [%type: [%t cty] -> Sexplib.Sexp.t]);
      Sig.value (Val.mk (Loc.mkloc (of_sexp name) _loc) [%type: Sexplib.Sexp.t -> [%t cty]])
    ]
  in
  Sig.type_ Recursive [Type.mk ~kind:(Ptype_variant decls) name] ::
  Sig.value (Val.mk (Loc.mkloc (getter name) _loc) [%type: [%t oty] -> [%t ctyo]]) ::
  Sig.value (Val.mk (Loc.mkloc (setter name) _loc) [%type: [%t cty] -> [%t oty]]) ::
  Sig.value (Val.mk (Loc.mkloc (printer name) _loc) [%type: [%t cty] -> string]) ::
  Sig.value (Val.mk (Loc.mkloc (parse name) _loc) [%type: string -> [%t cty] option]) ::
  if sexp then output_sexp_sig else []

let constr_enum = function
  | {pcd_name = f; pcd_args = Pcstr_tuple []; pcd_attributes = attrs; _} ->
    let id = match attrs with
      | [{txt = "id"; _}, PStr
           [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant cst; pexp_loc = loc; _}, _); _}]] ->
        let cst = match cst with
          | Pconst_integer(i, _) -> Int64.of_string i
          | _ ->
            loc_err loc "invalid id"
        in
        Some cst
      | _ ->
        None
    in
    (f, id)
  | {pcd_loc = loc; _} ->
    loc_err loc "invalid cenum variant"

let constr_field {pld_name = fname; pld_type = fty; pld_loc = loc; pld_attributes = att; _} =
  let get = function
    | [{txt = "len"; _}, PStr
         [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_integer(sz, _)); _}, _); _}]] ->
      Some (int_of_string sz)
    | _ ->
      None
  in
  let sz = match get fty.ptyp_attributes, get att with
  | Some sz, None
  | None, Some sz -> Some sz
  | Some _, Some _ -> loc_err loc "multiple field length attribute"
  | None, None -> None
  in
  let fty = match fty.ptyp_desc with
    | Ptyp_constr ({txt = Lident fty; _}, []) -> fty
    | _ ->
      loc_err fty.ptyp_loc "type identifier expected"
  in
  parse_field loc fname.txt fty sz

let cstruct decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_record fields -> List.map constr_field fields
    | _ -> loc_err loc "record type declaration expected"
  in
  let endian = match attrs with
    | [{txt = endian; _}, PStr []] -> endian
    | [_, _] -> loc_err loc "no attribute payload expected"
    | _ -> loc_err loc "too many attributes"
  in
  create_struct loc endian name fields

let cenum decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_variant fields -> fields
    | _ ->
      loc_err loc "expected variant type"
  in
  let width, sexp =
    match attrs with
    | ({txt = width; _}, PStr []) :: ({txt = "sexp"; _}, PStr []) :: [] ->
      width, true
    | ({txt = width; _}, PStr []) :: [] ->
      width, false
    | _ ->
      loc_err loc "invalid cenum attributes"
  in
  let n = ref Int64.minus_one in
  let incr_n () = n := Int64.succ !n in
  let fields = List.map constr_enum fields in
  let fields =
    List.map (function
        | (f, None)   -> incr_n (); (f, !n)
        | (f, Some i) -> n := i; (f, i)
      ) fields in
  name, fields, width, sexp

let signature_item' mapper = function
  | {psig_desc =
       Psig_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    output_struct_sig loc (cstruct decl)
  | {psig_desc =
       Psig_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    let name, fields, width, sexp = cenum decl in
    output_enum_sig loc name fields width ~sexp
  | other ->
    [default_mapper.signature_item mapper other]

let signature mapper s =
  List.concat (List.map (signature_item' mapper) s)

let structure_item' mapper = function
  | {pstr_desc =
       Pstr_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     pstr_loc = loc} ->
    output_struct loc (cstruct decl)
  | {pstr_desc =
       Pstr_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     pstr_loc = loc} ->
    let name, fields, width, sexp = cenum decl in
    output_enum loc name fields width ~sexp
  | other ->
    [default_mapper.structure_item mapper other]

let structure mapper s =
  List.concat (List.map (structure_item' mapper) s)

let () =
  Driver.register ~name:"ppx_cstruct" Versions.ocaml_404
    (fun _config _cookies -> {default_mapper with structure; signature})
