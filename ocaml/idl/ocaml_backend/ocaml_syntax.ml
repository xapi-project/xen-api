(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Helper types and functions for manipulating ocaml syntax at a high level *)

(**********************************************************************************)

type item =
  | Indent of item list
  | Line of string

let string_of_item x =
  let rec indent prefix = function
    | Line x -> prefix ^ x ^ "\n"
    | Indent x -> String.concat "" (List.map (indent (prefix ^ "  ")) x) in
  indent "" x

(**********************************************************************************)

type ty = string

type param = Anon of (string option) * ty | Named of string * ty

let string_of_param = function
  | Anon(None, _) -> failwith "Anonymous parameter has no name"
  | Anon(Some x, _) -> x
  | Named(x, _) -> x

let type_of_param = function
  | Anon(_, ty) -> ty
  | Named(_, ty) -> ty

module Val = struct
  type t = { name: string; params: param list }

  let item_of x =
    let param = function
      | Anon (_, ty) -> ty
      | Named (name, ty) -> name ^ ":" ^ ty in
    Line ("val " ^ x.name ^ " : " ^ (String.concat " -> " (List.map param x.params)))
end

module Let = struct
  type t = { name: string; params: param list; ty: string; body: string list; doc: string }

  let make ?(doc="") ~name ~params ~ty ~body () =
    { name = name; params = params; ty = ty; body = body; doc = doc }

  let val_of x = { Val.name = x.name; params = x.params @ [ Anon (None, x.ty) ] }

  let items_of ?(prefix="let") x =
    let param = function
      | Anon (None, ty) -> "(_:" ^ ty ^ ")"
      | Anon (Some x, ty) -> "(" ^ x ^ ": " ^ ty ^ ")"
      | Named (name, ty) -> "~" ^ name in
    [ Line ("(** " ^ x.doc ^ " *)");
      Line (prefix ^ " " ^ x.name ^ " " ^
            (String.concat " " (List.map param x.params)) ^ " =");
      Indent (List.map (fun x -> Line x) x.body) ]
end

module Type = struct
  type t = { name: string; body: string }

  let item_of x = Line ("type " ^ x.name ^ " = " ^ x.body)
end

module Module = struct
  type e = Let of Let.t | Module of t | Type of Type.t
  and t = { name: string;     (** OCaml module name *)
            preamble: string list; (** Convenient place for helper functions, opens etc *)
            letrec: bool;     (** True for all the let bindings to be mutually recursive*)
            args: string list; (** for functor *)
            elements: e list }

  let make ?(preamble=[]) ?(letrec=false) ?(args=[]) ~name ~elements () =
    { name = name; preamble = preamble; letrec = letrec;
      args = args; elements = elements }

  let rec items_of x =
    let e = function
      | Let y -> Let.items_of ~prefix:(if x.letrec then "and" else "let") y
      | Module x -> items_of x
      | Type x -> [ Type.item_of x ] in
    let opening = "module " ^ x.name ^ " = " ^
                  (if x.args = []
                   then ""
                   else String.concat " " (List.map (fun x -> "functor(" ^ x ^ ") ->") x.args)) ^
                  "struct" in
    [ Line opening;
      Indent  (
        List.map (fun x -> Line x) x.preamble @
        ( if x.letrec then [ Line "let rec __unused () = ()" ] else [] ) @
        (List.concat (List.map e x.elements))
      );
      Line "end" ]

  let strings_of x = List.map string_of_item (items_of x)
end

module Signature = struct
  type e = Val of Val.t | Module of t | Type of Type.t
  and t = { name: string; elements: e list }

  let rec items_of ?(toplevel=true) x =
    let e = function
      | Val x -> [ Val.item_of x ]
      | Module x -> items_of ~toplevel:false x
      | Type x -> [ Type.item_of x ] in
    [ if toplevel
      then Line ("module type " ^ x.name ^ " = sig")
      else Line ("module " ^ x.name ^ " : sig");
      Indent (
        List.concat (List.map e x.elements)
      );
      Line "end"
    ]

  let rec of_module (x: Module.t) =
    let e = function
      | Module.Let x -> Val (Let.val_of x)
      | Module.Type x -> Type x
      | Module.Module x -> Module (of_module x) in
    { name = x.Module.name;
      elements = List.map e x.Module.elements }

  let strings_of x = List.map string_of_item (items_of x)
end


