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
(** Custom doc generator for our generated code *)

(* Based on the example in the manual:
   http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html#s:ocamldoc-custom-tags
*)
class my_gen =
  object(self)
    inherit Odoc_html.html
    (** Return HTML code for the given text of a bar tag. *)
    method html_of_lock (t: Odoc_info.text) =
      (* Decode the locks annotation, stored as s-expresions *)
      let txt = Odoc_info.string_of_text t in
      let locks =
        match SExpr_TS.of_string txt with
        | SExpr.Node kv ->
          List.map (function SExpr.Node [ SExpr.String k; SExpr.String v ] -> k,v
                           | _ -> failwith "Failed to parse lock comment") kv
        | _ -> failwith "Failed to parse lock comment" in

      if locks = []
      then "<i>No locks held</i>"
      else Printf.sprintf "<i>With the following locks held: %s</i>" (String.concat " " (List.map (fun (k, v) -> k ^ "." ^ v) locks))

    method html_of_bar t = "<pre>hello</pre>"

    initializer
      tag_functions <- ("lock", self#html_of_lock) :: ("bar", self#html_of_bar) :: tag_functions
  end

let _ = print_endline "Running custom ocamldoc"
let my_generator = (new my_gen :>  Odoc_args.doc_generator)
let _ = Odoc_args.set_doc_generator (Some (my_generator :> Odoc_args.doc_generator))
