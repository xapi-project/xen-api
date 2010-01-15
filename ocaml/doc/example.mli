(** A one-sentence description of the module.
 *  @group Module-group Heading
 *)
 
(** Some more explanation about the module, its implementation
 *  and purpose, and whatever else it useful to know.
 *)
 
(** {2 A Sub-heading Used to Group Various Functions} *)

(** This is a description of the function [f], placed immediately above the function *)
val f : 'a -> 'b

(** This is a description of the function [g], placed immediately above the function,
 *  while leaving at least one empty line after the previous function. *)
val g : int -> unit

(** Besides functions, also other elements such as types and exceptions have comments.
 *  Note that also the values of a type are given descriptions of their own. *)
type t =
{
	a : string;	(** [a] is a useful string *)
	b : int;	(** [b] is an awesome number *)
}

(** A variant type *)
type q =
| Test of int * string	(** Testing *)
| AnotherTest			(** More testing *)

(** {2 Another heading} *)

(** 
 Additional comments, not associated to any element can be inserted anywhere.
 This gives me the oppotunity to introduce some fancy formatting options, such as
 - a bulleted list
 - of items
 
 Or a numbered list:
 + like
 + this
 
 Text can be formatted as [source code], {i in italics}, {b in bold face}, and
 links to a function such as {!Module.function} can be used.
 
 {3 Lower-level heading}
 
 Additional formatting options are listed in the OCamlDoc manual:
 http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html.
*)
 
(** The order of all functions, comments, etc. in an mli file is maintained in
 *  the documentation. So this exception will appear at the end of the page.
 *)
exception ThisWentTerriblyWrong of string
