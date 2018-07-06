(**
   Support for variable expansion and templates within s-expressions. The
   functions in this module evaluate the following constructs within
   s-expressions:

   {ul
   {- [(:include filename)] is replaced with the list of s-expressions contained
   in [filename], as if the contents of [filename] were directly inserted in
   place of [(:include filename)].  A relative [filename] is taken with respect
   to the file that contains the include macro.}

   {- [(:let v (v1 ... vn) S1 ... Sm)] defines a template [v] with arguments [v1,
   ..., vn] and body [S1 ... Sm]. The definition itself is removed from the
   input. The variables [v1, ..., vn] must be exactly the free variables of [S1,
   ..., Sm] (see below for the meaning of "free variable"). In particular, since
   a macro argument cannot be a function, a let body cannot call a macro that is
   defined elsewhere, only a macro that is defined in the body itself. However if
   you want to use the same macro inside two macros, it is still possible to define
   it in a separate file and include it in both macros. The list [S1 ... Sm] may
   not be empty.}

   {- [(:use v (v1 SS1) ... (vn SSn))] expands to the body of the template [v]
   with lists of s-expressions [SS1, ..., SSn] substituted for the arguments
   [v1, ..., vn] of [v].}

   {- [(:concat S1 ... Sn)] evaluates [S1 ... Sn] to atoms [C1, ..., Cn] when
   possible and is replaced by the string concatenation [C1 | ... | Cn].}}

   Macros other than [:include] will be called 'local'. All [:include] macros
   are resolved before all the local macros, which means that included file
   names cannot contain variables.

   The occurrence of variable [v] in [(:use v ...)] can be either free or bound, depending
   on the surrounding sexp.  The occurrence is free iff it it's not bound, and it's bound
   iff one of the following two conditions apply:

   {ol
   {- All occurrences of [v1], ..., [vn] in the body of [(:let v (v1 ... vn) S1 ... Sm)]
   are bound.}
   {- All occurrences of [v] from the appearance of [(:let v (v1 ... vn) S1 ... Sm)] to
   the end of the sexp nesting level are bound.}}

   Trying to [:use] an unbound variable is an error. Neither the top level file nor any of
   the included files may contain unbound variables.

   The [load...] functions of this module mirror the corresponding functions of
   the [Sexp] module except that they expand the macros in the loaded file and
   may throw additional exceptions.

   Example
   -------

   Assume that [input.sexp] contains
   {v
   (:include defs.sexp)
   (:include template.sexp)
   (:use f (a (:use a)) (b (:use b)))
   v}

   the file [defs.sexp] contains
   {v
   (:let a () hello)
   (:let b () " world")
   v}

   and the file [template.sexp] contains
   {v
   (:let f (a b) (:concat (:use a) (:use b)))
   v}

   Then [load_sexp "input.sexp"] will return "hello world".

   Formal Evaluation Rules
   -----------------------

   In the following [v] denotes a variable (an atom), [S] denotes a sexp, and
   [SS] denotes a list of sexps. Given a map [V] we write [V(v ~> a)] to update
   the map.

   Evaluation rules are of the form [V : SS => SS'] where [V] is a set of
   bindings of the form [v ~> SSv], each binding defining a template [v] with
   body [SSv].

   First some boilerplate rules: a sexp without macros evaluates to itself:

   {v
   V : <empty sexp list> => <empty sexp list>

   V : S  => SS1
   V : SS => SS2
   -------------------
   V : S SS => SS1 SS2

   C is an atom
   ------------
   V : C => C

   V : SS => SS'
   -----------------
   V : (SS) => (SS')
   v}

   Now the interesting rules.

   {v
   free_vars(SSv) = {v1, ..., vn}
   V(v ~> SSv) : SS => SS'
   --------------------------------------
   V : (:let v (v1 ... vn) SSv) SS => SS'

   V(v) = SS
   V : SSi => SSi' for each i
   V(v1 ~> SS1', ..., vn ~> SSn') : SS => SS'
   ------------------------------------------
   V : (:use v (v1 SS1) ... (vn SSn)) => SS'

   v not defined in V
   -----------------------
   V : (:use v ...) => _|_

   V : Si => Ci
   Each Ci is an atom
   -------------------------------------------------------
   V : (:concat S1 ... Sn) => String.concat [C1; ...; Cn]
   v}

   As follows from the let-rule, let definitions may only refer to the variables
   explicitly mentioned in the argument list. This avoids the complexities of
   variable capture and allows us to forego closure building.
*)

type 'a conv =
  [ `Result of 'a | `Error of exn * Sexp.t ]

val sexp_of_conv : ('a -> Sexp.t) -> 'a conv -> Sexp.t

type 'a annot_conv = ([ `Result of 'a | `Error of exn * Sexp.Annotated.t ] as 'body)
  constraint 'body = 'a Sexp.Annotated.conv

val sexp_of_annot_conv : ('a -> Sexp.t) -> 'a annot_conv -> Sexp.t

val load_sexp : string -> Sexp.t
(** [load_sexp file] like [{!Sexp.load_sexp} file], but resolves the macros
    contained in [file]. *)

val load_sexps : string -> Sexp.t list
(** [load_sexps file] like [{!Sexp.load_sexps} file], but resolves the macros
    contained in [file]. *)

val load_sexp_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv
(** [load_sexp_conv file f] uses {!load_sexp} and converts the result using
    [f]. *)

val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv list
(** [load_sexps_conv file f] uses {!load_sexps} and converts the result using
    [f]. *)

val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a
(** [load_sexp_conv_exn file f] like {!load_sexp_conv}, but raises an exception
    in case of conversion error. *)

val load_sexps_conv_exn : string -> (Sexp.t -> 'a) -> 'a list
(** [load_sexps_conv_exn file f] like {!load_sexps_conv}, but raises an
    exception in case of conversion error. *)

val expand_local_macros : Sexp.t list -> Sexp.t list conv
(** [expand_local_macros sexps] takes a list of sexps and performs macro-expansion on
    them, except that an error will be returned if an :include macro is found. *)

(** A version of [load_sexps] that is functorized with respect to the functions
    that load the sexps from files and the corresponding monad. *)
module type Sexp_loader = sig
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    module Monad_infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    end
    module List : sig
      val iter : 'a list -> f:('a -> unit t) -> unit t
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end
  val load_sexps           : string -> Sexp.t           list Monad.t
  val load_annotated_sexps : string -> Sexp.Annotated.t list Monad.t
end

module Loader (S : Sexp_loader) : sig
  val load_sexp_conv  : string -> (Sexp.t -> 'a) -> 'a annot_conv      S.Monad.t
  val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv list S.Monad.t
end

val add_error_location : string -> exn -> exn
