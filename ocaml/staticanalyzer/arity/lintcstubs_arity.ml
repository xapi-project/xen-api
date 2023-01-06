(** Parse a .ml file, extract all 'external ...' primitives,
    and print prototypes of C functions based on their number of arguments.

    If --verifier is specified then output attributes for [lintcstubs] to
    recognize the C stubs as entry points.

    Uses compiler-libs, which has an unstable API that can change between
    compiler versions, so extract only the minimal information needed here.
    If this breaks with newer compiler versions then
    ocaml-migrate-parsetree could be used.
    Currently require a 4.08 AST minimum (although this could be relaxed with
    migrate-parsetree).

    [ocamlc -dparsetree foo.ml] can be used to see how the parsetree looks
    like.
 *)
let verifier = ref false

(** [spec] defines command line arguments parsed by [Arg.parse] *)
let spec =
  [("--verifier", Arg.Set verifier, "output attributes for static analyzer")]

let tool_name = Sys.executable_name

let usage_msg = Printf.sprintf "%s [FILE.ml...]" tool_name

(** [argity_of_type typ] returns the number of arguments for the function type [typ].

    Type aliases are not expanded, and we only recurse on right hand side of
    the type arrow.
    @see <https://v2.ocaml.org/manual/intfc.html#ss:c-prim-decl> examples in the manual.
 *)
let rec arity_of_type =
  let open Parsetree in
  function
  | {ptyp_desc= Ptyp_arrow (_, _t1, t2); _} -> 1 + arity_of_type t2 | _ -> 0

(** [print_c_prototype ~arity bytename nativename] prints C prototypes for
    calls to user defined primitives implemented by [bytename]
    (in bytecode mode) and [nativename] (in native code mode).
    [arity] is the number of arguments, when <= 5 [bytename] and [nativename]
    are the same.

    Does not support unboxed or untagged calls (filtered out by caller).
*)
let print_c_prototype ~arity bytename nativename =
  let args = List.init arity @@ fun _ -> "value" in
  let str_of_args args = String.concat ", " @@ List.rev args in
  Printf.printf "CAMLprim value %s(%s);\n" nativename @@ str_of_args args ;
  if arity <= 5 then
    assert (bytename = nativename)
  else
    Printf.printf "CAMLprim value %s(value *argv, int argn);\n" bytename

(** [warning loc fmt] prints a warning at source location [loc],
    with message format defined by [fmt].
 *)
let warning loc =
  Printf.ksprintf @@ fun msg ->
  Location.prerr_warning loc (Preprocessor msg)

(** [no_attrs typ] returns true if there are no attributes on the type
    (components).

  @see <https://v2.ocaml.org/api/compilerlibref/Parsetree.html#2_Typeexpressions>
*)
let rec no_attrs =
  let open Parsetree in
  function
  | {ptyp_attributes= _ :: _; _} ->
      false
  | {ptyp_desc= Ptyp_arrow (_, t1, t2); _} ->
      no_attrs t1 && no_attrs t2
  | _ ->
      true

(** [value_description _ vd] is invoked by the AST iterator for value
    descriptions, including primitives ('external ...').

    @see <https://v2.ocaml.org/api/compilerlibref/Parsetree.html#2_Valuedescriptions>
*)
let value_description _ vd =
  let open Parsetree in
  let arity = arity_of_type vd.pval_type in
  match vd.pval_prim, vd.pval_attributes with
  | [], _ -> () (* not a primitive *)
  | _, ([] | [{attr_name= {txt= "noalloc"; _}; _}]) when no_attrs vd.pval_type -> (
    (* only process descriptions with no attributes, or with the [@@noalloc]
       attribute: in these cases the C stub is always called with [value]
       arguments.
    *)
    match vd.pval_prim with
    | [] ->
        () (* not a primitive *)
    | builtin :: _ when builtin.[0] = '%' ->
        () (* call to builtin primitive, no prototypes to print *)
    | [cfunction] ->
        print_c_prototype ~arity cfunction cfunction
    | [bytecode_c_function; native_c_function] ->
        print_c_prototype ~arity bytecode_c_function native_c_function
    | _ ->
        (* According to https://v2.ocaml.org/manual/intfc.html#ss:c-prim-decl
           extra flags names are reserved for the standard library's use
        *)
        warning vd.pval_loc
          "Ignored primitive declaration %S: flag names are not supported"
          vd.pval_name.txt
  )
  | _ ->
      (* Would need a Typedtree to correctly interpret these, see
         lintcstubs_cmt.
         It is in theory possible to redefine builtin types like
         'type int = string',
         thus we need the final, resolved type name to be sure.
         In this tool just ignore them.
      *)
      warning vd.pval_loc "Ignored primitive declaration %S: has attributes"
        vd.pval_name.txt

let verifier_section = "goblint-ocaml-cstub"

let () =
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    Arg.parse spec (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in
  (* [CAML_NAME_SPACE] is recommended by the manual *)
  print_endline "#define CAML_NAME_SPACE" ;
  (* get the definition of [value] *)
  print_endline "#include <caml/mlvalues.h>" ;

  if !verifier then (
    print_endline "#undef CAMLprim" ;
    (* The section name here must match the one used by the static analyzer,
       TODO export as a variable
    *)
    Printf.printf {|#define CAMLprim __attribute__((section("%s")))|}
      verifier_section ;
    print_endline ""
  ) ;
  try
    files
    |> List.iter @@ fun path ->
       let open Ast_iterator in
       (* use the AST iterator, because primitives might be declared inside a
          module, not necessarily at top level. *)
       let primitives_iterator = {default_iterator with value_description} in
       path
       (* have to parse the implementation, because the .mli may hide that it
          is a C stub by defining a 'val name ...' instead of 'external name ...'. *)
       |> Pparse.parse_implementation ~tool_name
       |> primitives_iterator.structure primitives_iterator
  with e ->
    (* if there are any syntax errors, or other exceptions escaping from
       compiler-libs this will report them properly *)
    Location.report_exception Format.err_formatter e
