(** Load a .cmt file which contains a Typedtree,
    and use it to extract primitives along with the shapes of their arguments,
    and generate a 'main' function to call them all for the purpose of static
    analysis.

    [ocamlc -dtypedtree foo.ml] can be used to see how the typedtree looks
    like.

    A Typedtree is better than a Parsetree for this purpose because it contains
    resolved types and type immediacy information from the compiler itself.
 *)

let tool_name = Sys.executable_name

let usage_msg = Printf.sprintf "%s [FILE.cmt...]" tool_name

(** [args_of_type typ] returns the sequence of arguments for the function type [typ].

    Type aliases are not expanded, and we only recurse on right hand side of
    the type arrow.
    @see <https://v2.ocaml.org/manual/intfc.html#ss:c-prim-decl> examples in the manual.
 *)
let rec args_of_type =
  let open Typedtree in
  function
  | {ctyp_desc= Ttyp_arrow (_, t1, t2); _} ->
      fun () -> Seq.Cons (t1, args_of_type t2)
  | t ->
      Seq.return t

let has_attr name lst =
  (* typedtree attributes are the same as in parsetree *)
  let open Parsetree in
  Option.is_some (lst |> List.find_opt @@ fun attr -> attr.attr_name.txt = name)

let ctype_of_ocaml ~is_unboxed =
  let open Typedtree in
  function
  | {ctyp_desc= Ttyp_constr (path, _, _); ctyp_attributes; _}
    when is_unboxed
         || has_attr "unboxed" ctyp_attributes
         || has_attr "untagged" ctyp_attributes ->
      let is_predef = Path.same path in
      if is_predef Predef.path_float then
        "double"
      else if is_predef Predef.path_int32 then
        "int32"
      else if is_predef Predef.path_int64 then
        "int64"
      else if is_predef Predef.path_nativeint then
        "intnat"
      else if is_predef Predef.path_int then
        "intnat"
      else
        invalid_arg
        @@ Format.asprintf "unknown type name for unboxed: %a" Path.print path
  | {ctyp_attributes= []; _} ->
      "value"
  | {ctyp_attributes= attrs; _} ->
      let attrnames = attrs |> List.map @@ fun a -> a.Parsetree.attr_name.txt in
      invalid_arg
      @@ Printf.sprintf "unknown attributes: %s" (String.concat ", " attrnames)

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
  Printf.ksprintf @@ fun msg -> Location.prerr_warning loc (Preprocessor msg)

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

      (* TODO: gen*)
let str_of_native_repr =
  let open Primitive in
  function
  | Same_as_ocaml_repr ->
      "value"
  | Unboxed_float ->
      "double"
  | Unboxed_integer Pnativeint ->
      "intnat"
  | Unboxed_integer Pint32 ->
      "int32_t"
  | Unboxed_integer Pint64 ->
      "int64_t"
  | Untagged_int ->
      "intnat"

let primitive_description desc =
  let open Primitive in
  if native_name_is_external desc then (
    (* only process primitives defined by the user (not the compiler) *)
    Printf.printf "void __call_%s(void) { %s result = %s(%s); }\n"
      (native_name desc)
      (str_of_native_repr desc.prim_native_repr_res)
      (native_name desc)
    @@ String.concat ", "
    @@ List.map str_of_native_repr desc.prim_native_repr_args ;
    let bytecode_args =
      String.concat ", " @@ List.init desc.prim_arity @@ fun _ -> "__VERIFIER_nondet_value()"
    in
    (* only output bytecode call if different *)
    if desc.prim_arity > 5 then
      Printf.printf
        "void __call_%s(void) { value argv[%d] = {%s}; value result =\n\
        \        %s(argv, %d);}\n"
        desc.prim_name desc.prim_arity bytecode_args desc.prim_name
        desc.prim_arity
    else if
      List.exists (( <> ) Same_as_ocaml_repr) desc.prim_native_repr_args
      || desc.prim_native_repr_res <> Same_as_ocaml_repr
    then
      Printf.printf "void __call_%s(void) { value result = %s(%s);}\n"
        desc.prim_name desc.prim_name bytecode_args
  )

(** [value_description _ mc] is invoked by the TAST iterator for
    value descriptions.
    Recursively iterate until we find a [primitive_coercion].
*)
let rec value_description _ vd =
  let open Typedtree in
  let open Types in
  match vd.val_val.val_kind with
  | Val_prim prim ->
      primitive_description prim
  | _ ->
      ()

let verifier_section = "goblint-ocaml-cstub"

let () =
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in

  print_endline {|#include  "primitives.h"|} ;
  try
    files
    |> List.iter @@ fun path ->
       let open Tast_iterator in
       path
       (* have to parse the implementation, because the .mli may hide that it
          is a C stub by defining a 'val name ...' instead of 'external name ...'. *)
       |> Cmt_format.read_cmt
       |> function
       | Cmt_format.{cmt_annots= Implementation typedtree; _} ->
           let iterator = {default_iterator with value_description} in
           iterator.structure iterator typedtree
       | _ ->
           invalid_arg "not a .cmt file (missing implementation)"
  with e ->
    (* if there are any syntax errors, or other exceptions escaping from
       compiler-libs this will report them properly *)
    Location.report_exception Format.err_formatter e
