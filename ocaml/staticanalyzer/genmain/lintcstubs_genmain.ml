(** Load a .cmt file which contains a Typedtree,
    and use it to extract primitives along with the shapes of their arguments,
    and generate a 'main' function to call them all for the purpose of static
    analysis.

    [ocamlc -dtypedtree foo.ml] can be used to see how the typedtree looks
    like.

    A Typedtree is better than a Parsetree for this purpose because it contains
    resolved types and type immediacy information from the compiler itself.
 *)

let usage_msg = Printf.sprintf "%s [FILE.cmt...]" Sys.executable_name

(** [warning loc fmt] prints a warning at source location [loc],
    with message format defined by [fmt].
 *)
let warning loc =
  Printf.ksprintf @@ fun msg -> Location.prerr_warning loc (Preprocessor msg)

(** [nondet ctype] is a generator for [ctype].
    See [sv-comp.c] in [goblint], these are the nondeterministic value
    generators used in static verifier competitions, and supported by various
    static analyzers
 *)
let nondet typ = "__VERIFIER_nondet_" ^ typ

let print_nondet_prototype t =
  let open Primitives_of_cmt in
  let ctype = ctype_of_native_arg t in
  Printf.printf "%s %s(void);" ctype (nondet ctype)

let gen_of_native_arg args =
  let open Primitives_of_cmt in
  function
  (* TODO: we could do more analysis on the type for value to determine whether
     it is an integer or not, what tag it can have, etc. *)
  | (Value | Double | Int32 | Int64 | Intnat) as arg ->
      nondet @@ ctype_of_native_arg arg ^ "()"
  | Bytecode_argv ->
      Printf.sprintf "value[]{%s}"
      @@ String.concat ", "
      @@ List.map ctype_of_native_arg args
  | Bytecode_argn ->
      List.length args |> string_of_int

module StringSet = Set.Make (String)

let calls = ref StringSet.empty

let print_call ~noalloc res name args =
  let open Printf in
  if not @@ StringSet.mem name !calls then (
    calls := StringSet.add name !calls ;
    printf "static void __call_%s(void) {\n" name ;
    if noalloc then
      printf "\tCAMLnoalloc;\n" ;
    printf "\t%s res = %s(%s);\n"
      (Primitives_of_cmt.ctype_of_native_arg res)
      name
    @@ String.concat ", "
    @@ List.map (gen_of_native_arg args) args ;
    if res = Value then
      printf "\t__access_Val(res);\n"
    (* check that the value is valid *)
    (* TODO: could insert more assertions based on actual type *)
    else
      printf "\t(void)res;\n" ;
    (*  suppress unused value warning *)
    print_endline "}"
  )

let print_c_prototype ~noalloc res name args =
  let open Primitives_of_cmt in
  Printf.printf "CAMLprim %s %s(%s);\n" (ctype_of_native_arg res) name
  @@ String.concat ",\n  "
  @@ List.map ctype_of_native_arg args ;
  print_call ~noalloc res name args

let print_c_prototype_arity arity byte_name =
  let open Primitives_of_cmt in
  print_c_prototype Value byte_name @@ List.init arity (fun _ -> Value)

let primitive_description desc =
  let open Primitives_of_cmt in
  (* print native first *)
  let noalloc = not desc.alloc in
  print_c_prototype ~noalloc desc.native_result desc.native_name
    desc.native_args ;
  (* if the bytecode one is different, print it *)
  if desc.native_name <> desc.byte_name then
    if desc.arity <= 5 then
      print_c_prototype_arity ~noalloc desc.arity desc.byte_name
    else
      print_c_prototype ~noalloc Value desc.byte_name
        [Bytecode_argv; Bytecode_argn]
  else
    (* according to https://v2.ocaml.org/manual/intfc.html#ss:c-prim-impl
       if the primitive takes more than 5 arguments then bytecode and native
       mode implementations must be different *)
    assert (desc.arity <= 5) ;
  print_endline ""

let print_call_all () =
  (* TODO: could use Format module *)
  print_endline "static void* __call__all(void* arg) {" ;
  print_endline "\t(void)arg;" ;
  print_endline "\tcaml_leave_blocking_section();" ;
  (* some of these may raise exceptions, so use a nondet to choose which one to
     call, to ensure they are all seen as called *)
  print_endline "\tswitch(__VERIFIER_nondet_int()) {" ;
  let () =
    !calls
    |> StringSet.elements
    |> List.iteri @@ fun i name ->
       Printf.printf "\tcase %d: __call_%s(); break;\n" i name
  in
  print_endline "\tdefault: __caml_maybe_run_gc(); break;" ;
  print_endline "\t}" ;
  print_endline "\tcaml_enter_blocking_section();" ;
  print_endline "}" ;

  print_endline "" ;
  print_endline "#include <pthread.h>" ;
  print_endline "int main(void)" ;
  print_endline "{" ;
  print_endline "\tpthread_t thread;" ;
  print_endline "\tint rc = pthread_create(&thread, NULL, __call__all, NULL);" ;
  print_endline "\t__goblint_assume(!rc);" ;
  (* don't model thread creation failure *)
  print_endline "\t(void)__call__all(NULL);" ;
  print_endline "\trc = pthread_join(thread, NULL);" ;
  print_endline "\t__goblint_assume(!rc);" ;
  (* don't model thread creation failure *)
  print_endline "\treturn 0;" ;
  print_endline "}"

let () =
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in

  print_endline {|#include "primitives.h"|} ;
  print_endline {|#include <goblint.h>|} ;
  print_endline {|#include "caml/threads.h"|} ;
  Printf.printf
    {|
#ifndef CAMLnoalloc
/* GC status assertions.

   CAMLnoalloc at the start of a block means that the GC must not be
   invoked during the block. */
#if defined(__GNUC__) && defined(DEBUG)
int caml_noalloc_begin(void);
void caml_noalloc_end(int*);
void caml_alloc_point_here(void);
#define CAMLnoalloc                          \
  int caml__noalloc                          \
  __attribute__((cleanup(caml_noalloc_end),unused)) \
    = caml_noalloc_begin()
#define CAMLalloc_point_here (caml_alloc_point_here())
#else
#define CAMLnoalloc
#define CAMLalloc_point_here ((void)0)
#endif
#endif
    |} ;

  let () =
    (* TODO: put in a header *)
    Printf.printf "int __VERIFIER_nondet_int(void);\n" ;
    Printf.printf "void __access_Val(value);\n" ;
    Primitives_of_cmt.[Value; Double; Int32; Int64; Intnat]
    |> List.iter @@ fun t -> print_nondet_prototype t
  in
  print_endline "void __caml_maybe_run_gc(void);" ;
  Primitives_of_cmt.with_report_exceptions @@ fun () ->
  let () =
    files
    |> List.iter @@ fun path ->
       Primitives_of_cmt.iter_primitives_exn ~path primitive_description
  in
  print_call_all ()
