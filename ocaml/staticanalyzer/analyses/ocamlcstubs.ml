open Prelude.Ana
open Analyses
open! Cilint

(* M.tracing is not enabled by default in upstream goblint build for
   performance reasons, use a boolean to turn tracing on/off just for this
   module.

   Usage on the command line: '--enable dbg.debug'
*)
let trace_name = __MODULE__

let tracing_enabled = lazy (GobConfig.get_bool "dbg.debug")

let tracing () = Lazy.force tracing_enabled

let tracel fmt = M.tracel trace_name (fmt ^^ "\n")

module DomainLock = struct
  (* This simulates OCaml 4.x semantics with a single global lock,
      it should instead be configurable to use per-domain locks (e.g. N threads with M domains)
  *)
  let runtime_lock_var =
    Goblintutil.create_var @@ makeGlobalVar "[OCaml runtime lock]" intType

  let runtime_lock_event = LockDomain.Addr.from_var runtime_lock_var

  let runtime_lock = AddrOf (Cil.var runtime_lock_var)

  let must_be_held ctx name =
    let lockset = ctx.ask Queries.MustLockset in
    if tracing () then
      tracel "OCaml domain lock must be held, current lockset is %a"
        Queries.LS.pretty lockset ;
    if not @@ Queries.LS.mem (runtime_lock_var, `NoOffset) lockset then
      (* we could use something similar to MayLocks to track may lock and give
         a better warning message: is the lock maybe held on some paths, or
         surely not held? *)
      Messages.error ~category:Messages.Category.Race
        "DomainLock: must be held when calling OCaml runtime function %s" name ;
    ctx.local

  let must_be_protected_by ctx write (arg : varinfo) =
    if tracing () then
      tracel
        "OCaml domain lock must protect access to OCaml value %s (write=%b)"
        arg.vname write ;
    let must =
      ctx.ask
        Queries.(
          MustBeProtectedBy {mutex= runtime_lock_event; write; global= arg}
        )
    in
    if not must then
      Messages.error ~category:Messages.Category.Race
        "DomainLock: must be held when dereferencing OCaml value %s" arg.vname ;
    if tracing () then
      tracel
        "OCaml domain lock must protect access to OCaml value %s (write=%b, \
         must = %b)"
        arg.vname write must ;
    (* sometimes the must above answers true even if the domain lock is not
       held? *)
    must_be_held ctx arg.vname ;
    (* TODO: this should say accessing OCaml value,
       not runtime function *)
    ctx.local
end

let ocaml_runtime_functions : (string * LibraryDesc.t) list =
  LibraryDsl.
    [
      ( "caml_leave_blocking_section"
      , special []
        @@ Lock
             {
               lock= DomainLock.runtime_lock
             ; try_= false
             ; write= true
             ; return_on_success= true
             }
      )
    ; ( "caml_enter_blocking_section"
      , special [] @@ Unlock DomainLock.runtime_lock
      )
      (* TODO: more functions here *)
    ]

module Cstub = struct
  let is_cstub_entry_svar svar =
    (* This relies on patcher having patched caml/misc.h in the copy used by
         the analyses, see 'camlprim-attr.patcher'.
         Normally CAMLprim
    *)
    let is =
      ContextUtil.has_attribute "section" "goblint-ocaml-cstub" svar.vattr
    in
    if tracing () then
      tracel "function %s is an OCaml C stub: %b" svar.vname is ;
    is

  let find_all file =
    []
    |> foldGlobals file @@ fun acc -> function
       | GFun ({svar; _}, _) when is_cstub_entry_svar svar -> svar.vname :: acc
       | _ -> acc

  let is_cstub_entry _ctx f = is_cstub_entry_svar f.svar

  let enter_cstub ctx _ =
    (* TODO: one CAMLprim can call another one, e.g. common in bytecode impl
        that calls native,
        so this should be a trylock, or there should be an outer function
          locking and calling this.
       For now take the lock here
    *)
    ctx.emit (Events.Lock (DomainLock.runtime_lock_event, true)) ;
    ctx.local

  let leave_cstub ctx _ =
    ctx.emit (Events.Unlock DomainLock.runtime_lock_event) ;
    ctx.local

  let call_caml_runtime ctx f _arglist =
    DomainLock.must_be_held ctx f.vname ;
    ctx.local
end

let is_ocaml_value_type = function
  | TNamed ({tname= "value"; _}, _) ->
      true
  | _ ->
      false

class exp_ocaml_value_extractor (acc : varinfo list ref) =
  object
    inherit nopCilVisitor

    method! vvrbl v =
      if tracing () then
        tracel "checking value use %s, type %a" v.vname Cil.d_type v.vtype ;
      if is_ocaml_value_type v.vtype then (
        acc := v :: !acc ;
        SkipChildren
      ) else
        DoChildren
  end

let ocaml_values_of_exp exp =
  let values = ref [] in
  let visitor = new exp_ocaml_value_extractor values in
  let (_ : exp) = visitCilExpr visitor exp in
  !values

class exp_ocaml_value_deref_extractor (acc : varinfo list ref) =
  object
    inherit nopCilVisitor

    method! vlval =
      function
      | Mem exp, _ ->
          if tracing () then
            tracel "checking exp %a" Cil.d_exp exp ;
          let ocaml_values = ocaml_values_of_exp exp in
          acc := List.rev_append ocaml_values !acc ;
          DoChildren
      | _ ->
          DoChildren
  end

let ocaml_value_derefs_of_exp exp =
  let values = ref [] in
  let visitor = new exp_ocaml_value_deref_extractor values in
  let (_ : exp) = visitCilExpr visitor exp in
  !values

module Spec : Analyses.MCPSpec = struct
  let name () = "ocamlcstubs"

  module D = Lattice.Unit
  module C = D

  let startstate _v = D.bot ()

  let exitstate _v = D.top ()

  include Analyses.IdentitySpec

  let body ctx f =
    (* TODO: set ctx bool that we're inside cstub, to avoid false positives on
       runtime inline functions *)
    if Cstub.is_cstub_entry ctx f then
      Cstub.enter_cstub ctx f
    else
      ctx.local

  let return ctx _ (f : fundec) =
    if Cstub.is_cstub_entry ctx f then
      Cstub.leave_cstub ctx f
    else
      ctx.local

  let special (ctx : (D.t, G.t, C.t, V.t) ctx) (_lval : lval option)
      (f : varinfo) (arglist : exp list) =
    if tracing () then
      tracel "special(%s)" f.vname ;
    match f.vname with
    | "caml_leave_blocking_section" ->
        ctx.local
    | "caml_alloc_custom" ->
        let local = Cstub.call_caml_runtime ctx f arglist in
        (* TODO: find functions in struct and register as C stub roots... *)
        ()
    | n when String.starts_with n "caml_" ->
        (* call into OCaml runtime system, must hold domain lock *)
        Cstub.call_caml_runtime ctx f arglist
    | _ ->
        ctx.local

  let event ctx e _octx =
    match e with
    | Events.Access {exp; kind= AccessKind.(Read | Write) as kind; reach; _} ->
        (* TODO: only for pointers *)
        if tracing () then
          tracel "access %a, kind %a, reach %b" Cil.d_exp exp AccessKind.pretty
            kind reach ;
        (* TODO: reject free and spawn kinds? *)
        exp
        |> ocaml_value_derefs_of_exp
        |> List.iter
           @@ DomainLock.must_be_protected_by ctx (kind = AccessKind.Write) ;
        ctx.local
    | _ ->
        ctx.local
end

let () =
  LibraryFunctions.register_library_functions ocaml_runtime_functions ;
  MCP.register_analysis
    ~dep:[AccessAnalysis.Spec.name ()]
    (module Spec : MCPSpec)
