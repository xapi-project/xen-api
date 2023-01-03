open Prelude.Ana
open Analyses
open Cilint

(* OCaml 5 style per-domain lock which must be held before calling the OCaml
   runtime functions.

   __thread local variables are not yet supported by goblint/CIL,
   so it may report a race on caml_local_roots, which can be avoided by
   wrapping memory.h local root manipulation with __VERIFIER_atomic_begin and
   __VERIFIER_atomic_end

   For now declare the domain lock as a function-local variable to simplify
   analysis (and we'll require that all ocaml runtime functions called have
   that lock held, but it'll make function exit awkward as we'd be leaking a
   lock, so we'll need to unlock it on return..)

   Actually we do have a threadid query which we can use

   Callbacks are more difficult to handle because we may not know the domain
   lock state there,
   and have to do interprocedural analysis...
*)


module ThreadLocal =
struct
  module TID = ThreadIdDomain.FlagConfiguredTID

  module HC = Printable.HConsed(TID)

  let fallback_global = TID.threadinit ~multiple:false (makeGlobalVar "__fallback_global__" intType)

  (** if we can implement __thread of get_domain_state with this then maybe we
   won't need the atomic begin/end
   *)

  let get name (ctx) =
      let ask = Analyses.ask_of_ctx ctx in
      let tid =
          match ThreadId.get_current ask with
          | `Lifted tid -> tid
          | `Top | `Bot -> fallback_global
      in
      let create_var tid =
      let tid_name =
          ThreadIdDomain.FlagConfiguredTID.show tid
      in
          Goblintutil.create_var (makeGlobalVar (name ^ "_" ^ tid_name) intType)
      in
      HC.lift_f create_var @@ HC.lift tid
end

module DomainLock = struct
  let get (ctx) =
      LockDomain.Addr.from_var @@ ThreadLocal.get "__VERIFIER_ocaml_domain_lock" ctx

  let is_held ctx =
      (* TODO: this should be tri or 4-state: known held, known notheld, and
         unknown, or known to be called from both *)
      let lockset = ctx.ask Queries.MustLockset in
      let lock = get ctx |> LockDomain.Addr.to_var |> Option.get in
      ignore (Pretty.printf "lockset: %a\n" Queries.LS.pretty lockset);
      Queries.LS.mem (lock, `NoOffset) lockset
end

module CStubs = struct
  let is_cstub (f:fundec) =
    (* This relies on patcher having patched caml/misc.h in the copy used by
       the analyses, see 'camlprim-attr.patcher'.
       Normally CAMLprim
    *)
    hasAttribute {|section|} f.svar.vattr
end

module Spec : Analyses.MCPSpec =
struct
  let name () = "domainlock"

  module D = Lattice.Unit
  module C = D
  let startstate v = D.bot ()
  let exitstate v = D.top ()

  include Analyses.IdentitySpec

  let is_value_ptr = function
    | TPtr (TNamed({tname = "value"; _}, _), _) -> true
    | _ -> false

  (* TODO: use visitor class *)
  let rec has_ocaml_value : exp -> bool = function
    | Lval(Mem e, _) -> has_ocaml_value e
    | CastE (t, e) ->
      is_value_ptr t || has_ocaml_value e
    | BinOp(_, a, b, _) ->
        has_ocaml_value a || has_ocaml_value b
    | e ->
      (* TODO: trace ignore (Pretty.printf "has_ocaml_value? %a\n" Cil.d_exp e); *)
      false

  let body ctx f =
      if CStubs.is_cstub f then begin
    (* TODO: handle finalizer too, lock held?.. *)
        let lock = DomainLock.get ctx in
          ctx.emit (Events.Lock (lock, true));
      end;
      ctx.local

  let return ctx _ (f:fundec) =
      if CStubs.is_cstub f then
        let lock = DomainLock.get ctx in
          ctx.emit (Events.Unlock lock);
          ctx.local

  let special (ctx:(D.t, G.t, C.t,V.t) ctx) (lval: lval option) (f:varinfo) (arglist:exp list) =
    match f.vname with
    | "caml_enter_blocking_section" ->
    let lock = DomainLock.get ctx in
      ctx.emit (Events.Unlock (lock));
    ctx.local
    | "caml_leave_blocking_section" ->
    let lock = DomainLock.get ctx in
      ctx.emit (Events.Lock (lock, true));
    ctx.local
  | name when String.starts_with "caml_" name ->
    (* call into OCaml runtime system, must hold domain lock *)
    if not @@ DomainLock.is_held ctx then
      (* TODO: perhaps show last lock/release position? *)
      Messages.error ~category:Messages.Category.Race "DomainLock: must be held
      when calling OCaml runtime function";
    ctx.local
    | _ ->
      let () = arglist |> List.iter @@ fun arg ->
      if has_ocaml_value arg && not @@ DomainLock.is_held ctx then
        Messages.error ~category:Messages.Category.Race
          "DomainLock: Call using OCaml value after domain lock has been released: %s(... %a ...)"
          f.vname Cil.d_exp arg
        in
      ctx.local

end

let () =
  MCP.register_analysis (module Spec : MCPSpec)
