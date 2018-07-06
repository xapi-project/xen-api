open Ppxlib
open Extension

(* An expect declaration resembles [%%expect {tag|...|tag}]. We allow arbitrary tags so
   that users can escape their strings properly if need be. *)
let expect =
  Expert.declare "expect"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:false)

(* An expect extension without pretty formatting *)
let expect_exact =
  Expert.declare "expect_exact"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:true)

let expectations = [ expect; expect_exact ]

let match_expectation e =
  match e.pexp_desc with
  | Pexp_extension extension -> begin
      match Expert.convert expectations ~loc:e.pexp_loc extension with
      | None -> None
      | Some f -> Some (f ~extension_id_loc:(fst extension).loc)
    end
  | _ -> None
;;

