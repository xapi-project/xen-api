open Expect_test_common.Std
open Ppxlib

let transl_loc (loc : Location.t) : File.Location.t =
  { filename    = File.Name.of_string loc.loc_start.pos_fname
  ; line_start  = loc.loc_start.pos_bol
  ; line_number = loc.loc_start.pos_lnum
  ; start_pos   = loc.loc_start.pos_cnum
  ; end_pos     = loc.loc_end.pos_cnum
  }

let make ~is_exact payload ~(extension_id_loc:Location.t) =
  let body_loc, body, tag =
    match payload with
    | None ->
      ({ extension_id_loc with loc_start = extension_id_loc.loc_end },
       Expectation.Body.Pretty "",
       Some "")
    | Some (loc, s, tag) ->
      (loc, (if is_exact then Exact s else Pretty s), tag)
  in
  let res : Expectation.Raw.t =
    { tag
    ; body
    ; extid_location = transl_loc extension_id_loc
    ; body_location  = transl_loc body_loc
    }
  in
  (* Check that we are not in this case:
     {[
       [%expect {|foo
                  bar
                |}]
     ]}
  *)
  match body with
  | Exact _ -> res
  | Pretty s ->
    let len = String.length s in
    let get i = if i >= len then None else Some s.[i] in
    let rec first_line i =
      match get i with
      | None              -> ()
      | Some (' ' | '\t') -> first_line (i + 1)
      | Some '\n'         -> ()
      | Some _            -> first_line_has_stuff (i + 1)
    and first_line_has_stuff i =
      match get i with
      | None      -> ()
      | Some '\n' -> rest_must_be_empty (i + 1)
      | Some _    -> first_line_has_stuff (i + 1)
    and rest_must_be_empty i =
      match get i with
      | None -> ()
      | Some (' ' | '\t' | '\n') ->
        rest_must_be_empty (i + 1)
      | Some _  ->
        Location.raise_errorf ~loc:body_loc
          "Multi-line expectations must start with an empty line"
    in
    if not is_exact then first_line 0;
    res

let pattern () =
  Ast_pattern.(
    map (single_expr_payload (pexp_loc __ (pexp_constant (pconst_string __ __))))
      ~f:(fun f loc s tag -> f (Some (loc, s, tag)))
    |||
    map (pstr nil)
      ~f:(fun f -> f None)
  )
