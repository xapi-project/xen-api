open Expect_test_common.Std
open Base
open Ppxlib
open Ast_builder.Default

let lifter ~loc = object
  inherit [expression] Lifter.lift
  inherit Ppxlib_metaquot_lifters.expression_lifters loc

  method filename file_name =
    eapply ~loc (evar ~loc "Expect_test_common.Std.File.Name.of_string")
      [ estring ~loc (File.Name.to_string file_name) ]
end

let lift_location ~loc of_loc =
  (lifter ~loc)#location of_loc
;;

let lift_expectation ~loc expect =
  (lifter ~loc)#raw expect
;;

let estring_option ~loc x =
  match x with
  | None -> pexp_construct ~loc (Located.mk ~loc (lident "None")) None
  | Some s ->
    pexp_construct ~loc (Located.mk ~loc (lident "Some")) (Some (estring ~loc s))
;;

(* Grab a list of all the output expressions *)
let collect_expectations = object
  inherit [(Location.t * Expectation.Raw.t) list] Ast_traverse.fold as super

  method! expression expr acc =
    match Expect_extension.match_expectation expr with
    | None ->
      super#expression expr acc
    | Some ext ->
      assert_no_attributes expr.pexp_attributes;
      (expr.pexp_loc, ext) :: acc
end

let replace_expects = object
  inherit [string] Ast_traverse.map_with_context as super

  method! expression instance_var ({ pexp_attributes; pexp_loc; _ } as expr) =
    match Expect_extension.match_expectation expr with
    | None -> super#expression instance_var expr
    | Some ext ->
      let loc = { pexp_loc with loc_end = pexp_loc.loc_start } in
      let expr =
        [%expr
          Expect_test_collector.Instance.save_output
            [%e evar ~loc instance_var]
            [%e lift_location ~loc ext.extid_location]
        ]
      in
      { expr with pexp_attributes }
end

let file_digest =
  let cache = Hashtbl.create (module String) ~size:32 in
  fun fname ->
    Hashtbl.find_or_add cache fname ~default:(fun () ->
      Caml.Digest.file fname |> Caml.Digest.to_hex)

let rewrite_test_body ~descr ~tags pstr_loc body =
  let loc = pstr_loc in
  let expectations =
    List.map (collect_expectations#expression body [])
      ~f:(fun (loc, expect_extension) -> lift_expectation ~loc expect_extension)
    |> elist ~loc
  in

  let instance_var = gen_symbol ~prefix:"_ppx_expect_instance" () in
  let body = replace_expects#expression instance_var body in

  let absolute_filename =
    Ppx_here_expander.expand_filename pstr_loc.loc_start.pos_fname
  in

  let hash = file_digest loc.loc_start.pos_fname in
  [%expr
    let module Expect_test_collector = Expect_test_collector.Make(Expect_test_config) in
    Expect_test_collector.run
      ~file_digest:        (Expect_test_common.Std.File.Digest.of_string [%e estring ~loc hash])
      ~location:           [%e lift_location ~loc (Ppx_expect_payload.transl_loc pstr_loc)]
      ~absolute_filename:  [%e estring ~loc absolute_filename]
      ~description:        [%e estring_option ~loc descr]
      ~tags:               [%e elist ~loc (List.map tags ~f:(estring ~loc))]
      ~expectations:       [%e expectations]
      ~inline_test_config: (module Inline_test_config)
      (fun [%p pvar ~loc instance_var] -> [%e body])
  ]

(* Set to [true] when we see a [%expect_test] extension *)
module Has_tests =
  Driver.Create_file_property
    (struct let name = "ppx_expect.has_tests" end)
    (Bool)

let expect_test =
  Extension.declare_inline "expect_test" Structure_item
    Ast_pattern.(Ppx_inline_test.opt_name_and_expr __)
    (fun ~loc ~path:_ ~name ~tags code ->
       Has_tests.set true;
       Ppx_inline_test.validate_extension_point_exn
         ~name_of_ppx_rewriter:"ppx_expect" ~loc ~tags;
       rewrite_test_body ~descr:name ~tags loc code
       |> Ppx_inline_test.maybe_drop loc)
;;

let () =
  Driver.register_transformation "expect_test"
    ~rules:[ Context_free.Rule.extension expect_test ]
    ~enclose_impl:(fun whole_loc ->
      match whole_loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> ([], [])
      | Some loc, Some _ ->
        let maybe_drop = Ppx_inline_test.maybe_drop in
        let absolute_filename =
          Ppx_here_expander.expand_filename loc.loc_start.pos_fname
        in
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop loc [%expr Expect_test_collector.Current_file.set
                                  ~absolute_filename:[%e estring ~loc absolute_filename]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc [%expr Expect_test_collector.Current_file.unset ()]
        in
        (header, footer)
    )
;;
