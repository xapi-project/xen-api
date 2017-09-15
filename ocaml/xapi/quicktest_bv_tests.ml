open Quicktest_common

let start session_id rpc =

  (* helper functions for API/PBD calls *)
  let with_api fn = fn ~rpc ~session_id in
  let with_obj fn obj = with_api fn ~self:obj in

  (* with_obj generalises to most classes, however, for some classes
   * (SR,Host,VDI,etc), a mix of label ~self and ~sr,~vdi etc is used *)

let get_set_config (class_name, obj, gt, st, permission) =
    let conf_t = make_test ("Testing " ^ class_name ^ " 'get,set_other_config'") 4 in
    try
      (* set config to arbitrary (key,value) *)
      start conf_t;
      let dummy_value = [("dummy_key","dummy_val")] in
      with_obj st obj ~value:dummy_value; (* set config value *)

      (* read-writeable configs succeed if the value is set, read-only fails *)
      if dummy_value = (with_obj gt obj)
      then match permission with
        | `RW -> success conf_t
        | `RO -> failed conf_t "Error: config read-only, should throw PERMISSION_DENIED"
      else failed conf_t "set_config failed";

      (* read-only config returns success test if and only if it throws PERMISSION_DENIED *)
    with
    | (Failure hd) -> failed conf_t (Printf.sprintf "Error: could not find %s object" class_name)
    | Api_errors.Server_error (x,_) when x = Api_errors.permission_denied && permission =`RO -> success conf_t;
    | e -> failed conf_t (ExnHelper.string_of_exn e); in


  (* check add_to and remove_from_other_config *)
  let add_rm_other_config (class_name, obj, gt, add, rm)  =
    let adrm_t = make_test ("Testing " ^ class_name ^ " 'add_to/remove_from_other_config'") 4 in
    try
      start adrm_t;

      (* add two (key,value) pairs to other_config *)
      let key,value, key_2, value_2 = "key_to_delete","value_to_delete", "key_to_keep", "value_to_keep" in
      List.iter (fun (key,value) -> with_obj add obj ~key ~value) [(key,value); (key_2, value_2)];

      (* check both pairs have been added, remove one *)
      if List.fold_left (&&) true (List.map
       (fun (key,value) -> List.mem (key,value) (with_obj gt obj))
       [ (key,value); (key_2, value_2) ])
      then with_obj rm obj ~key
      else failed adrm_t "Error: add_to_other_config failed";

      (* test fails if (key,value) has not been removed or if (key_2,value_2) has *)
      if (List.mem (key_2,value_2) (with_obj gt obj)) && not (List.mem (key,value) (with_obj gt obj))
      then success adrm_t
      else failed adrm_t "Error: remove_from_other_config failed";

    with
    | (Failure hd) -> failed adrm_t (Printf.sprintf "Error: could not find %s object" class_name)
    | e -> failed adrm_t (ExnHelper.string_of_exn e); in


  (* main function to run test suite *)
  let run_tests () =
    let test_str_indent = 2 in
    print_newline ();
    let test = make_test "Begin basic verification test\n" test_str_indent in
    try
      start test;
      (* for each class, must pass an object to the test *)
      let obj class_name gt_all =
        try
          List.hd (with_api gt_all)
        with _ -> failwith ("failed to acquire " ^ class_name ^ " object") in

      begin
        (* iterate test for other_config getters and setters over different classes *)
        debug test "Testing get and set_other_config";
        List.iter (fun (class_name, gt_all, gt, st, permission) ->
          get_set_config (class_name, obj class_name gt_all, gt, st, permission))
          Quicktest_bv_calls.get_set_config_calls_lst;
        debug test "Finished testing get and set_other_config\n";

        (* iterate test for other_config modifiers over different classes *)
        debug test "Testing add_to and remove_from_other_config";
        List.iter (fun (class_name, gt_all, gt, add, rm) ->
          add_rm_other_config (class_name, obj class_name gt_all, gt, add, rm))
          Quicktest_bv_calls.add_rm_config_calls_lst;
        debug test "Finished testing add_to and remove_from_other_config\n";
      end;

      print_endline ((String.make test_str_indent ' ') ^ "Finished basic verification test\n");
      success test;

    with
    | e -> failed test (ExnHelper.string_of_exn e);
  in run_tests () ;
