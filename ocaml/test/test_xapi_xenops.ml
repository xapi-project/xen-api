open OUnit
open Test_common
open Test_vgpu_common

let test_enabled_in_xenguest () =
  let should_raise = ["foo";"";"banana";"2"] in
  let should_be_true = ["TRUE";"tRuE";"1";"true"] in
  let should_be_false = ["FALSE";"fAlSe";"0";"false"] in

  let err k =
    failwith (Printf.sprintf "Failed to parse '%s' correctly" k)
  in

  let k = "test_key" in
  let p v = [k,v] in
  let val_fn p = Vm_platform.is_true ~key:k ~platformdata:p ~default:false in
  let valid_fn p = Vm_platform.is_valid ~key:k ~platformdata:p in

  (* Empty list should be valid *)
  if not (valid_fn []) then err "[]";

  List.iter (fun x -> if valid_fn (p x) then err x) should_raise;

  List.iter (fun x ->
      let e = val_fn (p x) in
      if not e then err x) should_be_true;

  List.iter (fun x ->
      let e = val_fn (p x) in
      if e then err x) should_be_false


let test_nested_virt_licensing () =
  let __context = make_test_database () in
  (* Nested_virt is restricted in the default test database *)

  (* List of plaform keys and whether they should be restricted when 'Nested_virt' is restricted.
     true -> definitely should be restricted
     false -> definitely should be unrestricted
  *)

  let nested_virt_checks =
    [[],                        false;
     ["foo","bar";"baz","moo";
      "nested-virt","true"],    true;
     ["nested-virt","TRUE"],    true;
     ["nested-virt","false"],   false;
     ["nested-virt","1"],       true;
     ["nested-virt","0"],       false;
     ["nested-virt","true"],    true;
    ] in

  let string_of_platform p =
    Printf.sprintf "[%s]"
      (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "'%s','%s'" k v) p))
  in

  let pool = Db.Pool.get_all ~__context |> List.hd in

  let check_one (platform,should_raise) =
    begin
      try
        Db.Pool.set_restrictions ~__context ~self:pool ~value:["restrict_nested_virt","true"];
        Vm_platform.check_restricted_flags ~__context platform;
        if should_raise
        then
          failwith
            (Printf.sprintf "Failed to raise an exception for platform map: '[%s]'"
               (string_of_platform platform));
      with Api_errors.Server_error(e,l) when e=Api_errors.license_restriction ->
        if not should_raise
        then
          failwith
            (Printf.sprintf "Raise an exception unexpectedly for platform map: '[%s]'"
               (string_of_platform platform));
    end;

    (* If the feature is unrestricted, nothing should raise an exception *)
    Db.Pool.set_restrictions ~__context ~self:pool ~value:["restrict_nested_virt","false"];
    Vm_platform.check_restricted_flags ~__context platform
  in

  List.iter check_one nested_virt_checks



let test =
  "test_vm_helpers" >:::
  [
    "test_nested_virt_licensing" >:: test_nested_virt_licensing;
    "test_enabled_in_xenguest" >:: test_enabled_in_xenguest;
  ]
