open! Core_kernel

let%test_module _ =
  (module struct
    type t = { x : bool } [@@deriving fields]

    let%test_unit "typical use of Validate.field_direct_folder doesn't allocate on success" =
      let validate_x = unstage (Validate.field_direct_folder Validate.pass_bool) in
      let validate t =
        Fields.Direct.fold t ~init:[] ~x:validate_x
        |> Validate.of_list
        |> Validate.result
      in
      let t = { x = true } in
      let initial_words = Gc.minor_words () in
      let _ : unit Or_error.t = validate t in
      let allocated = Gc.minor_words () - initial_words in
      [%test_result: int] allocated ~expect:0
    ;;

    let%test_unit "Validate.all doesn't allocate on success" =
      let checks = List.init 5 ~f:(const Validate.pass_bool) in
      let initial_words = Gc.minor_words () in
      let _ : Validate.t = Validate.all checks true in
      let allocated = Gc.minor_words () - initial_words in
      [%test_result: int] allocated ~expect:0
    ;;

    let%test_unit "Validate.combine doesn't allocate on success" =
      let initial_words = Gc.minor_words () in
      let _ : Validate.t = Validate.combine Validate.pass Validate.pass in
      let allocated = Gc.minor_words () - initial_words in
      [%test_result: int] allocated ~expect:0
    ;;
  end)
