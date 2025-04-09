module Message = Client.Client.Message

let rpc = Quicktest_args.rpc

module Testable = struct
  let ref () =
    let fmt = Fmt.of_to_string Ref.string_of in
    Alcotest.testable fmt ( = )
end

let get_all_records_where_test rpc session_id () =
  let with_api fn = fn ~rpc ~session_id in
  let create_message = with_api Message.create in

  let vm = List.hd (with_api Client.Client.VM.get_all) in
  let vm_uuid = with_api Client.Client.VM.get_uuid ~self:vm in

  let host = List.hd (with_api Client.Client.Host.get_all) in
  let host_uuid = with_api Client.Client.Host.get_uuid ~self:host in

  (* Create several different messages *)
  let messages =
    [
      create_message ~name:"VM_EXPLODED" ~priority:100L ~cls:`VM
        ~obj_uuid:vm_uuid ~body:"body"
    ; create_message ~name:"HOST_EXPLODED" ~priority:100L ~cls:`Host
        ~obj_uuid:host_uuid ~body:"body"
    ; create_message ~name:"VM_DROWNED" ~priority:50L ~cls:`VM ~obj_uuid:vm_uuid
        ~body:"body"
    ]
  in

  let get_messages_where = with_api Message.get_all_records_where in

  let queries =
    [
      (* query | is message expected in the list [bool ; bool ; bool] *)
      ({| field "name" = "VM_EXPLODED" |}, [true; false; false])
    ; ({| field "priority" = "100" |}, [true; true; false])
    ; ({| field "cls" = "VM" |}, [true; false; true])
    ; ({| field "cls" = "VM" and field "priority" = "50"|}, [false; false; true])
    ; ({| field "cls" = "VDI"|}, [false; false; false])
    ; ({| field "class" = "VM"|}, [false; false; false])
    ; ( {| field "cls" = "VM" or field "name" = "HOST_EXPLODED"|}
      , [true; true; true]
      )
    ]
  in
  let lists =
    List.map
      (fun (expr, expected) -> (expr, get_messages_where ~expr, expected))
      queries
  in

  (* Check that filtering returns correct messages *)
  let message_test msg list expected =
    let list = List.map fst list in
    List.iter2
      (fun msg_ref expected ->
        Alcotest.check' Alcotest.bool ~msg ~expected
          ~actual:(List.mem msg_ref list)
      )
      messages expected
  in
  List.iter (fun (msg, list, expected) -> message_test msg list expected) lists

let tests () =
  let open Qt_filter in
  [
    [("Message.get_all_records_where test", `Quick, get_all_records_where_test)]
    |> conn
  ]
  |> List.concat
