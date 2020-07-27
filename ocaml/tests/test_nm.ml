open Test_highlevel
open Test_common

module MaybeUpdateMasterPifMac = Generic.MakeStateful (struct
  module Io = struct
    type input_t = bool * string option

    (* auto_update_mac, new_primary_slave_mac *)

    type output_t = string * string

    (* record bond master mac, db bond master mac *)

    let show_primary_slave_mac_change = function
      | None ->
          "primary_slave mac unchanged"
      | Some new_primary_slave_mac ->
          Printf.sprintf "primary_slave mac -> %s" new_primary_slave_mac

    let string_of_input_t =
      Test_printers.pair Test_printers.bool show_primary_slave_mac_change

    let string_of_output_t (rc, db) =
      Printf.sprintf "Bond master macs. record: %s db : %s" rc db
  end

  module State = Test_state.XapiDb

  let original_primary_slave_mac = "aa:11:11:11:11:11"

  let original_master_mac = "cc:11:11:11:11:11"

  let a_new_mac = "12:34:56:78:90:ab"

  let load_input (__context : State.state_t)
      (auto_update_mac, o_new_primary_slave_mac) =
    let network = Test_common.make_network ~__context () in
    let host = Test_common.make_host ~__context () in
    let bond_master =
      Test_common.make_pif ~__context ~network ~host () ~mAC:original_master_mac
    in
    let primary_slave =
      Test_common.make_pif ~__context ~network ~host ~bond_slave_of:bond_master
        () ~mAC:"aa:11:11:11:11:11"
    in
    let _ =
      Test_common.make_pif ~__context ~network ~host ~bond_slave_of:bond_master
        () ~mAC:"bb:11:11:11:11:11"
      (* lowly slave *)
    in
    let bond =
      Test_common.make_bond ~__context ~master:bond_master ~primary_slave ()
    in
    Db.Bond.set_auto_update_mac ~__context ~self:bond ~value:auto_update_mac ;
    match o_new_primary_slave_mac with
    | None ->
        ()
    | Some new_primary_slave_mac ->
        Db.PIF.set_MAC ~__context ~self:primary_slave
          ~value:new_primary_slave_mac

  let extract_output (__context : State.state_t) _ =
    let bond = List.hd (Db.Bond.get_all ~__context) in
    let bond_record = Db.Bond.get_record ~__context ~self:bond in
    let bond_master = bond_record.API.bond_master in
    let bond_master_record = Db.PIF.get_record ~__context ~self:bond_master in
    let bond_master_record =
      Nm.maybe_update_master_pif_mac ~__context bond_record bond_master_record
        bond_master
    in
    (* these mac addresses should be equal; we test them both *)
    (bond_master_record.API.pIF_MAC, Db.PIF.get_MAC ~__context ~self:bond_master)

  let tests =
    `Documented
      [
        ( "primary_slave_mac_does_not_change_and_auto_update_mac_disabled"
        , `Quick
        , (false, None)
        , (original_master_mac, original_master_mac) )
      ; ( "primary_slave_mac_does_not_change_and_auto_update_mac_enabled"
        , `Quick
        , (true, None)
        , (original_primary_slave_mac, original_primary_slave_mac) )
      ; ( "primary_slave_mac_does_change_and_auto_update_mac_disabled"
        , `Quick
        , (false, Some a_new_mac)
        , (original_master_mac, original_master_mac) )
      ; ( "primary_slave_mac_does_change_and_auto_update_mac_enabled"
        , `Quick
        , (true, Some a_new_mac)
        , (a_new_mac, a_new_mac) )
      ]
end)

let tests =
  [("test_nm_maybe_update_master_pif_mac", MaybeUpdateMasterPifMac.tests)]
