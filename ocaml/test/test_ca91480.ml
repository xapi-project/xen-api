(* Fixture: Create DB with VM. VM has records for Blobs, Appliances,
   VBDs, VIFs, VGPUs, PCIs, VM_metrics, and VM_guest_metrics, but none
   of these objects should actually exist in the DB.  *)

open OUnit
open Test_common

let setup_fixture () =
  let __context = make_test_database () in
  let self = make_vm ~__context () in

  let fake_v f = f ~__context ~self ~value:(Ref.make ())
  and fake_m f = f ~__context ~self ~key:"fake" ~value:(Ref.make ())
  and fake_l f = f ~__context ~self ~value:[(Ref.make ())] in

  fake_m Db.VM.add_to_blobs ;
  fake_v Db.VM.set_appliance ;
  fake_l Db.VM.set_attached_PCIs ;
  fake_v Db.VM.set_metrics ;
  fake_v Db.VM.set_guest_metrics ;

  __context, self

let test_vm_destroy () =
  let __context, self = setup_fixture () in
  Xapi_vm_helpers.destroy ~__context ~self

let test =
  "test_ca91480" >:::
  [
    "test_vm_destroy" >:: test_vm_destroy;
  ]
