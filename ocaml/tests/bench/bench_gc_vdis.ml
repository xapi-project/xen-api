open Bechamel

let num_srs = 100

let num_vdis = 5000

let () =
  Suite_init.harness_init () ;
  Debug.set_level Syslog.Info

let allocate () =
  let __context = Test_common.make_test_database () in
  let (_ : API.ref_SM) = Test_common.make_sm ~__context () in
  let srs = Array.init num_srs (fun _ -> Test_common.make_sr ~__context ()) in
  let _vdis =
    Array.init num_vdis (fun i ->
        let sr = srs.(i mod num_srs) in
        Test_common.make_vdi ~__context ~sR:sr ()
    )
  in
  __context

let gc_vdis_list_mem __context =
  let all_srs = Db.SR.get_all ~__context in
  List.iter
    (fun vdi ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      let (_ : bool) = Sys.opaque_identity (List.mem sr all_srs) in
      ()
    )
    (Db.VDI.get_all ~__context)

let gc_vdis_valid_ref __context =
  List.iter
    (fun vdi ->
      let sr = Db.VDI.get_SR ~__context ~self:vdi in
      let (_ : bool) = Sys.opaque_identity (Db.is_valid_ref __context sr) in
      ()
    )
    (Db.VDI.get_all ~__context)

let benchmarks =
  [
    Test.make_with_resource ~name:"gc_VDIs: List.mem (before)" ~allocate
      ~free:ignore Test.uniq
      (Staged.stage gc_vdis_list_mem)
  ; Test.make_with_resource ~name:"gc_VDIs: valid_ref (after)" ~allocate
      ~free:ignore Test.uniq
      (Staged.stage gc_vdis_valid_ref)
  ]

let () = Bechamel_simple_cli.cli benchmarks
