open Bechamel

module D = Debug.Make (struct let name = __MODULE__ end)

(* tested configuration limits *)
let max_hosts = 64

let max_vms = (*2400*) 240

let max_vbds = (* 255 *) 25

let () =
  (* a minimal harness init *)
  Suite_init.harness_init () ;
  (* don't spam the logs in [allocate] *)
  Debug.set_level Syslog.Info

let allocate () =
  let open Test_common in
  let __context = make_test_database () in
  let (_sm_ref : API.ref_SM) = make_sm ~__context () in
  let sr_ref = make_sr ~__context () in
  let (_ : API.ref_PBD array) =
    Array.init max_hosts (fun _ -> make_pbd ~__context ~sR:sr_ref ())
  in
  let vms =
    Array.init max_vms @@ fun _ ->
    let vm_ref = make_vm ~__context () in
    Array.init (max_vbds / 2) @@ fun _ ->
    let vdi_ref = make_vdi ~__context ~sR:sr_ref () in
    let vbd_ref =
      make_vbd ~__context ~vDI:vdi_ref ~vM:vm_ref ~currently_attached:true
        ~mode:`RO ()
    in
    let vdi_ref' = make_vdi ~__context ~sR:sr_ref () in
    let vbd_ref' =
      make_vbd ~__context ~vDI:vdi_ref' ~vM:vm_ref ~currently_attached:true
        ~mode:`RW ()
    in
    (vdi_ref, vbd_ref, vdi_ref', vbd_ref')
  in
  D.info "Created test database" ;
  (__context, vms)

let test_vdi_update_allowed_operations (__context, vm_disks) =
  let _, _, vdi_ref, vbd_ref = vm_disks.(0).(0) in
  Db.VBD.set_currently_attached ~__context ~self:vbd_ref ~value:true ;
  Xapi_vdi.update_allowed_operations ~__context ~self:vdi_ref ;
  Db.VBD.set_currently_attached ~__context ~self:vbd_ref ~value:false ;
  Xapi_vdi.update_allowed_operations ~__context ~self:vdi_ref

let benchmarks =
  Test.make_grouped ~name:"update_allowed_operations"
    [
      Test.make_with_resource ~name:"VDI" ~allocate ~free:ignore Test.uniq
        (Staged.stage test_vdi_update_allowed_operations)
    ]

let () = Bechamel_simple_cli.cli benchmarks
