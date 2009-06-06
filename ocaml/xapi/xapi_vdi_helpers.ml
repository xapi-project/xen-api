
(* We only support .iso files (from an iso SR) and block devices from
   a local magic SR (eg /dev/hda) but NOT phantom_vbd block attach isos *)
let assert_vdi_is_valid_iso ~__context ~vdi = 
	let sr = Db.VDI.get_SR ~__context ~self:vdi in
	let ct = Db.SR.get_content_type ~__context ~self:sr in
	if ct <> "iso"
	then raise (Api_errors.Server_error(Api_errors.vdi_is_not_iso, [ Ref.string_of vdi; ct ]))

(* CA-26514: Block operations on 'unmanaged' VDIs *)
let assert_managed ~__context ~vdi = 
  if not (Db.VDI.get_managed ~__context ~self:vdi)
  then raise (Api_errors.Server_error(Api_errors.vdi_not_managed, [ Ref.string_of vdi ]))

