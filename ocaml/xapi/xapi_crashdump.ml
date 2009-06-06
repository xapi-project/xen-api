exception Not_implemented

let nothrow f () = try f() with _ -> ()

let create ~__context ~vM ~vDI =
  let cdumpref = Ref.make() in
  let uuid = Uuid.to_string (Uuid.make_uuid()) in
    Db.Crashdump.create ~__context ~ref:cdumpref ~uuid ~vM ~vDI ~other_config:[];
    cdumpref

let destroy ~__context ~self =
  Pervasiveext.finally
    (nothrow (fun ()->
		let vdi = Db.Crashdump.get_VDI ~__context ~self in
		  Helpers.call_api_functions ~__context
		    (fun rpc session_id ->
		       Client.Client.VDI.destroy rpc session_id vdi)))
    (fun ()->
       Db.Crashdump.destroy ~__context ~self)
