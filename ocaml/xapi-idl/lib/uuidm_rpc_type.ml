module Uuidm = struct
  include Uuidm

  (** Validate UUIDs by converting them to Uuidm.t in the API *)
  let typ_of =
    Rpc.Types.Abstract
      {
        aname= "uuid"
      ; test_data= [Uuidm.v4_gen (Random.get_state ()) ()]
      ; rpc_of= (fun t -> Rpc.String (Uuidm.to_string t))
      ; of_rpc=
          (function
          | Rpc.String s -> (
            match Uuidm.of_string s with
            | Some uuid ->
                Ok uuid
            | None ->
                Error
                  (`Msg
                    (Printf.sprintf "typ_of_vm_uuid: not a valid UUID: %s" s)
                    )
          )
          | r ->
              Error
                (`Msg
                  (Printf.sprintf
                     "typ_of_vm_uuid: expected rpc string but got %s"
                     (Rpc.to_string r)
                  )
                  )
          )
      }
end
