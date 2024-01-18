module Ipaddr = struct
  include Ipaddr

  let typ_of =
    Rpc.Types.Abstract
      {
        aname= "ipaddr"
      ; test_data=
          [Ipaddr.V4 Ipaddr.V4.localhost; Ipaddr.V6 Ipaddr.V6.localhost]
      ; rpc_of= (fun t -> Rpc.String (Ipaddr.to_string t))
      ; of_rpc=
          (function
          | Rpc.String s ->
              Ipaddr.of_string s
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
