let new_uuid () =
  let random = Random.State.make_self_init () in
  Uuidm.v4_gen random ()

module Uuidm = struct
  include Uuidm

  (** Validate UUIDs by converting them to Uuidm.t in the API *)
  let typ_of =
    Rpc.Types.Abstract
      {
        aname= "uuid"
      ; test_data= [new_uuid ()]
      ; rpc_of= (fun t -> Rpc.String (Uuidm.to_string t))
      ; of_rpc=
          (function
          | Rpc.String s -> (
            match Uuidm.of_string s with
            | Some uuid ->
                Ok uuid
            | None ->
                Error
                  (`Msg (Printf.sprintf "typ_of_vm_uuid: not a valid UUID: %s" s)
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

  let t_of_sexp sexp =
    match sexp |> Sexplib.Std.string_of_sexp |> Uuidm.of_string with
    | None ->
        Sexplib.Conv.of_sexp_error "not a UUID" sexp
    | Some u ->
        u

  let sexp_of_t t = t |> Uuidm.to_string |> Sexplib.Std.sexp_of_string
end
