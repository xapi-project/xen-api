let hash x = Digest.string x |> Digest.to_hex

(* BEWARE: if this changes, check that schema has been bumped accordingly *)
let last_known_schema_hash = "f1721720c4d31990853c8285f21844e7"

let current_schema_hash : string =
  let open Datamodel_types in
  let hash_of_obj x =
    List.map rpc_of_content x.contents
    |> List.map Jsonrpc.to_string
    |> String.concat ""
    |> hash
  in
  Datamodel.all_system |> List.map hash_of_obj |> String.concat ":" |> hash

let () =
  if last_known_schema_hash <> current_schema_hash then (
    Printf.eprintf
      {|

New schema hash ('%s') doesn't match the last known one. Please bump the
datamodel schema versions if necessary, and update 'last_known_schema_hash'.

|}
      current_schema_hash ;
    exit 1
  )
