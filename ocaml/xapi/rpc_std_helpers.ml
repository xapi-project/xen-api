let rpc_of_hashtbl ~rpc_of t =
  let dict = Hashtbl.fold (fun k v acc -> (k, rpc_of v) :: acc) t [] in
  Rpc.Dict dict

let hashtbl_of_rpc ~of_rpc = function
  | Rpc.Dict d ->
    let h = Hashtbl.create (List.length d) in
    List.iter (function (k, r) -> Hashtbl.add h k (of_rpc r)) d;
    h
  | r -> failwith (Printf.sprintf "Expected Rpc.Dict, but got %s" (Xmlrpc.to_string r))

