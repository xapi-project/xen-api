let default_on_missing_ref f default x =
  try f x with
  | Db_exn.DBCache_NotFound ("missing reference", _, _) ->
      default
  | Db_exn.DBCache_NotFound ("missing row", _, _) ->
      default
  (* When using the Client module, we get this exception for invalid references: *)
  | Api_errors.(Server_error (handle_invalid, _))
    when handle_invalid = Api_errors.handle_invalid ->
      default

let exists f = List.exists (default_on_missing_ref f false)

let filter f = List.filter (default_on_missing_ref f false)

let for_all f l = not (exists (fun x -> not @@ f x) l)

let map f = List.filter_map (default_on_missing_ref (fun x -> Some (f x)) None)

let iter f = List.iter (default_on_missing_ref f ())

let flat_map f l = List.map (default_on_missing_ref f []) l |> List.flatten

let filter_map f l = List.filter_map Fun.id (map f l)
