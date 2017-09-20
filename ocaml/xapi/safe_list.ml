
let default_on_missing_ref f default x =
  try
    f x
  with
  | Db_exn.DBCache_NotFound ("missing reference", _, _) -> default
  | Db_exn.DBCache_NotFound ("missing row", _, _) -> default

let map f = Stdext.Listext.List.filter_map (default_on_missing_ref (fun x -> Some (f x)) None)

let filter f = List.filter (default_on_missing_ref f false)

let flat_map f l = List.map (default_on_missing_ref f []) l |> List.flatten
