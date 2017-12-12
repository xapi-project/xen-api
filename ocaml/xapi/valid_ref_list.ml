
let default_on_missing_ref f default x =
  try
    f x
  with
  | Db_exn.DBCache_NotFound ("missing reference", _, _) -> default
  | Db_exn.DBCache_NotFound ("missing row", _, _) -> default

let exists f = List.exists (default_on_missing_ref f false)

let filter f = List.filter (default_on_missing_ref f false)

let for_all f l = not (exists (fun x -> not @@ f x) l)

let map f = Stdext.Listext.List.filter_map (default_on_missing_ref (fun x -> Some (f x)) None)

let iter f = List.iter (default_on_missing_ref f ())

let flat_map f l = List.map (default_on_missing_ref f []) l |> List.flatten

let filter_map f l = map f l |> List.filter ((<>) None) |> List.map Xapi_stdext_monadic.Opt.unbox
