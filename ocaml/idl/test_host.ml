module DT = Datamodel_types
module FieldSet = Astring.String.Set

let recent_field (f : DT.field) = f.lifecycle.transitions = []

let rec field_full_names = function
  | DT.Field f ->
      if recent_field f then
        f.full_name |> String.concat "_" |> Seq.return
      else
        Seq.empty
  | DT.Namespace (_, xs) ->
      xs |> List.to_seq |> Seq.concat_map field_full_names

let () =
  let create_params =
    Datamodel_host.create_params
    |> List.map (fun p -> p.DT.param_name)
    |> FieldSet.of_list
  and fields =
    Datamodel_host.t.contents
    |> List.to_seq
    |> Seq.concat_map field_full_names
    |> FieldSet.of_seq
  in
  let missing_in_create_params = FieldSet.diff fields create_params in
  if not (FieldSet.is_empty missing_in_create_params) then (
    Format.eprintf "Missing fields in create_params: %a@." FieldSet.dump
      missing_in_create_params ;
    exit 1
  )
