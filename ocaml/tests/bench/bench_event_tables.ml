open Bechamel

let event_tables_inline () =
  let objs =
    List.filter
      (fun x -> x.Datamodel_types.gen_events)
      (Dm_api.objects_of_api Datamodel.all_api)
  in
  let (_ : string list) =
    Sys.opaque_identity (List.map (fun x -> x.Datamodel_types.name) objs)
  in
  ()

let all_event_tables =
  Dm_api.objects_of_api Datamodel.all_api
  |> List.filter (fun x -> x.Datamodel_types.gen_events)
  |> List.map (fun x -> x.Datamodel_types.name)

let event_tables_hoisted () =
  let (_ : string list) = Sys.opaque_identity all_event_tables in
  ()

let benchmarks =
  [
    Test.make ~name:"Event.from tables: inline (before)"
      (Staged.stage event_tables_inline)
  ; Test.make ~name:"Event.from tables: hoisted (after)"
      (Staged.stage event_tables_hoisted)
  ]

let () = Bechamel_simple_cli.cli benchmarks
