module DT = Datamodel_types

let calls_with_datetime_params =
  let get_messages DT.{name; messages; _} =
    List.to_seq messages
    |> Seq.map (fun msg ->
           DT.{msg with msg_name= Printf.sprintf "%s.%s" name msg.msg_name}
       )
  in
  let with_datetimes DT.{msg_name; msg_params; _} =
    let cursed_params =
      List.filter_map
        (fun param ->
          if
            param.DT.param_type = DT.DateTime
            && not (Astring.String.is_infix ~affix:"UTC" param.param_doc)
          then
            Some (msg_name, param.param_name, param.param_doc)
          else
            None
        )
        msg_params
    in
    if cursed_params <> [] then Some (List.to_seq cursed_params) else None
  in

  Datamodel.all_system
  |> List.to_seq
  |> Seq.concat_map get_messages
  |> Seq.filter_map with_datetimes
  |> Seq.concat

let () =
  if not (Seq.is_empty calls_with_datetime_params) then (
    Printf.printf
      "\x1b[31;1mERROR\x1b[0m: Found datetime parameters in calls without \
       proper documentation. It must mention that datetimes are assumed to \
       have UTC when they do not contain a timezone. Parameters found:\n" ;
    calls_with_datetime_params
    |> Seq.iter (fun (call_name, param_name, param_doc) ->
           Printf.printf "%s (%s): %s\n" call_name param_name param_doc
       ) ;
    exit 1
  ) else
    Printf.printf
      "\x1b[32;1mOK\x1b[0m: All datetime parameters in calls have proper \
       documentation."
