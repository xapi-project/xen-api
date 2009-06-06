let get_backtrace () =
  let b = Printexc.get_backtrace () in
  let nicify_locator s =
    try
      match Stringext.String.split ',' s with
      | file :: line :: character :: [] ->
        let i = String.index_from file 0 '"' + 1 in
        let i2 = String.index_from file i '"' in
        String.concat "" [ String.sub file i (i2 - i); ":";
                           (try String.sub line 6 (String.length line - 6) with _ -> line); ".";
                           (try String.sub character 12 (String.length character - 12) with _ -> character) ]
      | _ -> s
    with _ -> s
    in
  try
    let list = Stringext.String.split '\n' b in
	let list = List.filter ((<>) "") list in
    "Raised at " ^ (String.concat " -> " (List.map nicify_locator list))
  with _ ->
    b
