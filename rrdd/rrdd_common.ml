let loadavg () =
  let split_colon line =
      Astring.String.fields ~empty:false line
  in
  let all = Xapi_stdext_unix.Unixext.string_of_file "/proc/loadavg" in
  try
    float_of_string (List.hd (split_colon all))
  with _ -> -1.
