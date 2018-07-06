let pretty_format ?std (x : json) =
  Pretty.format ?std (x :> json_max)

let pretty_print ?std out (x : json) =
  Easy_format.Pretty.to_formatter out (pretty_format ?std x)

let pretty_to_string ?std (x : json) =
  Pretty.to_string ?std (x :> json_max)

let pretty_to_channel ?std oc (x : json) =
  Pretty.to_channel ?std oc (x :> json_max)
